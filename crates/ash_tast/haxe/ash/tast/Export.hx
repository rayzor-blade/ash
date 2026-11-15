package ash.tast;

#if macro
import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.macro.Type;
import haxe.macro.TypeTools;
import ash.tast.Format;

/**
	Writes the program the compiler just typed as a TAST file.

	Add `--macro ash.tast.Export.init("out.tast")` to a build; it runs once the
	program is typed and the expression filters have run, before dead-code removal
	and the type filters (`Context.onGenerate`), and writes the file. With
	`--no-output` the target's own generator does not run.

	This exporter works with a stock `haxe`. It cannot write what a back end reads
	after the type filters (the program the HashLink generator receives), the
	initialization order, `__init__` blocks, declaration metadata, defines or
	resources; those sections stay empty. The compiler plugin in `ocaml/ash_tast`
	writes them, and is the exporter the lowering reads.
	`-D ash_tast_positions` adds a source position to every node, and
	`-D ash_tast_verbose` prints what was written and what could not be.

	A file can sit on layers: files built once for the standard library or a
	library, which the program then names by tagged id instead of exporting again.

	- `init("program.tast", ["std.tast", "lib.tast"])` builds a program on those
	  layers, bottom first. A class a layer holds is not exported again; the file
	  lists what dead-code removal kept from it. A layer whose sources or defines
	  have changed since it was built, or that does not hold the classes the program
	  names, is ignored with a warning and the program is exported whole.
	- `init("std.tast", [], ["<std>"])` builds a layer from the compiler's standard
	  library; the roots are the directories it covers. Build it with every class
	  typed, which is `-dce no` and `--macro include(...)` for what the layer is to hold.
**/
class Export {
	public static function init(path:String = "program.tast", ?parents:Array<String>, ?layerRoots:Array<String>):Void {
		Format.checkCompiler();
		Context.onGenerate(function(types) new Export(path, parents == null ? [] : parents, layerRoots).run(types));
	}

	// Strings: one blob, each string once.
	var blob = new BytesBuffer();
	var blobLen = 0;
	var strOffsets:Array<Int> = [];
	var strLens:Array<Int> = [];
	var strIds:Map<String, Int> = new Map();

	// Tables, each a flat Int array with a fixed stride per record.
	var types:Array<Int> = []; // kind, name, def, args_first, args_count
	var typeArgs:Array<Int> = [];
	var classes:Array<Int> = []; // name, super_type, flags, fields_first, fields_count, ifaces_first, ifaces_count
	var fields:Array<Int> = []; // name, ty, owner, flags, func, pos
	var abstracts:Array<Int> = []; // name, underlying, impl_class, flags
	var enums:Array<Int> = []; // name, ctors_first, ctors_count, flags
	var enumCtors:Array<Int> = []; // name, enum_id, index, ty, args_first, args_count
	var anonFields:Array<Int> = []; // name, ty, flags
	var locals:Array<Int> = []; // name, ty, flags
	var params:Array<Int> = []; // local, default_value
	var funcs:Array<Int> = []; // ret, params_first, params_count, root, flags, pos
	var positions:Array<Int> = []; // file, start, end
	var keyRecs:Array<Int> = []; // kind, key (string id), value
	var keptRecs:Array<Int> = []; // kind, id
	var parentRecs:Array<Int> = []; // id_lo, id_hi
	var layerId = [0, 0]; // lo, hi

	// Expression nodes, written as records straight into a buffer that doubles when full.
	var nodeBytes:Bytes = Bytes.alloc(1 << 20);
	var nodeCount = 0;
	// The queue: a node id is its index. Each entry is an expression or a pseudo node.
	var qExpr:Array<TypedExpr> = [];
	var qPseudo:Array<Pseudo> = [];
	// Interning. Types are found by the cheapest key their shape allows: a class or
	// abstract without arguments by its path, anything else by the compiler's own text
	// for the type, and only a miss pays for reading the structure.
	var instIds:Map<String, Int> = new Map();
	var abstractTypeIds:Map<String, Int> = new Map();
	var enumTypeIds:Map<String, Int> = new Map();
	var typedefIds:Map<String, Int> = new Map();
	var textIds:Map<String, Int> = new Map();
	var dynId = -1;
	/** Seconds spent in the compiler turning field bodies into macro values. */
	var bodyTime = 0.0;
	var typeIds:Map<String, Int> = new Map();
	var classIds:Map<String, Int> = new Map();
	var classTypes:Array<Ref<ClassType>> = [];
	var classFilled:Array<Bool> = [];
	var fieldIds:Map<String, Int> = new Map();
	var enumIds:Map<String, Int> = new Map();
	var enumCtorIds:Map<String, Int> = new Map();
	var abstractIds:Map<String, Int> = new Map();
	var localIds:Map<Int, Int> = new Map();
	var posIds:Map<String, Int> = new Map();
	var withPositions:Bool;
	var verbose:Bool;
	var typedefDepth = 0;
	var walking = false;

	// Layers below this file, bottom first.
	var parentLayers = 0;
	var parentRoots:Array<Array<String>> = [];
	var parentIdPairs:Array<Int> = []; // lo, hi per layer
	var superCache:Map<String, Null<Ref<ClassType>>> = new Map();
	static inline final TAG = 0x80000000;

	// What could not be written exactly.
	var dynamicFields = 0;
	var unsupported:Map<String, Int> = new Map();

	final path:String;
	final parentPaths:Array<String>;
	final layerRoots:Null<Array<String>>;

	function new(path:String, parentPaths:Array<String>, layerRoots:Null<Array<String>>) {
		this.path = path;
		this.parentPaths = parentPaths;
		this.layerRoots = layerRoots;
		withPositions = Context.defined("ash_tast_positions");
		verbose = Context.defined("ash_tast_verbose");
	}

	// ---- strings, positions

	function str(s:String):Int {
		var id = strIds.get(s);
		if (id != null) return id;
		id = strOffsets.length;
		var bytes = Bytes.ofString(s);
		strOffsets.push(blobLen);
		strLens.push(bytes.length);
		blob.add(bytes);
		blobLen += bytes.length;
		strIds.set(s, id);
		return id;
	}

	function posId(p:Position):Int {
		var info = Context.getPosInfos(p);
		var key = info.file + ":" + info.min + ":" + info.max;
		var id = posIds.get(key);
		if (id != null) return id;
		id = Std.int(positions.length / 3);
		positions.push(str(info.file));
		positions.push(info.min);
		positions.push(info.max);
		posIds.set(key, id);
		return id;
	}

	function unsupportedCase(what:String) {
		var n = unsupported.get(what);
		unsupported.set(what, n == null ? 1 : n + 1);
	}

	// ---- types

	function rec(table:Array<Int>, values:Array<Int>):Int {
		var stride = values.length;
		var id = Std.int(table.length / stride);
		for (v in values) table.push(v);
		return id;
	}

	function argList(ids:Array<Int>):Array<Int> {
		var first = typeArgs.length;
		for (id in ids) typeArgs.push(id);
		return [first, ids.length];
	}

	function typeId(t:Type):Int {
		switch (t) {
			case TInst(c, []):
				var key = c.toString();
				var id = instIds.get(key);
				if (id != null) return id;
				id = rec(types, [TypeKind.CLASS, Format.NONE, classId(c), 0, 0]);
				instIds.set(key, id);
				return id;
			case TAbstract(a, []):
				var key = a.toString();
				var id = abstractTypeIds.get(key);
				if (id != null) return id;
				id = rec(types, [TypeKind.ABSTRACT, Format.NONE, abstractId(a), 0, 0]);
				abstractTypeIds.set(key, id);
				return id;
			case TEnum(e, []):
				var key = e.toString();
				var id = enumTypeIds.get(key);
				if (id != null) return id;
				id = rec(types, [TypeKind.ENUM, Format.NONE, enumId(e), 0, 0]);
				enumTypeIds.set(key, id);
				return id;
			case TDynamic(_):
				return dynamicType();
			case TType(d, ps):
				return typedefId(t, d, ps);
			case TMono(r):
				// A monomorph nothing bound is Dynamic to every backend; the compilation
				// server leaves such types bound to Dynamic, so this keeps its output equal.
				var inner = r.get();
				return inner == null ? dynamicType() : typeId(inner);
			case TLazy(f):
				return typeId(f());
			case TInst(_, _) | TEnum(_, _) | TAbstract(_, _) | TFun(_, _) | TAnonymous(_):
				// Not followed: following strips `Null<T>`, and the program keeps that.
				return textType(t);
		}
	}

	/**
		A type found by the compiler's text for it; the structure is read only on a miss.
		A class's statics structure prints as `Class<T>` (an enum's as `Enum<T>`, an
		abstract's as `Abstract<T>`), as the class type of that name does, so the text of a
		type that mentions one does not say which it is; those are read every time.
	**/
	function textType(t:Type):Int {
		var key = TypeTools.toString(t);
		if (key.indexOf("Class<") >= 0 || key.indexOf("Enum<") >= 0 || key.indexOf("Abstract<") >= 0) return slowTypeId(t);
		var id = textIds.get(key);
		if (id != null) return id;
		id = slowTypeId(t);
		textIds.set(key, id);
		return id;
	}

	/**
		A type reached through a typedef, cached under the typedef's name. A structure
		typedef is given its id before its fields are read, so one that names itself
		finds that id instead of recursing.
	**/
	function typedefId(t:Type, d:Ref<DefType>, ps:Array<Type>):Int {
		if (ps.length == 0) {
			var cached = typedefIds.get(d.toString());
			if (cached != null) return cached;
		}
		// A typedef that instantiates itself at ever larger types has no finite table; cut it.
		if (typedefDepth > 8) {
			unsupportedCase("infinitely expanding typedef " + d.toString());
			return dynamicType();
		}
		typedefDepth++;
		var id = typedefIdAt(t, d, ps);
		typedefDepth--;
		return id;
	}

	function typedefIdAt(t:Type, d:Ref<DefType>, ps:Array<Type>):Int {
		var args = paramIds(ps);
		var key = "T" + d.toString() + argKey(args);
		var id = typeIds.get(key);
		if (id != null) return id;
		var next = Context.follow(t, true);
		switch (next) {
			case TAnonymous(an):
				id = rec(types, [TypeKind.ANON, Format.NONE, 0, 0, 0]);
				typeIds.set(key, id);
				if (ps.length == 0) typedefIds.set(d.toString(), id);
				var first = fillAnon(an, id);
				types[id * 5 + 2] = first;
				return id;
			case _ if (hasArguments(next)):
				// Reserved before the arguments are read: a typedef whose expansion names the
				// typedef again, such as `typedef C = Map<Int, {next:C}>`, finds this id.
				var kind = switch (next) {
					case TInst(_, _): TypeKind.CLASS;
					case TEnum(_, _): TypeKind.ENUM;
					case TAbstract(_, _): TypeKind.ABSTRACT;
					default: TypeKind.FUN;
				}
				id = rec(types, [kind, Format.NONE, Format.NONE, 0, 0]);
				typeIds.set(key, id);
				if (ps.length == 0) typedefIds.set(d.toString(), id);
				var def:Int;
				var argTypes:Array<Type>;
				switch (next) {
					case TInst(c, xs): def = classId(c); argTypes = xs;
					case TEnum(e, xs): def = enumId(e); argTypes = xs;
					case TAbstract(a, xs): def = abstractId(a); argTypes = xs;
					case TFun(fargs, ret): def = typeId(ret); argTypes = [for (x in fargs) x.t];
					default: def = Format.NONE; argTypes = [];
				}
				var a = argList(paramIds(argTypes));
				types[id * 5 + 2] = def;
				types[id * 5 + 3] = a[0];
				types[id * 5 + 4] = a[1];
				return id;
			default:
				id = typeId(next);
				typeIds.set(key, id);
				if (ps.length == 0) typedefIds.set(d.toString(), id);
				return id;
		}
	}

	/** A type whose record lists argument types: a class, enum or abstract with parameters, or a function. */
	function hasArguments(t:Type):Bool {
		return switch (t) {
			case TInst(_, ps) | TEnum(_, ps) | TAbstract(_, ps): ps.length > 0;
			case TFun(_, _): true;
			default: false;
		}
	}

	/** Write the fields of `an` and answer the id of the first. */
	function fillAnon(an:Ref<AnonType>, id:Int):Int {
		var fs = an.get().fields;
		var first = Std.int(anonFields.length / 3);
		// Reserve the records, then read the types: a field type can reach this structure again.
		for (f in fs) {
			anonFields.push(str(f.name));
			anonFields.push(0);
			anonFields.push(0);
		}
		types[id * 5 + 4] = fs.length;
		for (i in 0...fs.length) anonFields[(first + i) * 3 + 1] = typeId(fs[i].type);
		return first;
	}

	function argKey(args:Array<Int>):String {
		return args.length == 0 ? "" : "<" + args.join(",") + ">";
	}

	function paramIds(ps:Array<Type>):Array<Int> {
		return [for (p in ps) typeId(p)];
	}

	/** The structure of a type the text cache has not seen. */
	function slowTypeId(t:Type):Int {
		switch (t) {
			case TInst(c, ps):
				var args = paramIds(ps);
				var key = "I" + c.toString() + argKey(args);
				var id = typeIds.get(key);
				if (id != null) return id;
				var a = argList(args);
				id = rec(types, [TypeKind.CLASS, Format.NONE, classId(c), a[0], a[1]]);
				typeIds.set(key, id);
				return id;
			case TEnum(e, ps):
				var args = paramIds(ps);
				var key = "E" + e.toString() + argKey(args);
				var id = typeIds.get(key);
				if (id != null) return id;
				var a = argList(args);
				id = rec(types, [TypeKind.ENUM, Format.NONE, enumId(e), a[0], a[1]]);
				typeIds.set(key, id);
				return id;
			case TAbstract(ab, ps):
				var args = paramIds(ps);
				var key = "B" + ab.toString() + argKey(args);
				var id = typeIds.get(key);
				if (id != null) return id;
				var a = argList(args);
				id = rec(types, [TypeKind.ABSTRACT, Format.NONE, abstractId(ab), a[0], a[1]]);
				typeIds.set(key, id);
				return id;
			case TFun(args, ret):
				var ids = [for (a in args) typeId(a.t)];
				var r = typeId(ret);
				var key = "F" + ids.join(",") + ">" + r;
				var id = typeIds.get(key);
				if (id != null) return id;
				var a = argList(ids);
				id = rec(types, [TypeKind.FUN, Format.NONE, r, a[0], a[1]]);
				typeIds.set(key, id);
				return id;
			case TAnonymous(an):
				var fs = an.get().fields;
				var ids = [for (f in fs) typeId(f.type)];
				var key = "A" + [for (i in 0...fs.length) fs[i].name + ":" + ids[i]].join(",");
				var id = typeIds.get(key);
				if (id != null) return id;
				var first = Std.int(anonFields.length / 3);
				for (i in 0...fs.length) {
					anonFields.push(str(fs[i].name));
					anonFields.push(ids[i]);
					anonFields.push(0);
				}
				id = rec(types, [TypeKind.ANON, Format.NONE, first, 0, fs.length]);
				typeIds.set(key, id);
				return id;
			default:
				unsupportedCase("type " + Type.enumConstructor(t));
				return dynamicType();
		}
	}

	function dynamicType():Int {
		if (dynId < 0) dynId = rec(types, [TypeKind.DYNAMIC, Format.NONE, Format.NONE, 0, 0]);
		return dynId;
	}

	// ---- type definitions

	function classId(c:Ref<ClassType>):Int {
		var key = c.toString();
		var id = classIds.get(key);
		if (id != null) return id;
		id = Std.int(classes.length / 7);
		for (i in 0...7) classes.push(0);
		classIds.set(key, id);
		classTypes.push(c);
		classFilled.push(false);
		return id;
	}

	function enumId(e:Ref<EnumType>):Int {
		var key = e.toString();
		var id = enumIds.get(key);
		if (id != null) return id;
		var en = e.get();
		id = rec(enums, [str(key), 0, 0, 0]);
		enumIds.set(key, id);
		var first = Std.int(enumCtors.length / 6);
		var count = 0;
		for (name in en.names) {
			var ef = en.constructs.get(name);
			var ctor = rec(enumCtors, [str(name), id, ef.index, 0, 0, 0]);
			enumCtorIds.set(key + "." + name, ctor);
			count++;
		}
		enums[id * 4 + 1] = first;
		enums[id * 4 + 2] = count;
		// Types last: they may name this enum again.
		for (name in en.names) {
			var ef = en.constructs.get(name);
			var ctor = enumCtorIds.get(key + "." + name);
			enumCtors[ctor * 6 + 3] = typeId(ef.type);
			switch (Context.follow(ef.type)) {
				case TFun(args, _):
					var a = argList([for (x in args) typeId(x.t)]);
					enumCtors[ctor * 6 + 4] = a[0];
					enumCtors[ctor * 6 + 5] = a[1];
				default:
			}
		}
		return id;
	}

	function abstractId(a:Ref<AbstractType>):Int {
		var key = a.toString();
		var id = abstractIds.get(key);
		if (id != null) return id;
		var ab = a.get();
		id = rec(abstracts, [str(key), Format.NONE, Format.NONE, 0]);
		abstractIds.set(key, id);
		abstracts[id * 4 + 1] = typeId(ab.type);
		if (ab.impl != null) abstracts[id * 4 + 2] = classId(ab.impl);
		return id;
	}

	function localId(v:TVar):Int {
		var id = localIds.get(v.id);
		if (id != null) return id;
		id = rec(locals, [str(v.name), 0, v.capture ? LocalFlag.CAPTURED : 0]);
		localIds.set(v.id, id);
		locals[id * 3 + 1] = typeId(v.t);
		return id;
	}

	/** Fill a class's fields, once. */
	function fillClass(id:Int):Void {
		if (classFilled[id]) return;
		classFilled[id] = true;
		var c = classTypes[id];
		var ct = c.get();
		var path = c.toString();
		var flags = 0;
		if (ct.isInterface) flags |= ClassFlag.INTERFACE;
		if (ct.isFinal) flags |= ClassFlag.FINAL;
		if (ct.isExtern) flags |= ClassFlag.EXTERN;
		if (ct.isAbstract) flags |= ClassFlag.ABSTRACT;
		switch (ct.kind) {
			case KTypeParameter(_): flags |= ClassFlag.TYPE_PARAM;
			default:
		}
		var base = id * 7;
		classes[base] = str(path);
		classes[base + 1] = ct.superClass == null ? Format.NONE : typeId(TInst(ct.superClass.t, ct.superClass.params));
		classes[base + 2] = flags;
		var ifaces = [for (i in ct.interfaces) typeId(TInst(i.t, i.params))];
		var a = argList(ifaces);
		classes[base + 5] = a[0];
		classes[base + 6] = a[1];

		var all:Array<{f:ClassField, isStatic:Bool}> = [];
		if (ct.constructor != null) all.push({f: ct.constructor.get(), isStatic: false});
		for (f in ct.fields.get()) all.push({f: f, isStatic: false});
		for (f in ct.statics.get()) all.push({f: f, isStatic: true});
		var first = Std.int(fields.length / 6);
		// Reserve the records, so a field type that reaches this class again finds its fields in place.
		for (x in all) {
			var fid = rec(fields, [str(x.f.name), 0, id, fieldFlags(x.f, x.isStatic), Format.NONE, Format.NONE]);
			fieldIds.set(path + (x.isStatic ? "#s:" : "#i:") + x.f.name, fid);
		}
		classes[base + 3] = first;
		classes[base + 4] = all.length;
		for (i in 0...all.length) {
			var f = all[i].f;
			var fid = first + i;
			fields[fid * 6 + 1] = typeId(f.type);
			fields[fid * 6 + 4] = funcOfField(f);
		}
	}

	function fieldFlags(f:ClassField, isStatic:Bool):Int {
		var flags = isStatic ? FieldFlag.STATIC : 0;
		if (f.isPublic) flags |= FieldFlag.PUBLIC;
		if (f.isFinal) flags |= FieldFlag.FINAL;
		switch (f.kind) {
			case FMethod(k):
				flags |= FieldFlag.METHOD;
				if (k == MethInline) flags |= FieldFlag.INLINE;
			default:
		}
		return flags;
	}

	/** The id of the field `name` of `c` or of the nearest superclass that declares it. */
	function findField(c:Ref<ClassType>, name:String, isStatic:Bool):Int {
		var cur:Null<Ref<ClassType>> = c;
		var marker = isStatic ? "#s:" : "#i:";
		while (cur != null) {
			var path = cur.toString();
			var fid = fieldIds.get(path + marker + name);
			if (fid != null) return fid;
			var cid = classId(cur);
			if (cid >= 0 && !classFilled[cid]) {
				// Filling queues the class's bodies, which cannot happen between a node's children.
				if (walking) return Format.NONE;
				fillClass(cid);
				fid = fieldIds.get(path + marker + name);
				if (fid != null) return fid;
			}
			cur = superOf(path, cur);
		}
		return Format.NONE;
	}

	function superOf(path:String, c:Ref<ClassType>):Null<Ref<ClassType>> {
		if (superCache.exists(path)) return superCache.get(path);
		var sup = c.get().superClass;
		var result = sup == null ? null : sup.t;
		superCache.set(path, result);
		return result;
	}

	// ---- functions and the node queue

	function enqueueRoot(e:TypedExpr):Int {
		qExpr.push(e);
		qPseudo.push(null);
		return qExpr.length - 1;
	}

	function funcOfField(f:ClassField):Int {
		var t = Sys.time();
		var e = f.expr();
		bodyTime += Sys.time() - t;
		if (e == null) return Format.NONE;
		switch (e.expr) {
			case TFunction(tf):
				return funcId(tf);
			default:
				// A variable's initializer: a function with no parameters.
				return rec(funcs, [typeId(e.t), 0, 0, enqueueRoot(e), 0, Format.NONE]);
		}
	}

	function funcId(tf:TFunc):Int {
		var ps:Array<Int> = [];
		var defaults:Array<TypedExpr> = [];
		for (a in tf.args) {
			ps.push(localId(a.v));
			defaults.push(a.value);
		}
		var first = Std.int(params.length / 2);
		for (i in 0...ps.length) {
			params.push(ps[i]);
			params.push(defaults[i] == null ? Format.NONE : enqueueRoot(defaults[i]));
		}
		var id = rec(funcs, [typeId(tf.t), first, ps.length, 0, 0, Format.NONE]);
		funcs[id * 6 + 3] = enqueueRoot(tf.expr);
		return id;
	}

	function pushKid(e:TypedExpr):Void {
		qExpr.push(e);
		qPseudo.push(null);
	}

	function pushPseudo(kind:Int, a:Int, kids:Array<TypedExpr>):Void {
		qExpr.push(null);
		qPseudo.push(new Pseudo(kind, a, kids));
	}

	function moduleIndex(m:ModuleType):Array<Int> {
		switch (m) {
			case TClassDecl(c): return [ModuleKind.CLASS, classId(c)];
			case TEnumDecl(e): return [ModuleKind.ENUM, enumId(e)];
			case TAbstract(a): return [ModuleKind.ABSTRACT, abstractId(a)];
			case TTypeDecl(t):
				unsupportedCase("module type typedef");
				return [ModuleKind.CLASS, 0];
		}
	}

	/** Which enum an enum constructor belongs to, from the type of the value it is read from. */
	function enumCtorId(from:TypedExpr, ef:EnumField):Int {
		switch (Context.follow(from.t)) {
			case TEnum(e, _):
				enumId(e);
				var id = enumCtorIds.get(e.toString() + "." + ef.name);
				if (id != null) return id;
			default:
		}
		unsupportedCase("enum constructor");
		return 0;
	}

	function binopIndex(op:Binop):Int {
		return Type.enumIndex(op);
	}

	/** Write node `id` of the queue and enqueue its children. */
	function visit(e:TypedExpr):Void {
		var kind = 0, flags = 0, a = 0, b = 0;
		// Children are queued as they are met, as one group, so this is where it starts.
		// Nothing below queues anything else in between except `TFunction`, which has no children.
		var first = qExpr.length;
		switch (e.expr) {
			case TConst(c):
				kind = NodeKind.CONST;
				switch (c) {
					case TInt(i): flags = ConstKind.INT; a = i;
					case TFloat(s): flags = ConstKind.FLOAT; a = str(s);
					case TString(s): flags = ConstKind.STRING; a = str(s);
					case TBool(v): flags = ConstKind.BOOL; a = v ? 1 : 0;
					case TNull: flags = ConstKind.NULL;
					case TThis: flags = ConstKind.THIS;
					case TSuper: flags = ConstKind.SUPER;
				}
			case TLocal(v):
				kind = NodeKind.LOCAL;
				a = localId(v);
			case TArray(e1, e2):
				kind = NodeKind.ARRAY;
				pushKid(e1);
				pushKid(e2);
			case TBinop(op, e1, e2):
				kind = NodeKind.BINOP;
				switch (op) {
					case OpAssignOp(inner):
						a = binopIndex(op);
						b = binopIndex(inner);
					default:
						a = binopIndex(op);
				}
				pushKid(e1);
				pushKid(e2);
			case TField(obj, fa):
				kind = NodeKind.FIELD;
				switch (fa) {
					case FInstance(c, _, cf):
						a = FieldAccess.INSTANCE;
						b = findField(c, cf.get().name, false);
						if (b == Format.NONE) { a = FieldAccess.DYNAMIC; b = str(cf.get().name); dynamicFields++; }
					case FStatic(c, cf):
						a = FieldAccess.STATIC;
						b = findField(c, cf.get().name, true);
						if (b == Format.NONE) { a = FieldAccess.DYNAMIC; b = str(cf.get().name); dynamicFields++; }
					case FClosure(c, cf):
						var name = cf.get().name;
						if (c == null) {
							a = FieldAccess.DYNAMIC;
							b = str(name);
							dynamicFields++;
						} else {
							a = FieldAccess.CLOSURE;
							b = findField(c.c, name, false);
							if (b == Format.NONE) { a = FieldAccess.DYNAMIC; b = str(name); dynamicFields++; }
						}
					case FAnon(cf):
						a = FieldAccess.ANON;
						b = str(cf.get().name);
					case FDynamic(s):
						a = FieldAccess.DYNAMIC;
						b = str(s);
					case FEnum(en, ef):
						a = FieldAccess.ENUM;
						enumId(en);
						b = enumCtorIds.get(en.toString() + "." + ef.name);
				}
				pushKid(obj);
			case TTypeExpr(m):
				kind = NodeKind.TYPE_EXPR;
				var mi = moduleIndex(m);
				flags = mi[0];
				a = mi[1];
			case TParenthesis(e1):
				kind = NodeKind.PARENTHESIS;
				pushKid(e1);
			case TObjectDecl(fs):
				kind = NodeKind.OBJECT_DECL;
				for (f in fs) pushPseudo(NodeKind.OBJECT_FIELD, str(f.name), [f.expr]);
			case TArrayDecl(el):
				kind = NodeKind.ARRAY_DECL;
				for (x in el) pushKid(x);
			case TCall(callee, el):
				kind = NodeKind.CALL;
				pushKid(callee);
				for (x in el) pushKid(x);
			case TNew(c, _, el):
				kind = NodeKind.NEW;
				a = classId(c);
				for (x in el) pushKid(x);
			case TUnop(op, postFix, e1):
				kind = NodeKind.UNOP;
				a = Type.enumIndex(op);
				if (postFix) flags = NodeFlag.POSTFIX;
				pushKid(e1);
			case TFunction(tf):
				kind = NodeKind.FUNCTION;
				a = funcId(tf);
				first = qExpr.length;
			case TVar(v, init):
				kind = NodeKind.VAR;
				a = localId(v);
				if (init != null) {
					flags = NodeFlag.HAS_INIT;
					pushKid(init);
				}
			case TBlock(el):
				kind = NodeKind.BLOCK;
				for (x in el) pushKid(x);
			case TFor(v, it, body):
				kind = NodeKind.FOR;
				a = localId(v);
				pushKid(it);
				pushKid(body);
			case TIf(cond, t, els):
				kind = NodeKind.IF;
				pushKid(cond);
				pushKid(t);
				if (els != null) {
					flags = NodeFlag.HAS_ELSE;
					pushKid(els);
				}
			case TWhile(cond, body, normalWhile):
				kind = NodeKind.WHILE;
				if (!normalWhile) flags = NodeFlag.DO_WHILE;
				pushKid(cond);
				pushKid(body);
			case TSwitch(subject, cases, edef):
				kind = NodeKind.SWITCH;
				pushKid(subject);
				for (cs in cases) pushPseudo(NodeKind.CASE, cs.values.length, cs.values.concat([cs.expr]));
				if (edef != null) {
					flags = NodeFlag.HAS_DEFAULT;
					pushPseudo(NodeKind.DEFAULT, 0, [edef]);
				}
			case TTry(body, catches):
				kind = NodeKind.TRY;
				pushKid(body);
				for (c in catches) pushPseudo(NodeKind.CATCH, localId(c.v), [c.expr]);
			case TReturn(e1):
				kind = NodeKind.RETURN;
				if (e1 != null) {
					flags = NodeFlag.HAS_VALUE;
					pushKid(e1);
				}
			case TBreak:
				kind = NodeKind.BREAK;
			case TContinue:
				kind = NodeKind.CONTINUE;
			case TThrow(e1):
				kind = NodeKind.THROW;
				pushKid(e1);
			case TCast(e1, m):
				kind = NodeKind.CAST;
				if (m != null) {
					var mi = moduleIndex(m);
					flags = NodeFlag.HAS_TYPE | (mi[0] << 1);
					a = mi[1];
				}
				pushKid(e1);
			case TMeta(m, e1):
				kind = NodeKind.META;
				a = str(m.name);
				pushKid(e1);
			case TEnumParameter(e1, ef, index):
				kind = NodeKind.ENUM_PARAMETER;
				a = enumCtorId(e1, ef);
				b = index;
				pushKid(e1);
			case TEnumIndex(e1):
				kind = NodeKind.ENUM_INDEX;
				pushKid(e1);
			case TIdent(s):
				kind = NodeKind.IDENT;
				a = str(s);
		}
		var ty = typeId(e.t);
		var pos = withPositions ? posId(e.pos) : Format.NONE;
		writeNode(kind, flags, ty, pos, a, b, first, qExpr.length - first);
	}

	function writeNode(kind:Int, flags:Int, ty:Int, pos:Int, a:Int, b:Int, first:Int, count:Int):Void {
		var at = nodeCount * Format.NODE_REC_SIZE;
		if (at + Format.NODE_REC_SIZE > nodeBytes.length) {
			var bigger = Bytes.alloc(nodeBytes.length * 2);
			bigger.blit(0, nodeBytes, 0, at);
			nodeBytes = bigger;
		}
		Format.writeNodeRec(nodeBytes, at, kind, flags, ty, pos, a, b, first, count);
		nodeCount++;
	}

	function visitPseudo(p:Pseudo):Void {
		var first = qExpr.length;
		for (k in p.kids) pushKid(k);
		writeNode(p.kind, 0, Format.NONE, Format.NONE, p.a, 0, first, p.kids.length);
	}

	// ---- the entry point

	/**
		The program's entry: a function with no parameters whose body is the whole main
		expression, so whatever the compiler wrapped around `main` (the HL event loop,
		for one) runs as the compiler arranged.
	**/
	function entryFunc():Int {
		var m = Context.getMainExpr();
		if (m == null) return Format.NONE;
		return rec(funcs, [typeId(m.t), 0, 0, enqueueRoot(m), 0, Format.NONE]);
	}

	// ---- layers

	function warn(message:String):Void {
		Sys.println('[ash_tast] warning: $message');
	}

	function tagged(layer:Int, index:Int):Int {
		return TAG | (layer << 24) | index;
	}

	function stdRoot():String {
		return haxe.io.Path.removeTrailingSlashes(haxe.io.Path.directory(Context.resolvePath("StdTypes.hx")));
	}

	function resolveRoots(roots:Array<String>):Array<String> {
		var out = [for (r in roots) r == "<std>" ? stdRoot() : haxe.io.Path.removeTrailingSlashes(sys.FileSystem.fullPath(r))];
		out.sort(Reflect.compare);
		return out;
	}

	function scan(dir:String, lines:Array<String>):Void {
		for (name in sys.FileSystem.readDirectory(dir)) {
			var p = dir + "/" + name;
			if (sys.FileSystem.isDirectory(p))
				scan(p, lines);
			else if (StringTools.endsWith(name, ".hx")) {
				var st = sys.FileSystem.stat(p);
				lines.push("F " + p + ":" + st.size + ":" + Std.int(st.mtime.getTime() / 1000));
			}
		}
	}

	/** The defines in effect, less the ones that cannot change how a class types. */
	function currentDefines():Map<String, String> {
		var out:Map<String, String> = new Map();
		var defs = Context.getDefines();
		// The compiler reports each define under both spellings, `a-b` and `a_b`.
		for (k in defs.keys())
			if (k != "dce" && !StringTools.startsWith(StringTools.replace(k, "-", "_"), "ash_tast")) out.set(k, defs.get(k));
		return out;
	}

	/** What a layer was built from: every source file under its roots, by size and time. */
	function digestOf(roots:Array<String>):String {
		var lines:Array<String> = [];
		for (r in roots) scan(r, lines);
		lines.sort(Reflect.compare);
		return haxe.crypto.Md5.encode(lines.join("\n"));
	}

	/** Defines that change how the compiler types code without appearing in an `#if`. */
	function shapesTyping(name:String):Bool {
		return StringTools.startsWith(name, "analyzer") || name == "no-inline" || name == "no-opt" || name == "real-position"
			|| name == "haxe-next" || name == "display" || name == "no-debug";
	}

	/** The names the sources of the classes in `all` test in `#if` and `#elseif`. */
	function usedDefines(all:Array<Type>):Array<String> {
		var seen:Map<String, Bool> = new Map();
		var files:Map<String, Bool> = new Map();
		var condition = ~/^\s*#(?:if|elseif)\b(.*)$/;
		var word = ~/[A-Za-z_][A-Za-z0-9_]*/;
		for (t in all) {
			var pos:Null<Position> = switch (t) {
				case TInst(c, _): c.get().pos;
				case TEnum(e, _): e.get().pos;
				case TAbstract(a, _): a.get().pos;
				default: null;
			}
			if (pos == null) continue;
			var file = Context.getPosInfos(pos).file;
			if (files.exists(file)) continue;
			files.set(file, true);
			var text = try sys.io.File.getContent(file) catch (e:Dynamic) "";
			if (text.indexOf("#if") < 0 && text.indexOf("#elseif") < 0) continue;
			for (line in text.split("\n")) {
				if (!condition.match(line)) continue;
				var rest = condition.matched(1);
				while (word.match(rest)) {
					seen.set(word.matched(0), true);
					rest = word.matchedRight();
				}
			}
		}
		var names = [for (k in seen.keys()) k];
		names.sort(Reflect.compare);
		return names;
	}

	/**
		Whether code typed under `layer`'s defines types the same under this build's: every
		define the layer was built with is set the same, and any other define set now is one
		that neither the layer's sources test nor the compiler is known to type by.
	**/
	function definesAllow(file:String, layer:Map<String, String>, used:Map<String, Bool>):Bool {
		var now = currentDefines();
		for (k => v in layer)
			if (now.get(k) != v) {
				warn('layer $file is out of date: it was built with -D $k=$v and this build has ${now.exists(k) ? "-D " + k + "=" + now.get(k) : "no " + k}');
				return false;
			}
		for (k => v in now) {
			if (layer.exists(k)) continue;
			if (used.exists(StringTools.replace(k, "-", "_")) || used.exists(k) || shapesTyping(k)) {
				warn('layer $file is out of date: this build sets -D $k, which the layer was not built with and its sources depend on');
				return false;
			}
		}
		return true;
	}

	function loadParents():Void {
		for (p in 0...parentPaths.length)
			if (!readParent(p, parentPaths[p])) {
				resetParents();
				return;
			}
	}

	function resetParents():Void {
		parentLayers = 0;
		parentRoots = [];
		parentIdPairs = [];
		instIds = new Map();
		abstractTypeIds = new Map();
		enumTypeIds = new Map();
		typedefIds = new Map();
		textIds = new Map();
		typeIds = new Map();
		classIds = new Map();
		enumIds = new Map();
		abstractIds = new Map();
		fieldIds = new Map();
		enumCtorIds = new Map();
	}

	/** Read layer `p`'s key index into the maps, every id tagged; false when it cannot be used. */
	function readParent(p:Int, file:String):Bool {
		var bytes:Bytes;
		try
			bytes = sys.io.File.getBytes(file)
		catch (e:Dynamic) {
			warn('layer $file cannot be read');
			return false;
		}
		if (bytes.length < Format.HEADER_SIZE
			|| bytes.getInt32(Format.HEADER_MAGIC_AT) != Format.MAGIC
			|| bytes.getInt32(Format.HEADER_VERSION_AT) != Format.VERSION
			|| bytes.getInt32(Format.HEADER_SCHEMA_HASH_LO_AT) != Format.SCHEMA_HASH_LO
			|| bytes.getInt32(Format.HEADER_SCHEMA_HASH_HI_AT) != Format.SCHEMA_HASH_HI
			|| bytes.getInt32(Format.HEADER_SECTION_COUNT_AT) < Format.SECTION_COUNT) {
			warn('layer $file was written for another layout; export it again');
			return false;
		}
		function offsetOf(section:Int):Int {
			return bytes.getInt32(Format.HEADER_SIZE + section * Format.SECTION_REC_SIZE + Format.SECTION_REC_OFFSET_AT);
		}
		function countOf(section:Int):Int {
			return bytes.getInt32(Format.HEADER_SIZE + section * Format.SECTION_REC_SIZE + Format.SECTION_REC_COUNT_AT);
		}
		// Its own parents must be exactly the layers loaded before it.
		var named = countOf(Format.SECTION_PARENTS);
		var at = offsetOf(Format.SECTION_PARENTS);
		var same = named == p;
		for (i in 0...(same ? named : 0)) {
			var r = at + i * Format.PARENT_REC_SIZE;
			if (bytes.getInt32(r + Format.PARENT_REC_ID_LO_AT) != parentIdPairs[i * 2]
				|| bytes.getInt32(r + Format.PARENT_REC_ID_HI_AT) != parentIdPairs[i * 2 + 1]) same = false;
		}
		if (!same) {
			warn('layer $file was not built on the layers given before it');
			return false;
		}

		var blobAt = offsetOf(Format.SECTION_STRINGS);
		var indexAt = offsetOf(Format.SECTION_STRING_INDEX);
		function text(id:Int):String {
			var r = indexAt + id * Format.STR_REC_SIZE;
			return bytes.getString(blobAt + bytes.getInt32(r + Format.STR_REC_OFFSET_AT), bytes.getInt32(r + Format.STR_REC_LEN_AT));
		}
		var roots:Array<String> = [];
		var digest:Null<String> = null;
		var layerDefines:Map<String, String> = new Map();
		var usedNames:Map<String, Bool> = new Map();
		var keysAt = offsetOf(Format.SECTION_KEYS);
		for (i in 0...countOf(Format.SECTION_KEYS)) {
			var r = keysAt + i * Format.KEY_REC_SIZE;
			var kind = bytes.getInt32(r + Format.KEY_REC_KIND_AT);
			var key = text(bytes.getInt32(r + Format.KEY_REC_KEY_AT));
			var id = tagged(p, bytes.getInt32(r + Format.KEY_REC_VALUE_AT));
			switch (kind) {
				case KeyKind.INST_TYPE: instIds.set(key, id);
				case KeyKind.ABSTRACT_TYPE: abstractTypeIds.set(key, id);
				case KeyKind.ENUM_TYPE: enumTypeIds.set(key, id);
				case KeyKind.TYPEDEF_TYPE: typedefIds.set(key, id);
				case KeyKind.TEXT_TYPE: textIds.set(key, id);
				case KeyKind.CLASS: classIds.set(key, id);
				case KeyKind.ENUM: enumIds.set(key, id);
				case KeyKind.ABSTRACT: abstractIds.set(key, id);
				case KeyKind.FIELD: fieldIds.set(key, id);
				case KeyKind.ENUM_CTOR: enumCtorIds.set(key, id);
				case KeyKind.ROOT: roots.push(key);
				case KeyKind.DIGEST: digest = key;
				case KeyKind.DEFINE:
					var eq = key.indexOf("=");
					layerDefines.set(key.substr(0, eq), key.substr(eq + 1));
				case KeyKind.USED_DEFINE: usedNames.set(key, true);
				default:
			}
		}
		if (digest == null) {
			warn('$file is not a layer: it has no record of what it was built from');
			return false;
		}
		if (digestOf(roots) != digest) {
			warn('layer $file is out of date: its source files changed since it was built; build it again');
			return false;
		}
		if (!definesAllow(file, layerDefines, usedNames)) return false;
		parentRoots.push(roots);
		parentIdPairs.push(bytes.getInt32(Format.HEADER_LAYER_ID_LO_AT));
		parentIdPairs.push(bytes.getInt32(Format.HEADER_LAYER_ID_HI_AT));
		parentLayers++;
		return true;
	}

	/** Every class a layer holds must come from the sources that layer was built from. */
	function layersCoverClasses(all:Array<Type>):Bool {
		for (t in all) switch (t) {
			case TInst(c, _):
				var id = classIds.get(c.toString());
				if (id != null && id < 0) {
					var layer = (id >> 24) & 0x7f;
					var file = try sys.FileSystem.fullPath(Context.getPosInfos(c.get().pos).file) catch (e:Dynamic) "";
					var inside = false;
					for (root in parentRoots[layer])
						if (StringTools.startsWith(file, root + "/")) {
							inside = true;
							break;
						}
					if (!inside) {
						warn('class ${c.toString()} comes from $file, not from the sources of the layer that holds a class of that name');
						return false;
					}
				}
			default:
		}
		return true;
	}

	/** Note what dead-code removal kept from the layers: whole classes, enums, abstracts and fields. */
	function recordKept(all:Array<Type>):Void {
		for (t in all) switch (t) {
			case TInst(c, _):
				var path = c.toString();
				var id = classIds.get(path);
				if (id == null || id >= 0) continue;
				keptRecs.push(KeptKind.CLASS);
				keptRecs.push(id);
				var ct = c.get();
				function keep(name:String, isStatic:Bool) {
					var fid = fieldIds.get(path + (isStatic ? "#s:" : "#i:") + name);
					if (fid != null && fid < 0) {
						keptRecs.push(KeptKind.FIELD);
						keptRecs.push(fid);
					} else
						unsupportedCase("a kept field the layer does not index");
				}
				if (ct.constructor != null) keep("new", false);
				for (f in ct.fields.get()) keep(f.name, false);
				for (f in ct.statics.get()) keep(f.name, true);
			case TEnum(e, _):
				var id = enumIds.get(e.toString());
				if (id != null && id < 0) {
					keptRecs.push(KeptKind.ENUM);
					keptRecs.push(id);
				}
			case TAbstract(a, _):
				var id = abstractIds.get(a.toString());
				if (id != null && id < 0) {
					keptRecs.push(KeptKind.ABSTRACT);
					keptRecs.push(id);
				}
			default:
		}
	}

	/**
		A layer should hold only what its sources define. A class from elsewhere in it
		(typed because something here refers to it) means the layer needs a layer below
		it that holds that class, and a program built on it will not be able to use it.
	**/
	function warnAboutForeignClasses(roots:Array<String>):Void {
		var foreign:Array<String> = [];
		for (id in 0...classTypes.length) {
			var c = classTypes[id];
			var file = try sys.FileSystem.fullPath(Context.getPosInfos(c.get().pos).file) catch (e:Dynamic) "";
			var inside = false;
			for (root in roots)
				if (StringTools.startsWith(file, root + "/")) {
					inside = true;
					break;
				}
			if (!inside) foreign.push(c.toString());
		}
		if (foreign.length > 0)
			warn('this layer holds ${foreign.length} classes from outside its sources, such as ${foreign.slice(0, 3).join(", ")}; '
				+ 'build it on a layer that holds them, or a program cannot use it');
	}

	/** Index what this file holds, by name, for the files built on it. */
	function writeKeys(roots:Array<String>, digest:String, used:Array<String>):Void {
		function emit(kind:Int, map:Map<String, Int>) {
			var names = [for (k in map.keys()) k];
			names.sort(Reflect.compare);
			for (k in names) {
				var v = map.get(k);
				// Entries seeded from a layer below are tagged, and belong to that layer's index.
				if (v < 0) continue;
				keyRecs.push(kind);
				keyRecs.push(str(k));
				keyRecs.push(v);
			}
		}
		emit(KeyKind.INST_TYPE, instIds);
		emit(KeyKind.ABSTRACT_TYPE, abstractTypeIds);
		emit(KeyKind.ENUM_TYPE, enumTypeIds);
		emit(KeyKind.TYPEDEF_TYPE, typedefIds);
		emit(KeyKind.TEXT_TYPE, textIds);
		emit(KeyKind.CLASS, classIds);
		emit(KeyKind.ENUM, enumIds);
		emit(KeyKind.ABSTRACT, abstractIds);
		emit(KeyKind.FIELD, fieldIds);
		emit(KeyKind.ENUM_CTOR, enumCtorIds);
		for (r in roots) {
			keyRecs.push(KeyKind.ROOT);
			keyRecs.push(str(r));
			keyRecs.push(0);
		}
		keyRecs.push(KeyKind.DIGEST);
		keyRecs.push(str(digest));
		keyRecs.push(0);
		var defs = currentDefines();
		var names = [for (k in defs.keys()) k];
		names.sort(Reflect.compare);
		for (k in names) {
			keyRecs.push(KeyKind.DEFINE);
			keyRecs.push(str(k + "=" + defs.get(k)));
			keyRecs.push(0);
		}
		for (u in used) {
			keyRecs.push(KeyKind.USED_DEFINE);
			keyRecs.push(str(u));
			keyRecs.push(0);
		}
	}

	/** The first 32 hex digits of an MD5 as two 32-bit words, for a layer's identity. */
	function identityOf(hex:String):Array<Int> {
		function word(h:String):Int {
			return (Std.parseInt("0x" + h.substr(0, 4)) << 16) | Std.parseInt("0x" + h.substr(4, 4));
		}
		return [word(hex.substr(0, 8)), word(hex.substr(8, 8))];
	}

	// ---- writing

	/** The kind and path of a type, which orders the types the compiler hands over. */
	function sortKey(t:Type):String {
		return switch (t) {
			case TInst(c, _): "1" + c.toString();
			case TEnum(e, _): "2" + e.toString();
			case TAbstract(a, _): "3" + a.toString();
			default: "4";
		}
	}

	function run(compilerTypes:Array<Type>):Void {
		var t0 = Sys.time();
		// The compiler's order depends on what it had cached (a compilation server hands over
		// modules in the order it first met them); sorting makes the file the same either way.
		var all = compilerTypes.copy();
		var keys = [for (t in all) sortKey(t)];
		var order = [for (i in 0...all.length) i];
		order.sort(function(a, b) return Reflect.compare(keys[a], keys[b]));
		all = [for (i in order) all[i]];
		loadParents();
		if (parentLayers > 0 && !layersCoverClasses(all)) resetParents();
		var tLayers = Sys.time();
		for (t in all) switch (t) {
			case TInst(c, _):
				// A class a layer holds is named, not exported again.
				var id = classId(c);
				if (id >= 0) fillClass(id);
			case TEnum(e, _): enumId(e);
			case TAbstract(a, _): abstractId(a);
			default:
		}
		if (parentLayers > 0) recordKept(all);
		// Referenced classes the list did not carry (type parameters, externs) are filled too.
		var done = 0;
		while (done < classTypes.length) {
			fillClass(done);
			done++;
		}

		var entry = entryFunc();
		var i = 0;
		while (true) {
			walking = true;
			while (i < qExpr.length) {
				var e = qExpr[i];
				if (e != null) visit(e) else visitPseudo(qPseudo[i]);
				i++;
			}
			walking = false;
			// A class reached from an expression is filled too, which can queue its bodies.
			var k = done;
			while (k < classTypes.length) {
				fillClass(k);
				k++;
			}
			done = classTypes.length;
			if (i == qExpr.length) break;
		}
		var tBuilt = Sys.time();

		parentRecs = parentIdPairs.copy();
		if (layerRoots != null) {
			var roots = resolveRoots(layerRoots);
			warnAboutForeignClasses(roots);
			var digest = digestOf(roots);
			writeKeys(roots, digest, usedDefines(all));
			layerId = identityOf(haxe.crypto.Md5.encode(digest + ":" + parentIdPairs.join(",")));
		}
		var bytes = assemble(entry);
		sys.io.File.saveBytes(path, bytes);
		if (verbose) {
			var ms = function(a:Float, b:Float) return Std.int((b - a) * 1000);
			Sys.println('[ash_tast] ${path}: ${bytes.length} bytes, ${nodeCount} nodes, ${Std.int(types.length / 5)} types, '
				+ '${classTypes.length} classes, ${Std.int(fields.length / 6)} fields, ${Std.int(funcs.length / 6)} functions, '
				+ '${Std.int(locals.length / 3)} locals, ${strOffsets.length} strings');
			Sys.println('[ash_tast] build ${ms(t0, tBuilt)} ms (layers ${ms(t0, tLayers)} ms, field.expr() ${Std.int(bodyTime * 1000)} ms), assemble and write ${ms(tBuilt, Sys.time())} ms');
			if (parentLayers > 0 || parentPaths.length > 0)
				Sys.println('[ash_tast] layers used: $parentLayers of ${parentPaths.length}, kept from them: ${Std.int(keptRecs.length / 2)} entries');
			if (dynamicFields > 0) Sys.println('[ash_tast] fields written by name only: $dynamicFields');
			for (k => n in unsupported) Sys.println('[ash_tast] unsupported $k: $n');
		}
	}

	function assemble(entry:Int):Bytes {
		var counts = [
			blobLen, strOffsets.length, Std.int(positions.length / 3), Std.int(types.length / 5), typeArgs.length,
			Std.int(classes.length / 7), Std.int(fields.length / 6), Std.int(abstracts.length / 4),
			Std.int(enums.length / 4), Std.int(enumCtors.length / 6), Std.int(anonFields.length / 3),
			Std.int(locals.length / 3), Std.int(params.length / 2), Std.int(funcs.length / 6), nodeCount,
			Std.int(keyRecs.length / 3), Std.int(keptRecs.length / 2), Std.int(parentRecs.length / 2),
			// Order, Metas, Defines, Resources and ResourceData: only the compiler plugin writes them.
			0, 0, 0, 0, 0
		];
		var elems = [
			Format.STRINGS_ELEM, Format.STRING_INDEX_ELEM, Format.POSITIONS_ELEM, Format.TYPES_ELEM,
			Format.TYPE_ARGS_ELEM, Format.CLASSES_ELEM, Format.FIELDS_ELEM, Format.ABSTRACTS_ELEM,
			Format.ENUMS_ELEM, Format.ENUM_CTORS_ELEM, Format.ANON_FIELDS_ELEM, Format.LOCALS_ELEM,
			Format.PARAMS_ELEM, Format.FUNCS_ELEM, Format.NODES_ELEM, Format.KEYS_ELEM, Format.KEPT_ELEM,
			Format.PARENTS_ELEM, Format.ORDER_ELEM, Format.METAS_ELEM, Format.DEFINES_ELEM, Format.RESOURCES_ELEM,
			Format.RESOURCE_DATA_ELEM
		];
		var offsets:Array<Int> = [];
		var at = Format.HEADER_SIZE + Format.SECTION_COUNT * Format.SECTION_REC_SIZE;
		for (s in 0...Format.SECTION_COUNT) {
			offsets.push(at);
			at += counts[s] * elems[s];
		}
		var out = Bytes.alloc(at);
		Format.writeHeader(out, 0, Format.MAGIC, Format.VERSION, Format.SCHEMA_HASH_LO, Format.SCHEMA_HASH_HI,
			haxeVersion(), layerId[0], layerId[1], 0, entry, Format.SECTION_COUNT);
		for (s in 0...Format.SECTION_COUNT)
			Format.writeSectionRec(out, Format.HEADER_SIZE + s * Format.SECTION_REC_SIZE, offsets[s], counts[s]);

		out.blit(offsets[Format.SECTION_STRINGS], blob.getBytes(), 0, blobLen);
		for (i in 0...strOffsets.length)
			Format.writeStrRec(out, offsets[Format.SECTION_STRING_INDEX] + i * Format.STR_REC_SIZE, strOffsets[i], strLens[i]);
		for (i in 0...counts[Format.SECTION_POSITIONS])
			Format.writePosRec(out, offsets[Format.SECTION_POSITIONS] + i * Format.POS_REC_SIZE, positions[i * 3],
				positions[i * 3 + 1], positions[i * 3 + 2]);
		for (i in 0...counts[Format.SECTION_TYPES]) {
			var r = i * 5;
			Format.writeTypeRec(out, offsets[Format.SECTION_TYPES] + i * Format.TYPE_REC_SIZE, types[r], 0, types[r + 1],
				types[r + 2], types[r + 3], types[r + 4], 0);
		}
		for (i in 0...typeArgs.length)
			out.setInt32(offsets[Format.SECTION_TYPE_ARGS] + i * 4, typeArgs[i]);
		for (i in 0...counts[Format.SECTION_CLASSES]) {
			var r = i * 7;
			Format.writeClassRec(out, offsets[Format.SECTION_CLASSES] + i * Format.CLASS_REC_SIZE, classes[r], classes[r + 1],
				classes[r + 2], classes[r + 3], classes[r + 4], classes[r + 5], classes[r + 6], Format.NONE, Format.NONE);
		}
		for (i in 0...counts[Format.SECTION_FIELDS]) {
			var r = i * 6;
			Format.writeFieldRec(out, offsets[Format.SECTION_FIELDS] + i * Format.FIELD_REC_SIZE, fields[r], fields[r + 1],
				fields[r + 2], fields[r + 3], fields[r + 4], fields[r + 5]);
		}
		for (i in 0...counts[Format.SECTION_ABSTRACTS]) {
			var r = i * 4;
			Format.writeAbstractRec(out, offsets[Format.SECTION_ABSTRACTS] + i * Format.ABSTRACT_REC_SIZE, abstracts[r],
				abstracts[r + 1], abstracts[r + 2], abstracts[r + 3]);
		}
		for (i in 0...counts[Format.SECTION_ENUMS]) {
			var r = i * 4;
			Format.writeEnumRec(out, offsets[Format.SECTION_ENUMS] + i * Format.ENUM_REC_SIZE, enums[r], enums[r + 1],
				enums[r + 2], enums[r + 3], Format.NONE);
		}
		for (i in 0...counts[Format.SECTION_ENUM_CTORS]) {
			var r = i * 6;
			Format.writeEnumCtorRec(out, offsets[Format.SECTION_ENUM_CTORS] + i * Format.ENUM_CTOR_REC_SIZE, enumCtors[r],
				enumCtors[r + 1], enumCtors[r + 2], enumCtors[r + 3], enumCtors[r + 4], enumCtors[r + 5]);
		}
		for (i in 0...counts[Format.SECTION_ANON_FIELDS]) {
			var r = i * 3;
			Format.writeAnonFieldRec(out, offsets[Format.SECTION_ANON_FIELDS] + i * Format.ANON_FIELD_REC_SIZE,
				anonFields[r], anonFields[r + 1], anonFields[r + 2]);
		}
		for (i in 0...counts[Format.SECTION_LOCALS]) {
			var r = i * 3;
			Format.writeLocalRec(out, offsets[Format.SECTION_LOCALS] + i * Format.LOCAL_REC_SIZE, locals[r], locals[r + 1],
				locals[r + 2]);
		}
		for (i in 0...counts[Format.SECTION_PARAMS])
			Format.writeParamRec(out, offsets[Format.SECTION_PARAMS] + i * Format.PARAM_REC_SIZE, params[i * 2], params[i * 2 + 1]);
		for (i in 0...counts[Format.SECTION_FUNCS]) {
			var r = i * 6;
			Format.writeFuncRec(out, offsets[Format.SECTION_FUNCS] + i * Format.FUNC_REC_SIZE, funcs[r], funcs[r + 1],
				funcs[r + 2], funcs[r + 3], funcs[r + 4], funcs[r + 5]);
		}
		out.blit(offsets[Format.SECTION_NODES], nodeBytes, 0, nodeCount * Format.NODE_REC_SIZE);
		for (i in 0...counts[Format.SECTION_KEYS])
			Format.writeKeyRec(out, offsets[Format.SECTION_KEYS] + i * Format.KEY_REC_SIZE, keyRecs[i * 3], keyRecs[i * 3 + 1],
				keyRecs[i * 3 + 2]);
		for (i in 0...counts[Format.SECTION_KEPT])
			Format.writeKeptRec(out, offsets[Format.SECTION_KEPT] + i * Format.KEPT_REC_SIZE, keptRecs[i * 2], keptRecs[i * 2 + 1]);
		for (i in 0...counts[Format.SECTION_PARENTS])
			Format.writeParentRec(out, offsets[Format.SECTION_PARENTS] + i * Format.PARENT_REC_SIZE, parentRecs[i * 2],
				parentRecs[i * 2 + 1]);
		return out;
	}

	/** `major * 10000 + minor * 100 + patch`, from the `haxe_ver` define (4.3.6 is 4.306). */
	function haxeVersion():Int {
		var v = Std.parseFloat(Context.definedValue("haxe_ver"));
		var major = Std.int(v);
		var rest = Math.round((v - major) * 1000);
		return major * 10000 + Std.int(rest / 100) * 100 + rest % 100;
	}
}

private class Pseudo {
	public final kind:Int;
	public final a:Int;
	public final kids:Array<TypedExpr>;

	public function new(kind:Int, a:Int, kids:Array<TypedExpr>) {
		this.kind = kind;
		this.a = a;
		this.kids = kids;
	}
}
#end
