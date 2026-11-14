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
	program is typed and unused code is removed (`Context.onGenerate`), and
	writes the file. With `--no-output` the target's own generator does not run.
	`-D ash_tast_positions` adds a source position to every node, and
	`-D ash_tast_verbose` prints what was written and what could not be.
**/
class Export {
	public static function init(path:String = "program.tast"):Void {
		Format.checkCompiler();
		Context.onGenerate(function(types) new Export(path).run(types));
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
	var monoId = -1;
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

	// What could not be written exactly.
	var dynamicFields = 0;
	var unsupported:Map<String, Int> = new Map();

	final path:String;

	function new(path:String) {
		this.path = path;
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
				var inner = r.get();
				return inner == null ? monoType() : typeId(inner);
			case TLazy(f):
				return typeId(f());
			case TInst(_, _) | TEnum(_, _) | TAbstract(_, _) | TFun(_, _) | TAnonymous(_):
				// Not followed: following strips `Null<T>`, and the program keeps that.
				return textType(t);
		}
	}

	/** A type found by the compiler's text for it; the structure is read only on a miss. */
	function textType(t:Type):Int {
		var key = TypeTools.toString(t);
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
			default:
				id = typeId(next);
				typeIds.set(key, id);
				if (ps.length == 0) typedefIds.set(d.toString(), id);
				return id;
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

	function monoType():Int {
		if (monoId < 0) monoId = rec(types, [TypeKind.MONO, Format.NONE, Format.NONE, 0, 0]);
		return monoId;
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
			fieldIds.set(id + (x.isStatic ? "s:" : "i:") + x.f.name, fid);
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
		while (cur != null) {
			var cid = classId(cur);
			if (!classFilled[cid]) {
				// Filling queues the class's bodies, which cannot happen between a node's children.
				if (walking) return Format.NONE;
				fillClass(cid);
			}
			var fid = fieldIds.get(cid + (isStatic ? "s:" : "i:") + name);
			if (fid != null) return fid;
			var sup = cur.get().superClass;
			cur = sup == null ? null : sup.t;
		}
		return Format.NONE;
	}

	// ---- functions and the node queue

	function enqueueRoot(e:TypedExpr):Int {
		qExpr.push(e);
		qPseudo.push(null);
		return qExpr.length - 1;
	}

	function funcOfField(f:ClassField):Int {
		var e = f.expr();
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

	// ---- writing

	function run(all:Array<Type>):Void {
		var t0 = Sys.time();
		for (t in all) switch (t) {
			case TInst(c, _): fillClass(classId(c));
			case TEnum(e, _): enumId(e);
			case TAbstract(a, _): abstractId(a);
			default:
		}
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

		var bytes = assemble(entry);
		sys.io.File.saveBytes(path, bytes);
		if (verbose) {
			var ms = function(a:Float, b:Float) return Std.int((b - a) * 1000);
			Sys.println('[ash_tast] ${path}: ${bytes.length} bytes, ${nodeCount} nodes, ${Std.int(types.length / 5)} types, '
				+ '${classTypes.length} classes, ${Std.int(fields.length / 6)} fields, ${Std.int(funcs.length / 6)} functions, '
				+ '${Std.int(locals.length / 3)} locals, ${strOffsets.length} strings');
			Sys.println('[ash_tast] build ${ms(t0, tBuilt)} ms, assemble and write ${ms(tBuilt, Sys.time())} ms');
			if (dynamicFields > 0) Sys.println('[ash_tast] fields written by name only: $dynamicFields');
			for (k => n in unsupported) Sys.println('[ash_tast] unsupported $k: $n');
		}
	}

	function assemble(entry:Int):Bytes {
		var counts = [
			blobLen, strOffsets.length, Std.int(positions.length / 3), Std.int(types.length / 5), typeArgs.length,
			Std.int(classes.length / 7), Std.int(fields.length / 6), Std.int(abstracts.length / 4),
			Std.int(enums.length / 4), Std.int(enumCtors.length / 6), Std.int(anonFields.length / 3),
			Std.int(locals.length / 3), Std.int(params.length / 2), Std.int(funcs.length / 6), nodeCount
		];
		var elems = [
			Format.STRINGS_ELEM, Format.STRING_INDEX_ELEM, Format.POSITIONS_ELEM, Format.TYPES_ELEM,
			Format.TYPE_ARGS_ELEM, Format.CLASSES_ELEM, Format.FIELDS_ELEM, Format.ABSTRACTS_ELEM,
			Format.ENUMS_ELEM, Format.ENUM_CTORS_ELEM, Format.ANON_FIELDS_ELEM, Format.LOCALS_ELEM,
			Format.PARAMS_ELEM, Format.FUNCS_ELEM, Format.NODES_ELEM
		];
		var offsets:Array<Int> = [];
		var at = 32 + Format.SECTION_COUNT * Format.SECTION_REC_SIZE;
		for (s in 0...Format.SECTION_COUNT) {
			offsets.push(at);
			at += counts[s] * elems[s];
		}
		var out = Bytes.alloc(at);
		Format.writeHeader(out, 0, Format.MAGIC, Format.VERSION, Format.SCHEMA_HASH_LO, Format.SCHEMA_HASH_HI,
			haxeVersion(), 0, entry, Format.SECTION_COUNT);
		for (s in 0...Format.SECTION_COUNT)
			Format.writeSectionRec(out, 32 + s * Format.SECTION_REC_SIZE, offsets[s], counts[s]);

		out.blit(offsets[Format.SECTION_STRINGS], blob.getBytes(), 0, blobLen);
		for (i in 0...strOffsets.length)
			Format.writeStrRec(out, offsets[Format.SECTION_STRING_INDEX] + i * Format.STR_REC_SIZE, strOffsets[i], strLens[i]);
		for (i in 0...counts[Format.SECTION_POSITIONS])
			Format.writePosRec(out, offsets[Format.SECTION_POSITIONS] + i * Format.POS_REC_SIZE, positions[i * 3],
				positions[i * 3 + 1], positions[i * 3 + 2]);
		for (i in 0...counts[Format.SECTION_TYPES]) {
			var r = i * 5;
			Format.writeTypeRec(out, offsets[Format.SECTION_TYPES] + i * Format.TYPE_REC_SIZE, types[r], 0, types[r + 1],
				types[r + 2], types[r + 3], types[r + 4]);
		}
		for (i in 0...typeArgs.length)
			out.setInt32(offsets[Format.SECTION_TYPE_ARGS] + i * 4, typeArgs[i]);
		for (i in 0...counts[Format.SECTION_CLASSES]) {
			var r = i * 7;
			Format.writeClassRec(out, offsets[Format.SECTION_CLASSES] + i * Format.CLASS_REC_SIZE, classes[r], classes[r + 1],
				classes[r + 2], classes[r + 3], classes[r + 4], classes[r + 5], classes[r + 6]);
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
				enums[r + 2], enums[r + 3]);
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
