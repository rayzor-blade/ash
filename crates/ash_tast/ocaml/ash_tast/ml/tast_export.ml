open Type
module F = Tast_format

(* Writes the typed program as a TAST file. The structure follows haxe/ash/tast/Export.hx,
   reading the compiler's own types instead of the macro API's copies of them. *)

module Dyn = struct
	type 'a t = { mutable a : 'a array; mutable n : int; zero : 'a }

	let create zero = { a = Array.make 256 zero; n = 0; zero }
	let length d = d.n
	let get d i = d.a.(i)
	let set d i x = d.a.(i) <- x

	let push d x =
		if d.n = Array.length d.a then begin
			let bigger = Array.make (d.n * 2) d.zero in
			Array.blit d.a 0 bigger 0 d.n;
			d.a <- bigger
		end;
		d.a.(d.n) <- x;
		d.n <- d.n + 1
end

type pseudo = { pkind : int; pa : int; kids : item list }
and item = Empty | Expr of texpr | Arg of Ast.expr | Pseudo of pseudo

type path = Globals.path

type tkey =
	| KInst of path * int list
	| KEnum of path * int list
	| KAbstract of path * int list
	| KFun of int list * int * int
	| KAnon of (string * int) list * int * int
	| KTypedef of path * int list

type st = {
	com : Common.context;
	with_positions : bool;
	blob : Buffer.t;
	str_off : int Dyn.t;
	str_len : int Dyn.t;
	str_ids : (string, int) Hashtbl.t;
	types : int Dyn.t; (* kind, name, def, args_first, args_count, opt_mask, flags, via *)
	type_args : int Dyn.t;
	classes : int Dyn.t; (* name, super_type, flags, fields_first, fields_count, ifaces_first, ifaces_count, init_func, meta_root *)
	fields : int Dyn.t; (* name, ty, owner, flags, func, pos *)
	abstracts : int Dyn.t; (* name, underlying, impl_class, flags *)
	enums : int Dyn.t; (* name, ctors_first, ctors_count, flags, meta_root *)
	enum_ctors : int Dyn.t; (* name, enum_id, index, ty, args_first, args_count *)
	anon_fields : int Dyn.t; (* name, ty, flags *)
	locals : int Dyn.t; (* name, ty, flags *)
	params : int Dyn.t; (* local, default_value *)
	funcs : int Dyn.t; (* ret, params_first, params_count, root, flags, pos *)
	positions : int Dyn.t; (* file, start, end *)
	nodes : int Dyn.t; (* kind, flags, ty, pos, a, b, first_kid, kid_count *)
	order : int Dyn.t; (* module kind, id *)
	metas : int Dyn.t; (* owner_kind, owner, name, args_first, args_count *)
	mutable pending : (int * int * Ast.metadata) list; (* metadata waiting for a safe point to queue its arguments *)
	defines : int Dyn.t; (* name, value *)
	resources : int Dyn.t; (* name, offset, len *)
	resource_data : Buffer.t;
	queue : item Dyn.t;
	inst_ids : (path, int) Hashtbl.t;
	abstract_type_ids : (path, int) Hashtbl.t;
	enum_type_ids : (path, int) Hashtbl.t;
	typedef_ids : (path, int) Hashtbl.t;
	type_ids : (tkey, int) Hashtbl.t;
	mutable dyn_id : int;
	class_ids : (path, int) Hashtbl.t;
	class_types : tclass Dyn.t;
	class_filled : bool Dyn.t;
	field_ids : (path * bool * string, int) Hashtbl.t;
	enum_ids : (path, int) Hashtbl.t;
	enum_ctor_ids : (path * string, int) Hashtbl.t;
	abstract_ids : (path, int) Hashtbl.t;
	local_ids : (int, int) Hashtbl.t;
	pos_ids : (string * int * int, int) Hashtbl.t;
	mutable typedef_depth : int;
	mutable walking : bool;
	mutable dynamic_fields : int;
	unsupported : (string, int) Hashtbl.t;
}

let create com = {
	com;
	with_positions = Common.raw_defined com "ash_tast_positions";
	blob = Buffer.create (1 lsl 20);
	str_off = Dyn.create 0;
	str_len = Dyn.create 0;
	str_ids = Hashtbl.create 4096;
	types = Dyn.create 0;
	type_args = Dyn.create 0;
	classes = Dyn.create 0;
	fields = Dyn.create 0;
	abstracts = Dyn.create 0;
	enums = Dyn.create 0;
	enum_ctors = Dyn.create 0;
	anon_fields = Dyn.create 0;
	locals = Dyn.create 0;
	params = Dyn.create 0;
	funcs = Dyn.create 0;
	positions = Dyn.create 0;
	nodes = Dyn.create 0;
	order = Dyn.create 0;
	metas = Dyn.create 0;
	pending = [];
	defines = Dyn.create 0;
	resources = Dyn.create 0;
	resource_data = Buffer.create 1024;
	queue = Dyn.create Empty;
	inst_ids = Hashtbl.create 1024;
	abstract_type_ids = Hashtbl.create 64;
	enum_type_ids = Hashtbl.create 64;
	typedef_ids = Hashtbl.create 64;
	type_ids = Hashtbl.create 4096;
	dyn_id = -1;
	class_ids = Hashtbl.create 4096;
	class_types = Dyn.create null_class;
	class_filled = Dyn.create false;
	field_ids = Hashtbl.create 16384;
	enum_ids = Hashtbl.create 256;
	enum_ctor_ids = Hashtbl.create 1024;
	abstract_ids = Hashtbl.create 256;
	local_ids = Hashtbl.create 16384;
	pos_ids = Hashtbl.create 1024;
	typedef_depth = 0;
	walking = false;
	dynamic_fields = 0;
	unsupported = Hashtbl.create 16;
}

let none = F.none

(* A record of `stride` words appended to `table`; answers its index. *)
let new_rec table stride values =
	let id = Dyn.length table / stride in
	List.iter (Dyn.push table) values;
	id

let unsupported st what =
	let n = try Hashtbl.find st.unsupported what with Not_found -> 0 in
	Hashtbl.replace st.unsupported what (n + 1)

let str st s =
	match Hashtbl.find_opt st.str_ids s with
	| Some id -> id
	| None ->
		let id = Dyn.length st.str_off in
		Dyn.push st.str_off (Buffer.length st.blob);
		Dyn.push st.str_len (String.length s);
		Buffer.add_string st.blob s;
		Hashtbl.add st.str_ids s id;
		id

let pos_id st (p : Globals.pos) =
	let key = (p.Globals.pfile, p.Globals.pmin, p.Globals.pmax) in
	match Hashtbl.find_opt st.pos_ids key with
	| Some id -> id
	| None ->
		let id = Dyn.length st.positions / 3 in
		let file = str st p.Globals.pfile in
		Dyn.push st.positions file;
		Dyn.push st.positions p.Globals.pmin;
		Dyn.push st.positions p.Globals.pmax;
		Hashtbl.add st.pos_ids key id;
		id

(* Bit i is set when argument i is optional; bit 31 stands for every argument from the 31st on. *)
let opt_mask args =
	let m = ref 0 in
	List.iteri (fun i (_, opt, _) -> if opt then m := !m lor (1 lsl (min i 31))) args;
	!m

(* The metadata a back end reads: our own namespace, and what HashLink's generator reads. *)
let exported_meta m =
	let name = Meta.to_string m in
	(String.length name > 5 && String.sub name 0 5 = ":ash.")
	|| (match name with
		| ":hlNative" | ":struct" | ":packed" | ":coreType" | ":realPath" | ":noExpr" -> true
		| _ -> false)

(* Declaration metadata is queued later, between nodes: its arguments are nodes, and a group of
   children must stay consecutive in the queue. *)
let queue_metas st owner_kind owner (metas : Ast.metadata) =
	let keep = List.filter (fun (m, _, _) -> exported_meta m) metas in
	if keep <> [] then st.pending <- (owner_kind, owner, keep) :: st.pending

let flush_metas st =
	if st.pending <> [] then begin
		let todo = List.rev st.pending in
		st.pending <- [];
		List.iter (fun (kind, owner, metas) ->
			List.iter (fun (m, args, _) ->
				let first = Dyn.length st.queue in
				List.iter (fun a -> Dyn.push st.queue (Arg a)) args;
				List.iter (Dyn.push st.metas) [kind; owner; str st (Meta.to_string m); first; List.length args]
			) metas
		) todo
	end

let arg_list st ids =
	let first = Dyn.length st.type_args in
	List.iter (Dyn.push st.type_args) ids;
	(first, List.length ids)

(* Anonymous structure fields in the order the macro API lists them: by name. *)
let anon_field_list an =
	let l = ref [] in
	PMap.iter (fun _ cf -> l := cf :: !l) an.a_fields;
	List.rev !l

let class_id st c =
	match Hashtbl.find_opt st.class_ids c.cl_path with
	| Some id -> id
	| None ->
		let id = Dyn.length st.classes / 9 in
		for _ = 1 to 7 do Dyn.push st.classes 0 done;
		Dyn.push st.classes none;
		Dyn.push st.classes none;
		Hashtbl.add st.class_ids c.cl_path id;
		Dyn.push st.class_types c;
		Dyn.push st.class_filled false;
		id

let dynamic_type st =
	if st.dyn_id < 0 then st.dyn_id <- new_rec st.types 8 [F.Type_kind.dynamic; none; none; 0; 0; 0; 0; none];
	st.dyn_id

let rec type_id st t =
	match t with
	| TInst (c, []) ->
		(match Hashtbl.find_opt st.inst_ids c.cl_path with
		| Some id -> id
		| None ->
			let cid = class_id st c in
			let id = new_rec st.types 8 [F.Type_kind.class_; none; cid; 0; 0; 0; 0; none] in
			Hashtbl.replace st.inst_ids c.cl_path id;
			id)
	| TAbstract (a, []) ->
		(match Hashtbl.find_opt st.abstract_type_ids a.a_path with
		| Some id -> id
		| None ->
			let aid = abstract_id st a in
			let id = new_rec st.types 8 [F.Type_kind.abstract; none; aid; 0; 0; 0; 0; none] in
			Hashtbl.replace st.abstract_type_ids a.a_path id;
			abstract_via st id a [];
			id)
	| TEnum (e, []) ->
		(match Hashtbl.find_opt st.enum_type_ids e.e_path with
		| Some id -> id
		| None ->
			let eid = enum_id st e in
			let id = new_rec st.types 8 [F.Type_kind.enum; none; eid; 0; 0; 0; 0; none] in
			Hashtbl.replace st.enum_type_ids e.e_path id;
			id)
	| TDynamic _ -> dynamic_type st
	| TType (d, ps) -> typedef_id st t d ps
	| TMono r ->
		(* A monomorph nothing bound is Dynamic to every backend. *)
		(match r.tm_type with None -> dynamic_type st | Some inner -> type_id st inner)
	| TLazy f -> type_id st (lazy_type f)
	| TInst _ | TEnum _ | TAbstract _ | TFun _ | TAnon _ -> slow_type_id st t

and param_ids st ps = List.map (type_id st) ps

(* What the compiler unwraps an abstract to, with the type arguments applied; a core abstract has no
   such type, and a back end knows it by path. The record is already interned, so an abstract that
   names itself finds it. *)
and abstract_via st id a ps =
	if not (Meta.has Meta.CoreType a.a_meta) then
		Dyn.set st.types (id * 8 + 7) (type_id st (Abstract.get_underlying_type a ps))

(* A structure that is the statics of a class, an enum or an abstract is that type's object. *)
and anon_owner st an =
	match !(an.a_status) with
	| Statics c -> (F.Anon_status.statics, class_id st c)
	| EnumStatics e -> (F.Anon_status.enum_statics, enum_id st e)
	| AbstractStatics a -> (F.Anon_status.abstract_statics, abstract_id st a)
	| _ -> (0, 0)

(* The structure of a type with arguments, a function or a structure. *)
and slow_type_id st t =
	match t with
	| TInst (c, ps) ->
		let args = param_ids st ps in
		let key = KInst (c.cl_path, args) in
		(match Hashtbl.find_opt st.type_ids key with
		| Some id -> id
		| None ->
			let first, count = arg_list st args in
			let cid = class_id st c in
			let id = new_rec st.types 8 [F.Type_kind.class_; none; cid; first; count; 0; 0; none] in
			Hashtbl.replace st.type_ids key id;
			id)
	| TEnum (e, ps) ->
		let args = param_ids st ps in
		let key = KEnum (e.e_path, args) in
		(match Hashtbl.find_opt st.type_ids key with
		| Some id -> id
		| None ->
			let first, count = arg_list st args in
			let eid = enum_id st e in
			let id = new_rec st.types 8 [F.Type_kind.enum; none; eid; first; count; 0; 0; none] in
			Hashtbl.replace st.type_ids key id;
			id)
	| TAbstract (a, ps) ->
		let args = param_ids st ps in
		let key = KAbstract (a.a_path, args) in
		(match Hashtbl.find_opt st.type_ids key with
		| Some id -> id
		| None ->
			let first, count = arg_list st args in
			let aid = abstract_id st a in
			let id = new_rec st.types 8 [F.Type_kind.abstract; none; aid; first; count; 0; 0; none] in
			Hashtbl.replace st.type_ids key id;
			abstract_via st id a ps;
			id)
	| TFun (args, ret) ->
		let ids = List.map (fun (_, _, t) -> type_id st t) args in
		let r = type_id st ret in
		let mask = opt_mask args in
		let key = KFun (ids, r, mask) in
		(match Hashtbl.find_opt st.type_ids key with
		| Some id -> id
		| None ->
			let first, count = arg_list st ids in
			let id = new_rec st.types 8 [F.Type_kind.fun_; none; r; first; count; mask; 0; none] in
			Hashtbl.replace st.type_ids key id;
			id)
	| TAnon an ->
		let fs = anon_field_list an in
		let ids = List.map (fun cf -> type_id st cf.cf_type) fs in
		let status, owner = anon_owner st an in
		let key = KAnon (List.map2 (fun cf id -> (cf.cf_name, id)) fs ids, status, owner) in
		(match Hashtbl.find_opt st.type_ids key with
		| Some id -> id
		| None ->
			let first = Dyn.length st.anon_fields / 3 in
			List.iter2 (fun cf id ->
				Dyn.push st.anon_fields (str st cf.cf_name);
				Dyn.push st.anon_fields id;
				Dyn.push st.anon_fields 0
			) fs ids;
			let id = new_rec st.types 8 [F.Type_kind.anon; none; first; owner; List.length fs; 0; status; none] in
			Hashtbl.replace st.type_ids key id;
			id)
	| _ ->
		unsupported st "type";
		dynamic_type st

(* A type reached through a typedef, cached under the typedef's name. *)
and typedef_id st t d ps =
	let cached = if ps = [] then Hashtbl.find_opt st.typedef_ids d.t_path else None in
	match cached with
	| Some id -> id
	| None ->
		(* A typedef that instantiates itself at ever larger types has no finite table; cut it. *)
		if st.typedef_depth > 8 then begin
			unsupported st ("infinitely expanding typedef " ^ Globals.s_type_path d.t_path);
			dynamic_type st
		end else begin
			st.typedef_depth <- st.typedef_depth + 1;
			let id = typedef_id_at st t d ps in
			st.typedef_depth <- st.typedef_depth - 1;
			id
		end

and typedef_id_at st t d ps =
	let args = param_ids st ps in
	let key = KTypedef (d.t_path, args) in
	match Hashtbl.find_opt st.type_ids key with
	| Some id -> id
	| None ->
		let next = follow_once t in
		(match next with
		| TAnon an ->
			(* Given its id before its fields are read, so a structure that names itself finds it. *)
			let status, owner = anon_owner st an in
			let id = new_rec st.types 8 [F.Type_kind.anon; none; 0; owner; 0; 0; status; none] in
			Hashtbl.replace st.type_ids key id;
			if ps = [] then Hashtbl.replace st.typedef_ids d.t_path id;
			let first = fill_anon st an id in
			Dyn.set st.types (id * 8 + 2) first;
			id
		| TInst (_, _ :: _) | TEnum (_, _ :: _) | TAbstract (_, _ :: _) | TFun _ ->
			(* Reserved before the arguments are read: a typedef whose expansion names the
			   typedef again, such as `typedef C = Map<Int, {next:C}>`, finds this id. *)
			let kind = match next with
				| TInst _ -> F.Type_kind.class_
				| TEnum _ -> F.Type_kind.enum
				| TAbstract _ -> F.Type_kind.abstract
				| _ -> F.Type_kind.fun_
			in
			let id = new_rec st.types 8 [kind; none; none; 0; 0; 0; 0; none] in
			Hashtbl.replace st.type_ids key id;
			if ps = [] then Hashtbl.replace st.typedef_ids d.t_path id;
			let def, arg_types, mask = match next with
				| TInst (c, ps2) -> (class_id st c, ps2, 0)
				| TEnum (e, ps2) -> (enum_id st e, ps2, 0)
				| TAbstract (a, ps2) -> (abstract_id st a, ps2, 0)
				| TFun (args2, ret) -> (type_id st ret, List.map (fun (_, _, t) -> t) args2, opt_mask args2)
				| _ -> (none, [], 0)
			in
			let first, count = arg_list st (param_ids st arg_types) in
			Dyn.set st.types (id * 8 + 2) def;
			Dyn.set st.types (id * 8 + 3) first;
			Dyn.set st.types (id * 8 + 4) count;
			Dyn.set st.types (id * 8 + 5) mask;
			(match next with TAbstract (a, ps2) -> abstract_via st id a ps2 | _ -> ());
			id
		| _ ->
			let id = type_id st next in
			Hashtbl.replace st.type_ids key id;
			if ps = [] then Hashtbl.replace st.typedef_ids d.t_path id;
			id)

and fill_anon st an id =
	let fs = anon_field_list an in
	let first = Dyn.length st.anon_fields / 3 in
	(* Reserve the records, then read the types: a field type can reach this structure again. *)
	List.iter (fun cf ->
		Dyn.push st.anon_fields (str st cf.cf_name);
		Dyn.push st.anon_fields 0;
		Dyn.push st.anon_fields 0
	) fs;
	Dyn.set st.types (id * 8 + 4) (List.length fs);
	List.iteri (fun i cf -> Dyn.set st.anon_fields ((first + i) * 3 + 1) (type_id st cf.cf_type)) fs;
	first

and enum_id st e =
	match Hashtbl.find_opt st.enum_ids e.e_path with
	| Some id -> id
	| None ->
		let key = Globals.s_type_path e.e_path in
		let id = new_rec st.enums 5 [str st key; 0; 0; 0; none] in
		Hashtbl.replace st.enum_ids e.e_path id;
		queue_metas st F.Meta_owner.enum id e.e_meta;
		let first = Dyn.length st.enum_ctors / 6 in
		List.iter (fun name ->
			let ef = PMap.find name e.e_constrs in
			let ctor = new_rec st.enum_ctors 6 [str st name; id; ef.ef_index; 0; 0; 0] in
			queue_metas st F.Meta_owner.enum_ctor ctor ef.ef_meta;
			Hashtbl.replace st.enum_ctor_ids (e.e_path, name) ctor
		) e.e_names;
		Dyn.set st.enums (id * 5 + 1) first;
		Dyn.set st.enums (id * 5 + 2) (List.length e.e_names);
		(* Types last: they may name this enum again. *)
		List.iter (fun name ->
			let ef = PMap.find name e.e_constrs in
			let ctor = Hashtbl.find st.enum_ctor_ids (e.e_path, name) in
			Dyn.set st.enum_ctors (ctor * 6 + 3) (type_id st ef.ef_type);
			match follow ef.ef_type with
			| TFun (args, _) ->
				let first, count = arg_list st (List.map (fun (_, _, t) -> type_id st t) args) in
				Dyn.set st.enum_ctors (ctor * 6 + 4) first;
				Dyn.set st.enum_ctors (ctor * 6 + 5) count
			| _ -> ()
		) e.e_names;
		id

and abstract_id st a =
	match Hashtbl.find_opt st.abstract_ids a.a_path with
	| Some id -> id
	| None ->
		let id = new_rec st.abstracts 4 [str st (Globals.s_type_path a.a_path); none; none; 0] in
		Hashtbl.replace st.abstract_ids a.a_path id;
		queue_metas st F.Meta_owner.abstract id a.a_meta;
		Dyn.set st.abstracts (id * 4 + 1) (type_id st a.a_this);
		(match a.a_impl with Some c -> Dyn.set st.abstracts (id * 4 + 2) (class_id st c) | None -> ());
		id

let local_id st v =
	match Hashtbl.find_opt st.local_ids v.v_id with
	| Some id -> id
	| None ->
		let flags = if has_var_flag v VCaptured then F.Local_flag.captured else 0 in
		let id = new_rec st.locals 3 [str st v.v_name; 0; flags] in
		Hashtbl.replace st.local_ids v.v_id id;
		queue_metas st F.Meta_owner.local id v.v_meta;
		Dyn.set st.locals (id * 3 + 1) (type_id st v.v_type);
		id

(* ---- functions and the node queue *)

let enqueue_root st e =
	Dyn.push st.queue (Expr e);
	Dyn.length st.queue - 1

let func_id st tf =
	let ps = List.map (fun (v, _) -> local_id st v) tf.tf_args in
	let first = Dyn.length st.params / 2 in
	List.iter2 (fun l (_, default) ->
		Dyn.push st.params l;
		Dyn.push st.params (match default with None -> none | Some d -> enqueue_root st d)
	) ps tf.tf_args;
	let ty = type_id st tf.tf_type in
	let id = new_rec st.funcs 6 [ty; first; List.length ps; 0; 0; none] in
	Dyn.set st.funcs (id * 6 + 3) (enqueue_root st tf.tf_expr);
	id

(* An expression run once, as a function with no parameters: a variable's initializer, an `__init__` block. *)
let body_func st e =
	let ty = type_id st e.etype in
	let root = enqueue_root st e in
	new_rec st.funcs 6 [ty; 0; 0; root; 0; none]

let func_of_field st cf =
	match cf.cf_expr with
	| None -> none
	| Some e ->
		(match e.eexpr with
		| TFunction tf -> func_id st tf
		| _ -> body_func st e)

let field_flags cf is_static =
	let flags = ref (if is_static then F.Field_flag.static else 0) in
	if has_class_field_flag cf CfPublic then flags := !flags lor F.Field_flag.public;
	if has_class_field_flag cf CfFinal then flags := !flags lor F.Field_flag.final;
	(match cf.cf_kind with
	| Method m ->
		flags := !flags lor F.Field_flag.method_;
		if m = MethInline then flags := !flags lor F.Field_flag.inline
	| _ -> ());
	if not (is_physical_field cf) then flags := !flags lor F.Field_flag.not_physical;
	if has_class_field_flag cf CfExtern then flags := !flags lor F.Field_flag.extern;
	(match cf.cf_kind with Method MethDynamic -> flags := !flags lor F.Field_flag.dynamic | _ -> ());
	!flags

(* Fill a class's fields, once. *)
let fill_class st id =
	if not (Dyn.get st.class_filled id) then begin
		Dyn.set st.class_filled id true;
		let c = Dyn.get st.class_types id in
		ignore (c.cl_build ());
		let path = c.cl_path in
		let flags = ref 0 in
		if has_class_flag c CInterface then flags := !flags lor F.Class_flag.interface;
		if has_class_flag c CFinal then flags := !flags lor F.Class_flag.final;
		if has_class_flag c CExtern then flags := !flags lor F.Class_flag.extern;
		if has_class_flag c CAbstract then flags := !flags lor F.Class_flag.abstract;
		(match c.cl_kind with
		| KTypeParameter _ -> flags := !flags lor F.Class_flag.type_param
		| KExpr _ -> flags := !flags lor F.Class_flag.const_arg
		| _ -> ());
		let base = id * 9 in
		queue_metas st F.Meta_owner.class_ id c.cl_meta;
		Dyn.set st.classes base (str st (Globals.s_type_path path));
		Dyn.set st.classes (base + 1)
			(match c.cl_super with None -> none | Some (sc, ps) -> type_id st (TInst (sc, ps)));
		Dyn.set st.classes (base + 2) !flags;
		(* A type parameter's constraints take the place of the interfaces. *)
		let ifaces = match c.cl_kind with
			| KTypeParameter constraints -> List.map (type_id st) constraints
			| _ -> List.map (fun (ic, ps) -> type_id st (TInst (ic, ps))) c.cl_implements
		in
		let ifirst, icount = arg_list st ifaces in
		Dyn.set st.classes (base + 5) ifirst;
		Dyn.set st.classes (base + 6) icount;

		let all =
			(match c.cl_constructor with Some cf -> [(cf, false)] | None -> [])
			@ List.map (fun cf -> (cf, false)) c.cl_ordered_fields
			@ List.map (fun cf -> (cf, true)) c.cl_ordered_statics
		in
		let first = Dyn.length st.fields / 6 in
		(* Reserve the records, so a field type that reaches this class again finds its fields in place. *)
		List.iter (fun (cf, is_static) ->
			let fid = new_rec st.fields 6 [str st cf.cf_name; 0; id; field_flags cf is_static; none; none] in
			queue_metas st F.Meta_owner.field fid cf.cf_meta;
			Hashtbl.replace st.field_ids (path, is_static, cf.cf_name) fid
		) all;
		Dyn.set st.classes (base + 3) first;
		Dyn.set st.classes (base + 4) (List.length all);
		List.iteri (fun i (cf, _) ->
			let fid = first + i in
			Dyn.set st.fields (fid * 6 + 1) (type_id st cf.cf_type);
			Dyn.set st.fields (fid * 6 + 4) (func_of_field st cf)
		) all;
		(match c.cl_init with
		| Some e -> Dyn.set st.classes (base + 7) (body_func st e)
		| None -> ())
	end

(* The id of the field `name` of `c` or of the nearest superclass that declares it. *)
let find_field st c name is_static =
	let rec go cur =
		match Hashtbl.find_opt st.field_ids (cur.cl_path, is_static, name) with
		| Some fid -> fid
		| None ->
			let cid = class_id st cur in
			if not (Dyn.get st.class_filled cid) then begin
				(* Filling queues the class's bodies, which cannot happen between a node's children. *)
				if st.walking then none
				else begin
					fill_class st cid;
					match Hashtbl.find_opt st.field_ids (cur.cl_path, is_static, name) with
					| Some fid -> fid
					| None -> up cur
				end
			end else up cur
	and up cur = match cur.cl_super with None -> none | Some (sc, _) -> go sc in
	go c

let module_index st m =
	match m with
	| TClassDecl c -> (F.Module_kind.class_, class_id st c)
	| TEnumDecl e -> (F.Module_kind.enum, enum_id st e)
	| TAbstractDecl a -> (F.Module_kind.abstract, abstract_id st a)
	| TTypeDecl _ ->
		unsupported st "module type typedef";
		(F.Module_kind.class_, 0)

(* Which enum an enum constructor belongs to, from the type of the value it is read from. *)
let enum_ctor_id st from ef =
	match follow from.etype with
	| TEnum (e, _) ->
		ignore (enum_id st e);
		(match Hashtbl.find_opt st.enum_ctor_ids (e.e_path, ef.ef_name) with
		| Some id -> id
		| None -> unsupported st "enum constructor"; 0)
	| _ ->
		unsupported st "enum constructor";
		0

let binop_index = function
	| Ast.OpAdd -> 0 | Ast.OpMult -> 1 | Ast.OpDiv -> 2 | Ast.OpSub -> 3 | Ast.OpAssign -> 4
	| Ast.OpEq -> 5 | Ast.OpNotEq -> 6 | Ast.OpGt -> 7 | Ast.OpGte -> 8 | Ast.OpLt -> 9
	| Ast.OpLte -> 10 | Ast.OpAnd -> 11 | Ast.OpOr -> 12 | Ast.OpXor -> 13 | Ast.OpBoolAnd -> 14
	| Ast.OpBoolOr -> 15 | Ast.OpShl -> 16 | Ast.OpShr -> 17 | Ast.OpUShr -> 18 | Ast.OpMod -> 19
	| Ast.OpAssignOp _ -> 20 | Ast.OpInterval -> 21 | Ast.OpArrow -> 22 | Ast.OpIn -> 23
	| Ast.OpNullCoal -> 24

let unop_index = function
	| Ast.Increment -> 0 | Ast.Decrement -> 1 | Ast.Not -> 2 | Ast.Neg -> 3 | Ast.NegBits -> 4 | Ast.Spread -> 5

let write_node st kind flags ty pos a b first count =
	List.iter (Dyn.push st.nodes) [kind; flags; ty; pos; a; b; first; count]

let push_kid st e = Dyn.push st.queue (Expr e)
let push_pseudo_items st kind a kids = Dyn.push st.queue (Pseudo { pkind = kind; pa = a; kids })
let push_pseudo st kind a kids = push_pseudo_items st kind a (List.map (fun e -> Expr e) kids)

(* Write node `e` of the queue and enqueue its children. *)
let visit st e =
	let kind = ref 0 and flags = ref 0 and a = ref 0 and b = ref 0 in
	(* Children are queued as they are met, as one group, so this is where it starts. *)
	let first = ref (Dyn.length st.queue) in
	let dynamic_field name =
		a := F.Field_access.dynamic;
		b := str st name;
		st.dynamic_fields <- st.dynamic_fields + 1
	in
	(match e.eexpr with
	| TConst c ->
		kind := F.Node_kind.const;
		(match c with
		| TInt i -> flags := F.Const_kind.int; a := Int32.to_int i
		| TFloat s -> flags := F.Const_kind.float; a := str st s
		| TString s -> flags := F.Const_kind.string; a := str st s
		| TBool v -> flags := F.Const_kind.bool; a := if v then 1 else 0
		| TNull -> flags := F.Const_kind.null
		| TThis -> flags := F.Const_kind.this
		| TSuper -> flags := F.Const_kind.super)
	| TLocal v ->
		kind := F.Node_kind.local;
		a := local_id st v
	| TArray (e1, e2) ->
		kind := F.Node_kind.array;
		push_kid st e1;
		push_kid st e2
	| TBinop (op, e1, e2) ->
		kind := F.Node_kind.binop;
		a := binop_index op;
		(match op with Ast.OpAssignOp inner -> b := binop_index inner | _ -> ());
		push_kid st e1;
		push_kid st e2
	| TField (obj, fa) ->
		kind := F.Node_kind.field;
		(match fa with
		| FInstance (c, _, cf) ->
			a := F.Field_access.instance;
			b := find_field st c cf.cf_name false;
			if !b = none then dynamic_field cf.cf_name
		| FStatic (c, cf) ->
			a := F.Field_access.static;
			b := find_field st c cf.cf_name true;
			if !b = none then dynamic_field cf.cf_name
		| FClosure (co, cf) ->
			(match co with
			| None -> dynamic_field cf.cf_name
			| Some (c, _) ->
				a := F.Field_access.closure;
				b := find_field st c cf.cf_name false;
				if !b = none then dynamic_field cf.cf_name)
		| FAnon cf ->
			a := F.Field_access.anon;
			b := str st cf.cf_name
		| FDynamic s ->
			a := F.Field_access.dynamic;
			b := str st s
		| FEnum (en, ef) ->
			a := F.Field_access.enum;
			ignore (enum_id st en);
			b := Hashtbl.find st.enum_ctor_ids (en.e_path, ef.ef_name));
		push_kid st obj
	| TTypeExpr m ->
		kind := F.Node_kind.type_expr;
		let k, i = module_index st m in
		flags := k;
		a := i
	| TParenthesis e1 ->
		kind := F.Node_kind.parenthesis;
		push_kid st e1
	| TObjectDecl fs ->
		kind := F.Node_kind.object_decl;
		List.iter (fun ((name, _, _), value) -> push_pseudo st F.Node_kind.object_field (str st name) [value]) fs
	| TArrayDecl el ->
		kind := F.Node_kind.array_decl;
		List.iter (push_kid st) el
	| TCall (callee, el) ->
		kind := F.Node_kind.call;
		push_kid st callee;
		List.iter (push_kid st) el
	| TNew (c, _, el) ->
		kind := F.Node_kind.new_;
		a := class_id st c;
		List.iter (push_kid st) el
	| TUnop (op, flag, e1) ->
		kind := F.Node_kind.unop;
		a := unop_index op;
		if flag = Ast.Postfix then flags := F.Node_flag.postfix;
		push_kid st e1
	| TFunction tf ->
		kind := F.Node_kind.function_;
		a := func_id st tf;
		first := Dyn.length st.queue
	| TVar (v, init) ->
		kind := F.Node_kind.var;
		a := local_id st v;
		(match init with
		| Some i -> flags := F.Node_flag.has_init; push_kid st i
		| None -> ())
	| TBlock el ->
		kind := F.Node_kind.block;
		List.iter (push_kid st) el
	| TFor (v, it, body) ->
		kind := F.Node_kind.for_;
		a := local_id st v;
		push_kid st it;
		push_kid st body
	| TIf (cond, t, els) ->
		kind := F.Node_kind.if_;
		push_kid st cond;
		push_kid st t;
		(match els with Some x -> flags := F.Node_flag.has_else; push_kid st x | None -> ())
	| TWhile (cond, body, flag) ->
		kind := F.Node_kind.while_;
		if flag <> Ast.NormalWhile then flags := F.Node_flag.do_while;
		push_kid st cond;
		push_kid st body
	| TSwitch (subject, cases, edef) ->
		kind := F.Node_kind.switch;
		push_kid st subject;
		List.iter (fun (values, body) -> push_pseudo st F.Node_kind.case (List.length values) (values @ [body])) cases;
		(match edef with
		| Some d -> flags := F.Node_flag.has_default; push_pseudo st F.Node_kind.default 0 [d]
		| None -> ())
	| TTry (body, catches) ->
		kind := F.Node_kind.try_;
		push_kid st body;
		List.iter (fun (v, handler) -> push_pseudo st F.Node_kind.catch (local_id st v) [handler]) catches
	| TReturn e1 ->
		kind := F.Node_kind.return;
		(match e1 with Some x -> flags := F.Node_flag.has_value; push_kid st x | None -> ())
	| TBreak -> kind := F.Node_kind.break
	| TContinue -> kind := F.Node_kind.continue
	| TThrow e1 ->
		kind := F.Node_kind.throw;
		push_kid st e1
	| TCast (e1, m) ->
		kind := F.Node_kind.cast;
		(match m with
		| Some m ->
			let k, i = module_index st m in
			flags := F.Node_flag.has_type lor (k lsl 1);
			a := i
		| None -> ());
		push_kid st e1
	| TMeta ((m, args, _), e1) ->
		kind := F.Node_kind.meta;
		a := str st (Meta.to_string m);
		push_kid st e1;
		if exported_meta m then List.iter (fun x -> Dyn.push st.queue (Arg x)) args
	| TEnumParameter (e1, ef, index) ->
		kind := F.Node_kind.enum_parameter;
		a := enum_ctor_id st e1 ef;
		b := index;
		push_kid st e1
	| TEnumIndex e1 ->
		kind := F.Node_kind.enum_index;
		push_kid st e1
	| TIdent s ->
		kind := F.Node_kind.ident;
		a := str st s);
	let ty = type_id st e.etype in
	let pos = if st.with_positions then pos_id st e.epos else none in
	write_node st !kind !flags ty pos !a !b !first (Dyn.length st.queue - !first)

let visit_pseudo st p =
	let first = Dyn.length st.queue in
	List.iter (Dyn.push st.queue) p.kids;
	write_node st p.pkind 0 none none p.pa 0 first (List.length p.kids)

(* A metadata argument, an untyped expression: constants, identifiers, arrays and objects. *)
let visit_arg st (arg : Ast.expr) =
	let kind = ref F.Node_kind.ident and flags = ref 0 and a = ref 0 in
	let first = Dyn.length st.queue in
	let const k v = kind := F.Node_kind.const; flags := k; a := v in
	let ident text = kind := F.Node_kind.ident; a := str st text in
	let rec dotted ((def, _) : Ast.expr) =
		match def with
		| Ast.EConst (Ast.Ident s) -> Some s
		| Ast.EField (inner, name, _) -> Option.map (fun p -> p ^ "." ^ name) (dotted inner)
		| _ -> None
	in
	let int_const negate text =
		match Int32.of_string_opt ((if negate then "-" else "") ^ text) with
		| Some i -> const F.Const_kind.int (Int32.to_int i)
		| None -> unsupported st "meta argument"; ident text
	in
	let rec go ((def, _) as e : Ast.expr) =
		match def with
		| Ast.EConst (Ast.Int (s, _)) -> int_const false s
		| Ast.EConst (Ast.Float (s, _)) -> const F.Const_kind.float (str st s)
		| Ast.EConst (Ast.String (s, _)) -> const F.Const_kind.string (str st s)
		| Ast.EConst (Ast.Ident "true") -> const F.Const_kind.bool 1
		| Ast.EConst (Ast.Ident "false") -> const F.Const_kind.bool 0
		| Ast.EConst (Ast.Ident "null") -> const F.Const_kind.null 0
		| Ast.EConst (Ast.Ident s) -> ident s
		| Ast.EConst (Ast.Regexp (s, _)) -> unsupported st "meta argument"; ident s
		| Ast.EParenthesis inner -> go inner
		| Ast.EUnop (Ast.Neg, Ast.Prefix, (Ast.EConst (Ast.Int (s, _)), _)) -> int_const true s
		| Ast.EUnop (Ast.Neg, Ast.Prefix, (Ast.EConst (Ast.Float (s, _)), _)) -> const F.Const_kind.float (str st ("-" ^ s))
		| Ast.EArrayDecl l ->
			kind := F.Node_kind.array_decl;
			List.iter (fun x -> Dyn.push st.queue (Arg x)) l
		| Ast.EObjectDecl fields ->
			kind := F.Node_kind.object_decl;
			List.iter (fun ((name, _, _), v) -> push_pseudo_items st F.Node_kind.object_field (str st name) [Arg v]) fields
		| Ast.EField _ ->
			(match dotted e with
			| Some p -> ident p
			| None -> unsupported st "meta argument"; ident "?")
		| _ -> unsupported st "meta argument"; ident "?"
	in
	go arg;
	write_node st !kind !flags none none !a 0 first (Dyn.length st.queue - first)

(* The program's entry: a function with no parameters whose body is the whole main expression. *)
let entry_func st =
	match st.com.Common.main with
	| None -> none
	| Some m ->
		let ty = type_id st m.etype in
		let root = enqueue_root st m in
		new_rec st.funcs 6 [ty; 0; 0; root; 0; none]

(* ---- writing *)

(* `major * 10000 + minor * 100 + patch` from the compiler's version. *)
let haxe_version () =
	Globals.version_major * 10000 + Globals.version_minor * 100 + Globals.version_revision

let assemble st entry =
	let len d = Dyn.length d in
	let counts = [|
		Buffer.length st.blob; len st.str_off; len st.positions / 3; len st.types / 8; len st.type_args;
		len st.classes / 9; len st.fields / 6; len st.abstracts / 4; len st.enums / 5;
		len st.enum_ctors / 6; len st.anon_fields / 3; len st.locals / 3; len st.params / 2;
		len st.funcs / 6; len st.nodes / 8; 0; 0; 0;
		len st.order / 2; len st.metas / 5; len st.defines / 2; len st.resources / 3;
		Buffer.length st.resource_data;
	|] in
	let elems = [|
		F.strings_elem; F.string_index_elem; F.positions_elem; F.types_elem; F.type_args_elem;
		F.classes_elem; F.fields_elem; F.abstracts_elem; F.enums_elem; F.enum_ctors_elem;
		F.anon_fields_elem; F.locals_elem; F.params_elem; F.funcs_elem; F.nodes_elem;
		F.keys_elem; F.kept_elem; F.parents_elem; F.order_elem; F.metas_elem; F.defines_elem;
		F.resources_elem; F.resource_data_elem;
	|] in
	let offsets = Array.make F.section_count 0 in
	let at = ref (F.header_size + F.section_count * F.section_rec_size) in
	for s = 0 to F.section_count - 1 do
		offsets.(s) <- !at;
		at := !at + counts.(s) * elems.(s)
	done;
	let out = Bytes.make !at '\000' in
	F.write_header out 0 ~magic:F.magic ~version:F.version ~schema_hash_lo:F.schema_hash_lo
		~schema_hash_hi:F.schema_hash_hi ~haxe_version:(haxe_version ()) ~layer_id_lo:0 ~layer_id_hi:0
		~flags:0 ~entry ~section_count:F.section_count;
	for s = 0 to F.section_count - 1 do
		F.write_section_rec out (F.header_size + s * F.section_rec_size) ~offset:offsets.(s) ~count:counts.(s)
	done;
	Bytes.blit_string (Buffer.contents st.blob) 0 out offsets.(F.section_strings) (Buffer.length st.blob);
	let g d i = Dyn.get d i in
	for i = 0 to counts.(F.section_string_index) - 1 do
		F.write_str_rec out (offsets.(F.section_string_index) + i * F.str_rec_size)
			~offset:(g st.str_off i) ~len:(g st.str_len i)
	done;
	for i = 0 to counts.(F.section_positions) - 1 do
		let r = i * 3 in
		F.write_pos_rec out (offsets.(F.section_positions) + i * F.pos_rec_size)
			~file:(g st.positions r) ~start:(g st.positions (r + 1)) ~end_:(g st.positions (r + 2))
	done;
	for i = 0 to counts.(F.section_types) - 1 do
		let r = i * 8 in
		F.write_type_rec out (offsets.(F.section_types) + i * F.type_rec_size)
			~kind:(g st.types r) ~flags:(g st.types (r + 6)) ~name:(g st.types (r + 1)) ~def:(g st.types (r + 2))
			~args_first:(g st.types (r + 3)) ~args_count:(g st.types (r + 4)) ~opt_mask:(g st.types (r + 5))
			~via:(g st.types (r + 7))
	done;
	for i = 0 to Dyn.length st.type_args - 1 do
		Bytes.set_int32_le out (offsets.(F.section_type_args) + i * 4) (Int32.of_int (g st.type_args i))
	done;
	for i = 0 to counts.(F.section_classes) - 1 do
		let r = i * 9 and c = st.classes in
		F.write_class_rec out (offsets.(F.section_classes) + i * F.class_rec_size)
			~name:(g c r) ~super_type:(g c (r + 1)) ~flags:(g c (r + 2)) ~fields_first:(g c (r + 3))
			~fields_count:(g c (r + 4)) ~interfaces_first:(g c (r + 5)) ~interfaces_count:(g c (r + 6))
			~init_func:(g c (r + 7)) ~meta_root:(g c (r + 8))
	done;
	for i = 0 to counts.(F.section_fields) - 1 do
		let r = i * 6 and f = st.fields in
		F.write_field_rec out (offsets.(F.section_fields) + i * F.field_rec_size)
			~name:(g f r) ~ty:(g f (r + 1)) ~owner:(g f (r + 2)) ~flags:(g f (r + 3))
			~func:(g f (r + 4)) ~pos:(g f (r + 5))
	done;
	for i = 0 to counts.(F.section_abstracts) - 1 do
		let r = i * 4 and a = st.abstracts in
		F.write_abstract_rec out (offsets.(F.section_abstracts) + i * F.abstract_rec_size)
			~name:(g a r) ~underlying:(g a (r + 1)) ~impl_class:(g a (r + 2)) ~flags:(g a (r + 3))
	done;
	for i = 0 to counts.(F.section_enums) - 1 do
		let r = i * 5 and e = st.enums in
		F.write_enum_rec out (offsets.(F.section_enums) + i * F.enum_rec_size)
			~name:(g e r) ~ctors_first:(g e (r + 1)) ~ctors_count:(g e (r + 2)) ~flags:(g e (r + 3))
			~meta_root:(g e (r + 4))
	done;
	for i = 0 to counts.(F.section_enum_ctors) - 1 do
		let r = i * 6 and c = st.enum_ctors in
		F.write_enum_ctor_rec out (offsets.(F.section_enum_ctors) + i * F.enum_ctor_rec_size)
			~name:(g c r) ~enum_id:(g c (r + 1)) ~index:(g c (r + 2)) ~ty:(g c (r + 3))
			~args_first:(g c (r + 4)) ~args_count:(g c (r + 5))
	done;
	for i = 0 to counts.(F.section_anon_fields) - 1 do
		let r = i * 3 and a = st.anon_fields in
		F.write_anon_field_rec out (offsets.(F.section_anon_fields) + i * F.anon_field_rec_size)
			~name:(g a r) ~ty:(g a (r + 1)) ~flags:(g a (r + 2))
	done;
	for i = 0 to counts.(F.section_locals) - 1 do
		let r = i * 3 and l = st.locals in
		F.write_local_rec out (offsets.(F.section_locals) + i * F.local_rec_size)
			~name:(g l r) ~ty:(g l (r + 1)) ~flags:(g l (r + 2))
	done;
	for i = 0 to counts.(F.section_params) - 1 do
		F.write_param_rec out (offsets.(F.section_params) + i * F.param_rec_size)
			~local:(g st.params (i * 2)) ~default_value:(g st.params (i * 2 + 1))
	done;
	for i = 0 to counts.(F.section_funcs) - 1 do
		let r = i * 6 and f = st.funcs in
		F.write_func_rec out (offsets.(F.section_funcs) + i * F.func_rec_size)
			~ret:(g f r) ~params_first:(g f (r + 1)) ~params_count:(g f (r + 2)) ~root:(g f (r + 3))
			~flags:(g f (r + 4)) ~pos:(g f (r + 5))
	done;
	for i = 0 to counts.(F.section_nodes) - 1 do
		let r = i * 8 and n = st.nodes in
		F.write_node_rec out (offsets.(F.section_nodes) + i * F.node_rec_size)
			~kind:(g n r) ~flags:(g n (r + 1)) ~ty:(g n (r + 2)) ~pos:(g n (r + 3)) ~a:(g n (r + 4))
			~b:(g n (r + 5)) ~first_kid:(g n (r + 6)) ~kid_count:(g n (r + 7))
	done;
	for i = 0 to counts.(F.section_order) - 1 do
		F.write_order_rec out (offsets.(F.section_order) + i * F.order_rec_size)
			~kind:(g st.order (i * 2)) ~id:(g st.order (i * 2 + 1))
	done;
	for i = 0 to counts.(F.section_metas) - 1 do
		let r = i * 5 and m = st.metas in
		F.write_meta_rec out (offsets.(F.section_metas) + i * F.meta_rec_size)
			~owner_kind:(g m r) ~owner:(g m (r + 1)) ~name:(g m (r + 2)) ~args_first:(g m (r + 3))
			~args_count:(g m (r + 4))
	done;
	for i = 0 to counts.(F.section_defines) - 1 do
		F.write_define_rec out (offsets.(F.section_defines) + i * F.define_rec_size)
			~name:(g st.defines (i * 2)) ~value:(g st.defines (i * 2 + 1))
	done;
	for i = 0 to counts.(F.section_resources) - 1 do
		let r = i * 3 and x = st.resources in
		F.write_resource_rec out (offsets.(F.section_resources) + i * F.resource_rec_size)
			~name:(g x r) ~offset:(g x (r + 1)) ~len:(g x (r + 2))
	done;
	Bytes.blit_string (Buffer.contents st.resource_data) 0 out offsets.(F.section_resource_data)
		(Buffer.length st.resource_data);
	out

(* The kind and path of a type, which orders the types the compiler hands over. *)
let sort_key = function
	| TClassDecl c -> "1" ^ Globals.s_type_path c.cl_path
	| TEnumDecl e -> "2" ^ Globals.s_type_path e.e_path
	| TAbstractDecl a -> "3" ^ Globals.s_type_path a.a_path
	| TTypeDecl _ -> "4"

let run com path =
	let t0 = Unix.gettimeofday () in
	let st = create com in
	let verbose = Common.raw_defined com "ash_tast_verbose" in
	(* Sorted, so the file does not depend on the order the compiler met the modules in. *)
	let keyed = List.map (fun mt -> (sort_key mt, mt)) com.Common.types in
	let all = List.map snd (List.stable_sort (fun (a, _) (b, _) -> compare a b) keyed) in
	List.iter (function
		| TClassDecl c -> fill_class st (class_id st c)
		| TEnumDecl e -> ignore (enum_id st e)
		| TAbstractDecl a -> ignore (abstract_id st a)
		| TTypeDecl _ -> ()
	) all;
	(* Referenced classes the list did not carry (type parameters, externs) are filled too. *)
	let filled = ref 0 in
	while !filled < Dyn.length st.class_types do
		fill_class st !filled;
		incr filled
	done;
	(* The compiler's own order: a class after what its static initializers reach. *)
	List.iter (function
		| TClassDecl c -> List.iter (Dyn.push st.order) [F.Module_kind.class_; class_id st c]
		| TEnumDecl e -> List.iter (Dyn.push st.order) [F.Module_kind.enum; enum_id st e]
		| TAbstractDecl a -> List.iter (Dyn.push st.order) [F.Module_kind.abstract; abstract_id st a]
		| TTypeDecl _ -> ()
	) com.Common.types;
	(* The runtime metadata object that reflection reads, as an expression. *)
	let runtime_meta mt =
		match (try Texpr.build_metadata com.Common.basic mt with _ -> unsupported st "runtime metadata"; None) with
		| Some e -> enqueue_root st e
		| None -> none
	in
	List.iter (function
		| TClassDecl c as mt -> Dyn.set st.classes (class_id st c * 9 + 8) (runtime_meta mt)
		| TEnumDecl e as mt -> Dyn.set st.enums (enum_id st e * 5 + 4) (runtime_meta mt)
		| _ -> ()
	) all;
	PMap.iter (fun name value -> List.iter (Dyn.push st.defines) [str st name; str st value])
		com.Common.defines.Define.values;
	let resources = List.sort compare (Hashtbl.fold (fun name data acc -> (name, data) :: acc) com.Common.resources []) in
	List.iter (fun (name, data) ->
		List.iter (Dyn.push st.resources) [str st name; Buffer.length st.resource_data; String.length data];
		Buffer.add_string st.resource_data data
	) resources;
	flush_metas st;
	let entry = entry_func st in
	let i = ref 0 in
	let continue = ref true in
	while !continue do
		st.walking <- true;
		while !i < Dyn.length st.queue do
			(match Dyn.get st.queue !i with
			| Expr e -> visit st e
			| Pseudo p -> visit_pseudo st p
			| Arg a -> visit_arg st a
			| Empty -> ());
			incr i;
			flush_metas st
		done;
		st.walking <- false;
		(* A class reached from an expression is filled too, which can queue its bodies. *)
		let k = ref !filled in
		while !k < Dyn.length st.class_types do
			fill_class st !k;
			incr k
		done;
		filled := Dyn.length st.class_types;
		flush_metas st;
		if !i = Dyn.length st.queue then continue := false
	done;
	let t_built = Unix.gettimeofday () in
	let bytes = assemble st entry in
	let oc = open_out_bin path in
	output_bytes oc bytes;
	close_out oc;
	if verbose then begin
		let ms a b = int_of_float ((b -. a) *. 1000.) in
		Printf.printf "[ash_tast] %s: %d bytes, %d nodes, %d types, %d classes, %d fields, %d functions, %d locals, %d strings\n"
			path (Bytes.length bytes) (Dyn.length st.nodes / 8) (Dyn.length st.types / 8)
			(Dyn.length st.classes / 9) (Dyn.length st.fields / 6) (Dyn.length st.funcs / 6)
			(Dyn.length st.locals / 3) (Dyn.length st.str_off);
		Printf.printf "[ash_tast] native: build %d ms, assemble and write %d ms\n"
			(ms t0 t_built) (ms t_built (Unix.gettimeofday ()));
		if st.dynamic_fields > 0 then Printf.printf "[ash_tast] fields written by name only: %d\n" st.dynamic_fields;
		Hashtbl.iter (fun k n -> Printf.printf "[ash_tast] unsupported %s: %d\n" k n) st.unsupported
	end
