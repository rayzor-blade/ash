open EvalValue
open Type

(* Counts what the exporter would write, reading the typed program from the compiler. *)
let count_program (types : module_type list) =
	let classes = ref 0 and funcs = ref 0 and nodes = ref 0 in
	let rec walk e =
		incr nodes;
		Texpr.iter walk e
	in
	let field cf =
		let rec one cf =
			(match cf.cf_expr with
			| Some e ->
				(match e.eexpr with TFunction _ -> incr funcs | _ -> ());
				walk e
			| None -> ());
			List.iter one cf.cf_overloads
		in
		one cf
	in
	List.iter (function
		| TClassDecl c ->
			incr classes;
			List.iter field c.cl_ordered_fields;
			List.iter field c.cl_ordered_statics;
			(match c.cl_constructor with Some cf -> field cf | None -> ());
			(match c.cl_init with Some e -> walk e | None -> ())
		| _ -> ()
	) types;
	(!classes, !funcs, !nodes)

class plugin =
	object
		(* Reports counts on stderr after generation, once the filters have run. *)
		method count_on_generate () : value =
			let compiler = (EvalContext.get_ctx()).curapi in
			compiler.on_generate (fun _ ->
				let t0 = Unix.gettimeofday () in
				let (classes, funcs, nodes) = count_program (compiler.get_com ()).Common.types in
				Printf.eprintf "ash_tast(ocaml): %d classes, %d function bodies, %d nodes, %.1f ms\n%!"
					classes funcs nodes ((Unix.gettimeofday () -. t0) *. 1000.)
			) false;
			vnull
	end
;;

let api = new plugin in
EvalStdLib.StdContext.register [
	("countOnGenerate", EvalEncode.vfun0 api#count_on_generate);
]
