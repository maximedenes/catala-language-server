open Lsp.Types
open Catalalsp

type typing_result = {
  diagnostics : Diagnostic.t list PathMap.t;
  references : References.reference_map;
  program : Desugared.Ast.program option;
  nameres_context : Desugared.Name_resolution.context option;
  revdeps : string PathMap.t;
}

(*
let push_diag fname diag diagnostics =
  let update = function None -> Some [diag] | Some l -> Some (diag::l) in
  PathMap.update fname update diagnostics
  *)

  (*
let range_of_error_loc default_fname = function
  | Jasmin.Utils.Lnone ->
    let start = Position.create ~character:0 ~line:0 in
    let end_ = Position.create ~character:0 ~line:0 in
    default_fname, Range.create ~start ~end_
  | Jasmin.Utils.Lone loc -> loc.loc_fname, Range.of_jasmin_loc loc
  | Jasmin.Utils.Lmore iloc -> iloc.base_loc.loc_fname, Range.of_jasmin_loc iloc.base_loc

let push_error_diag fname diagnostics e =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  e.Jasmin.Utils.err_msg fmt;
  Format.pp_print_flush fmt ();
  let message = Buffer.contents buf in
  let fname, range = range_of_error_loc fname e.Jasmin.Utils.err_loc in
  let diag = Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Error () in
  push_diag fname diag diagnostics
*)

let type_program ast =
  (*
  let prg =
    Surface.Parser_driver.parse_top_level_file (get_input_src ~fname)
  in
  *)
  let prg = Surface.Fill_positions.fill_pos_with_legislative_info ast in
  let mod_uses, modules = Catala_utils.String.Map.empty, Shared_ast.ModuleName.Map.empty (* FIXME analyze workspace *) in
  let nameres_context = Desugared.Name_resolution.form_context (prg, mod_uses) modules in
  let prg = Desugared.From_surface.translate_program nameres_context prg in
  let program = Desugared.Disambiguate.program prg in
  Desugared.Linting.lint_program program;
  let exceptions_graphs =
    Scopelang.From_desugared.build_exceptions_graph program
  in
  let prg =
    Scopelang.From_desugared.translate_program program exceptions_graphs
  in
  let _type_ordering =
    Scopelang.Dependency.check_type_cycles prg.program_ctx.ctx_structs
      prg.program_ctx.ctx_enums
  in
  let prg = Scopelang.Ast.type_program prg in
  (* Strictly type-checking could stop here, but we also want this pass to
     check full name-resolution and cycle detection. These are checked during
     translation to dcalc so we run it here and drop the result. *)
  let prg = Dcalc.From_scopelang.translate_program prg in

  let prg = Shared_ast.Typing.program prg in
  if not (Dcalc.Invariants.check_all_invariants prg) then
    Printf.eprintf "Some Dcalc invariants are invalid\n"; (* FIXME push diagnostic *)
  { diagnostics = PathMap.empty;
    references = References.empty_reference_map;
    program = Some program;
    nameres_context = Some nameres_context;
    revdeps = PathMap.empty;
  }

let find_definition ~fname ctxt references pos =
  References.find_definition ctxt references ~fname pos