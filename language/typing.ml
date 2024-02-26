open Lsp.Types
open Catalalsp

type typing_result = {
  diagnostics : Diagnostic.t list PathMap.t;
  references : References.reference_map;
  program : Desugared.Ast.program option;
  nameres_context : Desugared.Name_resolution.context option;
  revdeps : string PathMap.t;
}

let push_diag fname diag diagnostics =
  let update = function None -> Some [diag] | Some l -> Some (diag::l) in
  PathMap.update fname update diagnostics

  (*
let range_of_error_loc default_fname = function
  | Jasmin.Utils.Lnone ->
    let start = Position.create ~character:0 ~line:0 in
    let end_ = Position.create ~character:0 ~line:0 in
    default_fname, Range.create ~start ~end_
  | Jasmin.Utils.Lone loc -> loc.loc_fname, Range.of_jasmin_loc loc
  | Jasmin.Utils.Lmore iloc -> iloc.base_loc.loc_fname, Range.of_jasmin_loc iloc.base_loc
*)

let push_error_diag fname diagnostics (message, pos) =
  let range = LspData.Range.of_catala_pos pos in
  let diag = Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Error () in
  push_diag fname diag diagnostics

let push_warning_diag fname diagnostics (msg, pos) =
  let message = Catala_utils.Message.unformat msg in
  let range = LspData.Range.of_catala_pos pos in
  let diag = Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Warning () in
  push_diag fname diag diagnostics

let type_program ~fname ast =
  (*
  let prg =
    Surface.Parser_driver.parse_top_level_file (get_input_src ~fname)
  in
  *)
  Catala_utils.Message.clear_warnings ();
  try
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
    let warnings = Catala_utils.Message.get_warnings () in
    let warnings = List.concat_map (fun (msg, posl) -> List.filter_map (function (None, pos) -> Some (msg, pos) | _ -> None) posl) warnings in
    let diagnostics = List.fold_left (push_warning_diag fname) (PathMap.singleton fname []) warnings in
    let references = References.collect_references program in
    { diagnostics;
      references;
      program = Some program;
      nameres_context = Some nameres_context;
      revdeps = PathMap.singleton fname fname;
    }
  with Catala_utils.Message.CompilerError content ->
    match Catala_utils.Message.Content.main_message content with
    | Some (msg, pos) ->
      let diagnostics = push_error_diag fname PathMap.empty (msg,pos) in
      { diagnostics = diagnostics;
        references = References.empty_reference_map;
        program = None;
        nameres_context = None;
        revdeps = PathMap.singleton fname fname;
      }
    | None ->
      { diagnostics = PathMap.singleton fname []; (* FIXME add generic error *)
        references = References.empty_reference_map;
        program = None;
        nameres_context = None;
        revdeps = PathMap.singleton fname fname;
      }

let find_definition ~fname ctxt references pos =
  References.find_definition ctxt references ~fname pos