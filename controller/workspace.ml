open Lsp.Types
open Catalalsp
open Language

type root_doc = {
  (* prog : Desugared.Ast.program option; *)
  nameres_context : Desugared.Name_resolution.context option;
}

type workspace = {
  diagnostics : Diagnostic.t list PathMap.t;
  references : References.reference_map;
  open_documents : DocumentManager.state PathMap.t;
  root_documents : root_doc PathMap.t;
  syntax_trees : Parsing.Syntax.Concrete.node PathMap.t;
  revdeps : string PathMap.t;
}

let empty_workspace = {
  diagnostics = PathMap.empty;
  references = References.empty_reference_map;
  open_documents = PathMap.empty;
  root_documents = PathMap.empty;
  syntax_trees = PathMap.empty;
  revdeps = PathMap.empty;
}

(*
let find_files ~root acc =
  let rec explore acc = function
    | [] -> acc
    | hd :: tl when Sys.is_directory hd ->
      let files = List.map (Filename.concat hd) @@ Array.to_list (Sys.readdir hd) in
      explore acc (files @ tl)
    | hd :: tl when Filename.extension hd = ".catala_en" && not (PathMap.mem hd acc) ->
      explore (PathMap.add hd architecture acc) tl
    | _ :: tl -> explore acc tl
  in
  explore acc [root]
*)

let analyze_file fname workspace =
  Printf.eprintf "Analyzing file %s\n" fname;
  let input_src = match PathMap.find_opt fname workspace.open_documents with
    | None -> Catala_utils.Cli.FileName fname
    | Some st -> Catala_utils.Cli.Contents (DocumentManager.get_text st, fname)
  in
  try
    let cst, ast = Parsing.Parser_driver.parse_program input_src in
    let Typing.{ diagnostics; references; nameres_context; revdeps } = Typing.type_program ast in
    let diagnostics = PathMap.union (fun _ v _ -> Some v) diagnostics workspace.diagnostics in
    let root_doc = { nameres_context } in
    let root_documents = PathMap.add fname root_doc workspace.root_documents in
    let syntax_trees = PathMap.add fname cst workspace.syntax_trees in
    let revdeps = PathMap.union (fun _ v _ -> Some v) revdeps workspace.revdeps in
    { workspace with diagnostics; references; root_documents; syntax_trees; revdeps }
  with Parsing.Parser_driver.ParserError { msg; error_loc } ->
    let range = LspData.Range.of_catala_pos error_loc in
    let diag = Diagnostic.create ~message:msg ~range ~severity:DiagnosticSeverity.Error () in
    let diagnostics = PathMap.singleton fname [diag] in
    let diagnostics = PathMap.union (fun _ v _ -> Some v) diagnostics workspace.diagnostics in
    { workspace with diagnostics }

let init ~root =
  let _path = Lsp.Uri.to_path root in
  empty_workspace

let open_document workspace ~fname ~text =
  let doc = DocumentManager.init ~text in
  let open_documents = PathMap.add fname doc workspace.open_documents in
  let workspace = { workspace with open_documents } in
  analyze_file fname workspace
  (*
  match PathMap.find_opt fname workspace.revdeps with
  | None -> Printf.eprintf "Cannot find root document for %s\n" fname; workspace
  | Some root_fname ->
    Printf.eprintf "Opening %s, found root %s\n" fname root_fname;
    match PathMap.find_opt root_fname workspace.root_documents with
    | Some _root_doc -> analyze_file root_fname workspace
    | None -> Printf.eprintf "Cannot find root document %s\n" root_fname; workspace
    *)

let get_document workspace ~fname =
  PathMap.find fname workspace.open_documents

let close_document workspace ~fname =
  let open_documents = PathMap.remove fname workspace.open_documents in
  { workspace with open_documents }

let get_syntax_tree workspace ~fname =
  PathMap.find_opt fname workspace.syntax_trees

let get_diagnostics workspace =
  workspace.diagnostics

let goto_definition workspace ~fname pos =
  match PathMap.find_opt fname workspace.revdeps with
  | None -> Printf.eprintf "Cannot find root document for %s\n" fname; None
  | Some root_fname ->
    Printf.eprintf "Opening %s, found root %s\n" fname root_fname;
    match PathMap.find_opt root_fname workspace.root_documents with
    | Some root_doc ->
      begin match root_doc.nameres_context with
      | None -> Printf.eprintf "No name resolution context for doc %s\n" root_fname; None
      | Some nameres_context ->
        Typing.find_definition ~fname nameres_context workspace.references pos
      end
    | None -> Printf.eprintf "Cannot find root document %s\n" root_fname; None
