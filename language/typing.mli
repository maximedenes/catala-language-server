open Lsp.Types
open Catalalsp
open LspData

type typing_result = {
  diagnostics : Diagnostic.t list PathMap.t;
  references : References.reference_map;
  program : Desugared.Ast.program option;
  nameres_context : Desugared.Name_resolution.context option;
  revdeps : string PathMap.t;
}

val type_program : fname:string -> Surface.Ast.program -> typing_result

val find_definition : fname:string -> Desugared.Name_resolution.context -> References.reference_map -> Lsp.Types.Position.t -> Location.t option