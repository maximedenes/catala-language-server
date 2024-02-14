open Catalalsp.LspData

type reference_map

val empty_reference_map : reference_map

val collect_references : Desugared.Ast.program -> reference_map

val find_definition : Desugared.Name_resolution.context -> reference_map -> fname:string -> Lsp.Types.Position.t -> Location.t option