open Catalalsp
open LspData

module PositionMap = Map.Make(Position)

type reference_r =
  | RefScope of Surface.Ast.uident
  | RefStruct of Surface.Ast.uident

type reference =
  { range : Lsp.Types.Range.t; reference: reference_r }

type reference_map = reference PositionMap.t PathMap.t

let empty_reference_map = PathMap.empty

let collect_references prog =
  let f acc expr = match Catala_utils.Mark.remove expr with
    | Shared_ast.EScopeCall { scope } ->
      Printf.eprintf "Found EScopeCall\n";
      let pos = Catala_utils.Mark.get @@ Shared_ast.ScopeName.get_info scope in
      let range = Range.of_catala_pos pos in
      let r = { range; reference = RefScope (Shared_ast.ScopeName.to_string scope) } in
      let fname = Catala_utils.Pos.get_file pos in
      PathMap.update fname (function None -> Some (PositionMap.singleton range.start r) | Some map -> Some (PositionMap.add range.start r map)) acc
    | Shared_ast.EStruct _ ->
      Printf.eprintf "Found EStruct\n";
      acc
    | Shared_ast.EStructAccess { name } ->
      Printf.eprintf "Found EStructAccess\n";
      let pos = Catala_utils.Mark.get @@ Shared_ast.StructName.get_info name in
      let range = Range.of_catala_pos pos in
      let r = { range; reference = RefStruct (Shared_ast.StructName.to_string name) } in
      let fname = Catala_utils.Pos.get_file pos in
      PathMap.update fname (function None -> Some (PositionMap.singleton range.start r) | Some map -> Some (PositionMap.add range.start r map)) acc
    | _ -> acc
  in
  Printf.eprintf "Collecting references\n";
  Desugared.Ast.fold_exprs ~f ~init:PathMap.empty prog

let find_reference refmap pos =
  Printf.eprintf "Search for reference at line %d char %d\n" pos.Position.line pos.character;
  match PositionMap.find_last_opt (fun start -> Position.compare start pos <= 0) refmap with
  | None -> None
  | Some (_,r) ->
    Printf.eprintf "Found reference close to pos, ends line %d char %d\n" r.range.end_.line r.range.end_.character;
    if Position.compare r.range.end_ pos >= 0 then Some r else None

let find_definition ctxt refmap ~fname pos =
  begin match PathMap.find_opt fname refmap with
  | None ->
    Printf.eprintf "No reference map found\n";
    None
  | Some map ->
    Printf.eprintf "Found map\n";
    begin match find_reference map pos with
    | None -> None
    | Some { reference = RefScope scopename } ->
      Printf.eprintf "Found scope reference %s\n" scopename;
      begin match Shared_ast.Ident.Map.find_opt scopename ctxt.Desugared.Name_resolution.local.typedefs with
      | None -> None
      | Some (TScope (id, _)) ->
        let pos = Catala_utils.Mark.get @@ Shared_ast.ScopeName.get_info id in
        Some (Location.of_catala_pos pos)
      | Some _ -> None
      end
    | Some { reference = RefStruct structname } ->
      Printf.eprintf "Found struct reference %s\n" structname;
      begin match Shared_ast.Ident.Map.find_opt structname ctxt.Desugared.Name_resolution.local.typedefs with
      | None -> None
      | Some (TStruct id) ->
        let pos = Catala_utils.Mark.get @@ Shared_ast.StructName.get_info id in
        Some (Location.of_catala_pos pos)
      | Some _ -> None
      end
    end
  end