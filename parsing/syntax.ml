module Concrete = struct

  type xnonterminal =
    | X : 'a Parser.Interpreter.nonterminal -> xnonterminal
    | Error : xnonterminal

  type green_r =
  | Terminal of Surface.Tokens.token
  | NonTerminal of {
      kind : xnonterminal;
      children : green list;
    }

  and green = green_r Catala_utils.Mark.pos

  type node = {
    green: green;
    top: node option; (* TODO can we express that this is not a leaf? *)
  }

  let node_children node =
    let mk_child green =
      { green; top = Some node; }
    in
    match fst node.green with
    | Terminal _ -> []
    | NonTerminal { children } ->
      List.map mk_child children

  let join_pos pos1 pos2 =
    let open Catala_utils in
    if pos1 = Pos.no_pos then pos2
    else if pos2 = Pos.no_pos then pos1
    else Pos.join pos1 pos2

  let make_terminal start stop token = Catala_utils.(Mark.add (Pos.from_lpos (start,stop)) @@ Terminal token) 

  let make_nonterminal kind children =
    let rec last_stop = function
    | [] -> assert false
    | [x] -> snd x
    | _ :: tl -> last_stop tl
    in
    let start, stop = match children with
    | [] -> Catala_utils.Pos.no_pos, Catala_utils.Pos.no_pos
    | _ -> snd (List.hd children), last_stop children
    in
    Catala_utils.(Mark.add (join_pos start stop) @@ NonTerminal { kind; children })

  let rec fold f acc node =
    let acc = f acc node in
    List.fold_left (fold f) acc (node_children node)

  let rec fold_skip_errors f acc node =
    match fst node.green with
    | NonTerminal { kind = Error } -> acc
    | _ ->
      let acc = f acc node in
      List.fold_left (fold_skip_errors f) acc (node_children node)

  let mk_root green =
    { green; top = None }

end