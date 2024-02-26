let token_types = ["function"; "keyword"]

let token_modifiers = []

let _log msg = Format.eprintf "       [%d, %f] %s" (Unix.getpid ()) (Unix.gettimeofday ()) msg

let push_token tokens ~deltaLine ~deltaStart ~length ~tokenType ~tokenModifiers =
  tokenModifiers :: tokenType :: length :: deltaStart :: deltaLine :: tokens

let delta_loc lastLine lastStart pos =
  let open Catala_utils in
  let newLine = Pos.get_start_line pos in
  let newStart = Pos.get_start_column pos - 1 in
  let newLine = newLine - 1 in
  let deltaLine = newLine - lastLine in
  let deltaStart = if deltaLine > 0 then newStart else newStart - lastStart in
  (deltaLine, deltaStart, newLine, newStart)

let is_keyword (token : Surface.Tokens.token) =
  match token with
  | WITH_V | WITH | WE_HAVE | VARIES | UNDER_CONDITION | THEN | THAT | SUCH | STRUCT
  | STATE | SCOPE | RULE | OUTPUT | OF | MATCH | LET | INTERNAL | INPUT | INCREASING | IN
  | IF | FOR | FIXED | FILLED | EXCEPTION | DECLARATION -> true
  | _ -> false

(* TODO replace is_keyword with complete token classification *)

let compute_token (lastLine,lastStart,tokens) node =
  (* log ("cst = " ^ Syntax.Concrete.show_tree node); *)
  let open Catala_utils in
  let open Parsing.Syntax.Concrete in
  let pos = Mark.get node.green in
  let green = Mark.remove node.green in
  if not (Pos.get_start_line pos = Pos.get_end_line pos) then begin
     Printf.eprintf "unsupported multi line token\n"; lastLine, lastStart, tokens
  end else
  let length = Pos.get_end_column pos - Pos.get_start_column pos in
  let parent_green = Option.bind node.top (fun x -> Some (Mark.remove x.green)) in
  let (deltaLine, deltaStart, newLine, newStart) = delta_loc lastLine lastStart pos in
  match green, parent_green with
  | Terminal (Surface.Tokens.UIDENT _), Some (NonTerminal { kind = (X Parsing.Parser.Interpreter.N_code_item) }) -> 
    newLine, newStart, push_token tokens ~deltaLine ~deltaStart ~length ~tokenType:0 ~tokenModifiers:0
  | Terminal token, _ when is_keyword token ->
    newLine, newStart, push_token tokens ~deltaLine ~deltaStart ~length ~tokenType:1 ~tokenModifiers:0
  | _ -> lastLine, lastStart, tokens

let compute_tokens cst =
  let _,_,tokens = Parsing.Syntax.Concrete.fold compute_token (0,0,[]) cst in
  List.rev tokens