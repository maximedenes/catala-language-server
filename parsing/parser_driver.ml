module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports
module I = Parser.Interpreter

(* -------------------------------------------------------------------- *)
open I

module Printers = struct

  let buf = Buffer.create 16
  let print s = Buffer.add_string buf s

  let print_nt : type a. a I.nonterminal -> string =
    fun nt ->
    match nt with
    | _ -> assert false

  let print_terminal : type a. a terminal -> string =
    fun t -> match t with
    | _ -> assert false

  let print_symbol (symbol : xsymbol) =
    let s = match symbol with
    | X (T t) -> print_terminal t
    | X (N nt) -> print_nt nt
    in
    Buffer.add_string buf s

  let print_element = Some (fun _e -> Buffer.add_string buf "EL")

  let reset () = Buffer.reset buf

end

module Print = MenhirLib.Printers.Make(I)(Printers)

let string_of_symbol symb =
  Printers.reset ();
  Printers.print_symbol symb;
  Buffer.contents Printers.buf

let rec reduce_cst n nt cst acc =
  match cst with
  | [] -> if n > 0 then raise (Failure "More symbols but empty cst")
    else [Syntax.Concrete.make_nonterminal nt acc]
  | (Syntax.Concrete.NonTerminal { kind = Error }, _) as symb :: cst' ->
    (* Error nodes should not count as RHS symbols *)
    reduce_cst n nt cst' (symb::acc)
  | symb :: cst' ->
    if n > 0 then reduce_cst (n-1) nt cst' (symb::acc)
    else Syntax.Concrete.make_nonterminal nt acc :: cst

let extract_nonterminal symb =
  match symb with
  | I.(X T _) -> assert false
  | I.(X N nt) -> Syntax.Concrete.X nt

(** TODO export in Catala *)

(** After parsing, heading structure is completely flat because of the
    [source_file_item] rule. We need to tree-i-fy the flat structure, by looking
    at the precedence of the law headings. *)
let rec law_struct_list_to_tree (f : Surface.Ast.law_structure list) :
    Surface.Ast.law_structure list =
  match f with
  | [] -> []
  | [item] -> [item]
  | first_item :: rest -> (
    let rest_tree = law_struct_list_to_tree rest in
    match rest_tree with
    | [] -> assert false (* there should be at least one rest element *)
    | rest_head :: rest_tail -> (
      match first_item with
      | CodeBlock _ | LawText _ | LawInclude _ | ModuleDef _ | ModuleUse _ ->
        (* if an article or an include is just before a new heading , then we
           don't merge it with what comes next *)
        first_item :: rest_head :: rest_tail
      | LawHeading (heading, _) ->
        (* here we have encountered a heading, which is going to "gobble"
           everything in the [rest_tree] until it finds a heading of at least
           the same precedence *)
        let rec split_rest_tree (rest_tree : Surface.Ast.law_structure list) :
            Surface.Ast.law_structure list * Surface.Ast.law_structure list =
          match rest_tree with
          | [] -> [], []
          | LawHeading (new_heading, _) :: _
            when new_heading.law_heading_precedence
                 <= heading.law_heading_precedence ->
            (* we stop gobbling *)
            [], rest_tree
          | first :: after ->
            (* we continue gobbling *)
            let after_gobbled, after_out = split_rest_tree after in
            first :: after_gobbled, after_out
        in
        let gobbled, rest_out = split_rest_tree rest_tree in
        LawHeading (heading, gobbled) :: rest_out))

exception ParserError of {
  suggestion : string list option;
  error_loc : Catala_utils.Pos.t;
  last_good_loc : Catala_utils.Pos.t option;
  token : string;
  msg : string;
}

(** Usage: [raise_parser_error error_loc last_good_loc token msg]

    Raises an error message featuring the [error_loc] position where the parser
    has failed, the [token] on which the parser has failed, and the error
    message [msg]. If available, displays [last_good_loc] the location of the
    last token correctly parsed. *)
    (*
let raise_parser_error
    ?(suggestion : string list option)
    (error_loc : Catala_utils.Pos.t)
    (last_good_loc : Catala_utils.Pos.t option)
    (token : string)
    (msg : string) : 'a =
  Catala_utils.Message.raise_multispanned_error_full ?suggestion
    ((Some (fun ppf -> Format.pp_print_string ppf "Error token:"), error_loc)
    ::
    (match last_good_loc with
    | None -> []
    | Some last_good_loc ->
      [
        ( Some (fun ppf -> Format.pp_print_string ppf "Last good token:"),
          last_good_loc );
      ]))
    "@[<v>Syntax error at token %a@,%t@]"
    (fun ppf string -> Format.fprintf ppf "@{<yellow>\"%s\"@}" string)
    token msg
    *)

(** Returns the state number from the Menhir environment *)
  let state (env : 'semantic_value I.env) : int =
    match Lazy.force (I.stack env) with
    | MenhirLib.General.Nil -> 0
    | MenhirLib.General.Cons (Element (s, _, _, _), _) -> I.number s

(** Usage: [fail lexbuf env token_list last_input_needed]

    Raises an error with meaningful hints about what the parsing error was.
    [lexbuf] is the lexing buffer state at the failure point, [env] is the
    Menhir environment and [last_input_needed] is the last checkpoint of a
    valid Menhir state before the parsing error. [token_list] is provided by
    things like {!val: Surface.Lexer_common.token_list_language_agnostic} and
    is used to provide suggestions of the tokens acceptable at the failure
    point *)
let fail
    (lexbuf : Sedlexing.lexbuf)
    (env : 'semantic_value I.env)
    (token_list : (string * Surface.Tokens.token) list)
    (last_input_needed : 'semantic_value I.env option) : 'a =
  let wrong_token = Sedlexing.Utf8.lexeme lexbuf in
  let acceptable_tokens, last_positions =
    match last_input_needed with
    | Some last_input_needed ->
      ( List.filter
          (fun (_, t) ->
            I.acceptable
              (I.input_needed last_input_needed)
              t
              (fst (Sedlexing.lexing_positions lexbuf)))
          token_list,
        Some (I.positions last_input_needed) )
    | None -> token_list, None
  in
  let similar_acceptable_tokens =
    Catala_utils.Suggestions.suggestion_minimum_levenshtein_distance_association
      (List.map (fun (s, _) -> s) acceptable_tokens)
      wrong_token
  in
  let custom_menhir_message =
    try
      Surface.Parser_errors.message (state env)
    with
    | Not_found ->
      "Unexpected token"
    in
    raise (ParserError {
      suggestion = Some similar_acceptable_tokens;
      error_loc = Catala_utils.Pos.from_lpos (positions env);
      last_good_loc = (Option.map Catala_utils.Pos.from_lpos last_positions);
      token = Sedlexing.Utf8.lexeme lexbuf;
      msg =  custom_menhir_message;
    })

  (** Main parsing loop *)
  let rec loop
      (next_token : unit -> Surface.Tokens.token * Lexing.position * Lexing.position)
      (token_list : (string * Surface.Tokens.token) list)
      (lexbuf : Sedlexing.lexbuf)
      (last_input_needed : 'semantic_value I.env option)
      extra_tokens cst
      (checkpoint : 'semantic_value I.checkpoint) =
    match checkpoint with
    | I.InputNeeded env ->
      let (token, startp, endp as triple) = next_token () in
      let extra_tokens = Syntax.Concrete.make_terminal startp endp token :: extra_tokens in
      let checkpoint = I.offer checkpoint triple in
      loop next_token token_list lexbuf (Some env) extra_tokens cst checkpoint
    | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      let cst = List.append extra_tokens cst in
      loop next_token token_list lexbuf last_input_needed [] cst checkpoint
    | I.AboutToReduce (_env, production) ->
      let n = List.length (rhs production) in
      let nt = extract_nonterminal (lhs production) in
      let cst = reduce_cst n nt cst [] in
      let checkpoint = I.resume checkpoint in
      loop next_token token_list lexbuf last_input_needed extra_tokens cst checkpoint
    | I.HandlingError env -> fail lexbuf env token_list last_input_needed
    | I.Accepted v ->
      begin match cst with
      | [root] -> Syntax.Concrete.mk_root root, v
      | _ ->
        assert false
      end
    | I.Rejected ->
      (* Cannot happen as we stop at syntax error immediatly *)
      assert false

(** Stub that wraps the parsing main loop and handles the Menhir/Sedlex type
    difference for [lexbuf]. *)
let sedlex_with_menhir
    (lexer' : Sedlexing.lexbuf -> Surface.Tokens.token)
    (token_list : (string * Surface.Tokens.token) list)
    (target_rule : Lexing.position -> 'semantic_value I.checkpoint)
    (lexbuf : Sedlexing.lexbuf) =
  let lexer : unit -> Surface.Tokens.token * Lexing.position * Lexing.position =
    Sedlexing.with_tokenizer lexer' lexbuf
  in
  try
    loop lexer token_list lexbuf None [] []
      (target_rule (fst @@ Sedlexing.lexing_positions lexbuf))
  with Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _ ->
    Surface.Lexer_common.raise_lexer_error
      (Catala_utils.Pos.from_lpos (Sedlexing.lexing_positions lexbuf))
      (Sedlexing.Utf8.lexeme lexbuf)

let commands_or_includes (lexbuf : Sedlexing.lexbuf) : Syntax.Concrete.node * Surface.Ast.source_file =
  sedlex_with_menhir Surface.Lexer_en.lexer Surface.Lexer_en.token_list
    Parser.CatalaParser.Incremental.source_file lexbuf

(** {1 Parsing multiple files} *)

let lexbuf_file lexbuf =
  (fst (Sedlexing.lexing_positions lexbuf)).Lexing.pos_fname

let with_sedlex_file file f =
  let ic = open_in file in
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  Sedlexing.set_filename lexbuf file;
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f lexbuf)

open Catala_utils
open Surface

(** Parses a single source file *)
let rec parse_source (lexbuf : Sedlexing.lexbuf) : Syntax.Concrete.node * Surface.Ast.program =
  let source_file_name = lexbuf_file lexbuf in
  Message.emit_debug "Parsing %a" File.format source_file_name;
  let language = Cli.file_lang source_file_name in
  let cst, commands = commands_or_includes lexbuf in
  let program = expand_includes source_file_name commands in
  cst, {
    program with
    program_source_files = source_file_name :: program.Ast.program_source_files;
    program_lang = language;
  }

(** Expands the include directives in a parsing result, thus parsing new source
    files *)
and expand_includes (source_file : string) (commands : Ast.law_structure list) :
    Surface.Ast.program =
  let language = Cli.file_lang source_file in
  let rprg =
    List.fold_left
      (fun acc command ->
        let join_module_names name_opt =
          match acc.Ast.program_module_name, name_opt with
          | opt, None | None, opt -> opt
          | Some id1, Some id2 ->
            Message.raise_multispanned_error
              [None, Mark.get id1; None, Mark.get id2]
              "Multiple definitions of the module name"
        in
        match command with
        | Ast.ModuleDef (id, _) ->
          {
            acc with
            Ast.program_module_name = join_module_names (Some id);
            Ast.program_items = command :: acc.Ast.program_items;
          }
        | Ast.ModuleUse (mod_use_name, alias) ->
          let mod_use_alias = Option.value ~default:mod_use_name alias in
          {
            acc with
            Ast.program_used_modules =
              { mod_use_name; mod_use_alias } :: acc.Ast.program_used_modules;
            Ast.program_items = command :: acc.Ast.program_items;
          }
        | Ast.LawInclude (Ast.CatalaFile inc_file) ->
          let source_dir = Filename.dirname source_file in
          let sub_source = File.(source_dir / Mark.remove inc_file) in
          with_sedlex_file sub_source
          @@ fun lexbuf ->
          let _cst, includ_program = parse_source lexbuf in
          let () =
            includ_program.Ast.program_module_name
            |> Option.iter
               @@ fun id ->
               Message.raise_multispanned_error
                 [
                   Some "File include", Mark.get inc_file;
                   Some "Module declaration", Mark.get id;
                 ]
                 "A file that declares a module cannot be used through the raw \
                  '@{<yellow>> Include@}' directive. You should use it as a \
                  module with '@{<yellow>> Use @{<blue>%s@}@}' instead."
                 (Mark.remove id)
          in
          {
            Ast.program_module_name = acc.program_module_name;
            Ast.program_source_files =
              List.rev_append includ_program.program_source_files
                acc.Ast.program_source_files;
            Ast.program_items =
              List.rev_append includ_program.program_items acc.Ast.program_items;
            Ast.program_used_modules =
              List.rev_append includ_program.program_used_modules
                acc.Ast.program_used_modules;
            Ast.program_lang = language;
          }
        | Ast.LawHeading (heading, commands') ->
          let {
            Ast.program_module_name;
            Ast.program_items = commands';
            Ast.program_source_files = new_sources;
            Ast.program_used_modules = new_used_modules;
            Ast.program_lang = _;
          } =
            expand_includes source_file commands'
          in
          {
            Ast.program_module_name = join_module_names program_module_name;
            Ast.program_source_files =
              List.rev_append new_sources acc.Ast.program_source_files;
            Ast.program_items =
              Ast.LawHeading (heading, commands') :: acc.Ast.program_items;
            Ast.program_used_modules =
              List.rev_append new_used_modules acc.Ast.program_used_modules;
            Ast.program_lang = language;
          }
        | i -> { acc with Ast.program_items = i :: acc.Ast.program_items })
      {
        Ast.program_module_name = None;
        Ast.program_source_files = [];
        Ast.program_items = [];
        Ast.program_used_modules = [];
        Ast.program_lang = language;
      }
      commands
  in
  {
    Ast.program_lang = language;
    Ast.program_module_name = rprg.Ast.program_module_name;
    Ast.program_source_files = List.rev rprg.Ast.program_source_files;
    Ast.program_items = List.rev rprg.Ast.program_items;
    Ast.program_used_modules = List.rev rprg.Ast.program_used_modules;
  }

(** {1 API} *)

let with_sedlex_source source_file f =
  match source_file with
  | Cli.FileName file -> with_sedlex_file file f
  | Cli.Contents (str, file) ->
    let lexbuf = Sedlexing.Utf8.from_string str in
    Sedlexing.set_filename lexbuf file;
    f lexbuf
  | Cli.Stdin file ->
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    Sedlexing.set_filename lexbuf file;
    f lexbuf

let check_modname program source_file =
  match program.Ast.program_module_name, source_file with
  | ( Some (mname, pos),
      (Cli.FileName file | Cli.Contents (_, file) | Cli.Stdin file) )
    when not File.(equal mname Filename.(remove_extension (basename file))) ->
    Message.raise_spanned_error pos
      "@[<hov>Module declared as@ @{<blue>%s@},@ which@ does@ not@ match@ the@ \
       file@ name@ %a.@ Rename the module to@ @{<blue>%s@}@ or@ the@ file@ to@ \
       %a.@]"
      mname File.format file
      (String.capitalize_ascii Filename.(remove_extension (basename file)))
      File.format
      File.((dirname file / mname) ^ Filename.extension file)
  | _ -> ()

let parse_program (source_file : Cli.input_src) =
  let cst, program = with_sedlex_source source_file parse_source in
  check_modname program source_file;
  cst, {
    program with
    Ast.program_items = law_struct_list_to_tree program.Ast.program_items;
  }

(*
let program_cst ~fname (inc : Jasmin.Utils.IO.input) =
  let ch = Jasmin.Utils.IO.to_input_channel inc in
  let lexbuf = L.init fname (Lexing.from_channel ch) in
  Lexer.initialize lexbuf;
  let checkpoint = P.Incremental.module_ lexbuf.lex_curr_p in
  let recovery_state = { last_reduction = FoundNothingAt (checkpoint, []); new_symbols = 0 } in
  loop Lexer.start (checkpoint, []) [] recovery_state [] [] checkpoint

let succeed v = v

let fail buffer _checkpoint =
  let (p1,p2) = E.last buffer in
  let location = Jasmin.Location.make p1 p2 in
  let message = "Parsing error" in
  raise (Jasmin.Syntax.ParseError (location, Some message))

let parse_program_from_tokens startp supplier =
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = P.Incremental.module_ startp in
  I.loop_handle succeed (fail buffer) supplier checkpoint

let pos_of_loc l =
  let open Lexing in
  let open Jasmin.Location in
  let pos_fname = l.loc_fname in
  let (pos_lnum_start, pos_char_start) = l.loc_start in
  let (pos_lnum_end, pos_char_end) = l.loc_end in
  let pos_bol_start = l.loc_bchar - pos_char_start in
  let pos_bol_end = l.loc_echar - pos_char_end in
  let startp = { pos_fname; pos_lnum = pos_lnum_start; pos_cnum = l.loc_bchar; pos_bol = pos_bol_start } in
  let endp = { pos_fname; pos_lnum = pos_lnum_end; pos_cnum = l.loc_echar; pos_bol = pos_bol_end } in
  (startp, endp)

let tokens_of_cst cst =
  Syntax.Concrete.fold_skip_errors (fun acc node -> match fst node.green with Terminal x -> let (startp,endp) = pos_of_loc (snd node.green) in (x, startp, endp) :: acc | _ -> acc) [] cst

let dispenser_of_token_list l =
  let d = Seq.to_dispenser (List.to_seq l) in
  fun () -> Option.get (d ())

let parse_program ~fname inc =
  let cst, errors = program_cst ~fname inc in
  let startp = Lexing.{
    pos_fname = fname;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0
  }
  in
  let tokens = List.rev (tokens_of_cst cst) in
  cst, errors, parse_program_from_tokens startp (dispenser_of_token_list tokens)

  *)