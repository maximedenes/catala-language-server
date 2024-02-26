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

  let show_token (token : Surface.Tokens.token) =
    match token with
    | Surface.Tokens.YEAR -> "YEAR"
    | Surface.Tokens.XOR -> "XOR"
    | Surface.Tokens.WITH_V -> "WITH_V"
    | Surface.Tokens.WITH -> "WITH"
    | Surface.Tokens.WILDCARD -> "WILDCARD"
    | Surface.Tokens.WE_HAVE -> "WE_HAVE"
    | Surface.Tokens.VARIES -> "VARIES"
    | Surface.Tokens.UNDER_CONDITION -> "UNDER_CONDITION"
    | Surface.Tokens.UIDENT s -> "UIDENT " ^ s
    | Surface.Tokens.TRUE -> "TRUE"
    | Surface.Tokens.THEN -> "THEN"
    | Surface.Tokens.THAT -> "THAT"
    | Surface.Tokens.TEXT -> "TEXT"
    | Surface.Tokens.SUM -> "SUM"
    | Surface.Tokens.SUCH -> "SUCH"
    | Surface.Tokens.STRUCT -> "STRUCT"
    | Surface.Tokens.STATE -> "STATE"
    | Surface.Tokens.SEMICOLON -> "SEMICOLON"
    | Surface.Tokens.SCOPE -> "SCOPE"
    | Surface.Tokens.RULE -> "RULE"
    | Surface.Tokens.RPAREN -> "RPAREN"
    | Surface.Tokens.RBRACKET -> "RBRACKET"
    | Surface.Tokens.RBRACE -> "RBRACE"
    | Surface.Tokens.PLUSPLUS -> "PLUSPLUS"
    | Surface.Tokens.PLUS _ -> "PLUS"
    | Surface.Tokens.PERCENT -> "PERCENT"
    | Surface.Tokens.OUTPUT -> "OUTPUT"
    | Surface.Tokens.OR -> "OR"
    | Surface.Tokens.OF -> "OF"
    | Surface.Tokens.NOT_EQUAL -> "NOT_EQUAL"
    | Surface.Tokens.NOT -> "NOT"
    | Surface.Tokens.MULT _ -> "MULT"
    | Surface.Tokens.MONTH -> "MONTH"
    | Surface.Tokens.MONEY_AMOUNT _ -> "MONEY_AMOUNT"
    | Surface.Tokens.MONEY -> "MONEY"
    | Surface.Tokens.MODULE_USE -> "MODULE_USE"
    | Surface.Tokens.MODULE_EXTERNAL -> "MODULE_EXTERNAL"
    | Surface.Tokens.MODULE_DEF -> "MODULE_DEF"
    | Surface.Tokens.MODULE_ALIAS -> "MODULE_ALIAS"
    | Surface.Tokens.MINUS _ -> "MINUS"
    | Surface.Tokens.MINIMUM -> "MINIMUM"
    | Surface.Tokens.MAXIMUM -> "MAXIMUM"
    | Surface.Tokens.MATCH -> "MATCH"
    | Surface.Tokens.LPAREN -> "LPAREN"
    | Surface.Tokens.LIST_EMPTY -> "LIST_EMPTY"
    | Surface.Tokens.LIST -> "LIST"
    | Surface.Tokens.LIDENT s -> "LIDENT " ^ s
    | Surface.Tokens.LET -> "LET"
    | Surface.Tokens.LESSER_EQUAL _ -> "LESSER_EQUAL"
    | Surface.Tokens.LESSER _ -> "LESSER"
    | Surface.Tokens.LBRACKET -> "LBRACKET"
    | Surface.Tokens.LBRACE -> "LBRACE"
    | Surface.Tokens.LAW_TEXT s -> "LAW_TEXT " ^ s
    | Surface.Tokens.LAW_INCLUDE -> "LAW_INCLUDE"
    | Surface.Tokens.LAW_HEADING _ -> "LAW_HEADING"
    | Surface.Tokens.LABEL -> "LABEL"
    | Surface.Tokens.IS -> "IS"
    | Surface.Tokens.INT_LITERAL _ -> "INT_LITERAL"
    | Surface.Tokens.INTERNAL -> "INTERNAL"
    | Surface.Tokens.INTEGER -> "INTEGER"
    | Surface.Tokens.INPUT -> "INPUT"
    | Surface.Tokens.INCREASING -> "INCREASING"
    | Surface.Tokens.IN -> "IN"
    | Surface.Tokens.IF -> "IF"
    | Surface.Tokens.GREATER_EQUAL _ -> "GREATER_EQUAL"
    | Surface.Tokens.GREATER _ -> "GREATER"
    | Surface.Tokens.FOR -> "FOR"
    | Surface.Tokens.FIXED -> "FIXED"
    | Surface.Tokens.FILLED -> "FILLED"
    | Surface.Tokens.FALSE -> "FALSE"
    | Surface.Tokens.EXISTS -> "EXISTS"
    | Surface.Tokens.EXCEPTION -> "EXCEPTION"
    | Surface.Tokens.EQUAL -> "EQUAL"
    | Surface.Tokens.EOF -> "EOF"
    | Surface.Tokens.ENUM -> "ENUM"
    | Surface.Tokens.END_DIRECTIVE -> "END_DIRECTIVE"
    | Surface.Tokens.END_CODE _ -> "END_CODE"
    | Surface.Tokens.ELSE -> "ELSE"
    | Surface.Tokens.DURATION -> "DURATION"
    | Surface.Tokens.DOT -> "DOT"
    | Surface.Tokens.DIV _ -> "DIV"
    | Surface.Tokens.DIRECTIVE_ARG _ -> "DIRECTIVE_ARG"
    | Surface.Tokens.DEPENDS -> "DEPENDS"
    | Surface.Tokens.DEFINITION -> "DEFINITION"
    | Surface.Tokens.DEFINED_AS -> "DEFINED_AS"
    | Surface.Tokens.DECREASING -> "DECREASING"
    | Surface.Tokens.DECLARATION -> "DECLARATION"
    | Surface.Tokens.DECIMAL_LITERAL _ -> "DECIMAL_LITERAL"
    | Surface.Tokens.DECIMAL -> "DECIMAL"
    | Surface.Tokens.DAY -> "DAY"
    | Surface.Tokens.DATE_LITERAL _ -> "DATE_LITERAL"
    | Surface.Tokens.DATE -> "DATE"
    | Surface.Tokens.DATA -> "DATA"
    | Surface.Tokens.CONTEXT -> "CONTEXT"
    | Surface.Tokens.CONTENT -> "CONTENT"
    | Surface.Tokens.CONTAINS -> "CONTAINS"
    | Surface.Tokens.CONSEQUENCE -> "CONSEQUENCE"
    | Surface.Tokens.CONDITION -> "CONDITION"
    | Surface.Tokens.COMMA -> "COMMA"
    | Surface.Tokens.COLON -> "COLON"
    | Surface.Tokens.CARDINAL -> "CARDINAL"
    | Surface.Tokens.BY -> "BY"
    | Surface.Tokens.BOOLEAN -> "BOOLEAN"
    | Surface.Tokens.BEGIN_METADATA -> "BEGIN_METADATA"
    | Surface.Tokens.BEGIN_DIRECTIVE -> "BEGIN_DIRECTIVE"
    | Surface.Tokens.BEGIN_CODE -> "BEGIN_CODE"
    | Surface.Tokens.AT_PAGE _ -> "AT_PAGE"
    | Surface.Tokens.ASSERTION -> "ASSERTION"
    | Surface.Tokens.AND -> "AND"
    | Surface.Tokens.AMONG -> "AMONG"
    | Surface.Tokens.ALT -> "ALT"
    | Surface.Tokens.ALL -> "ALL"

  let show_nonterminal nt =
    match nt with
    | X (Parser.Interpreter.N_variation_type) -> "VARIATION_TYPE"
    | X (Parser.Interpreter.N_typ_data) -> "TYP_DATA"
    | X (Parser.Interpreter.N_struct_scope_base) -> "STRUCT_SCOPE_BASE"
    | X (Parser.Interpreter.N_struct_scope) -> "STRUCT_SCOPE"
    | X (Parser.Interpreter.N_struct_content_field) -> "STRUCT_CONTENT_FIELD"
    | X (Parser.Interpreter.N_state) -> "STATE"
    | X (Parser.Interpreter.N_source_file_item) -> "SOURCE_FILE_ITEM"
    | X (Parser.Interpreter.N_source_file) -> "SOURCE_FILE"
    | X (Parser.Interpreter.N_separated_nonempty_list_SEMICOLON_expression_) -> "separated_nonempty_list_SEMICOLON_expression"
    | X (Parser.Interpreter.N_separated_nonempty_list_DOT_addpos_LIDENT__) -> "separated_nonempty_list_DOT_addpos_LIDENT"
    | X (Parser.Interpreter.N_separated_nonempty_list_COMMA_var_content_) -> "separated_nonempty_list_COMMA_var_content"
    | X (Parser.Interpreter.N_separated_nonempty_list_COMMA_lident_) -> "separated_nonempty_list_COMMA_lident"
    | X (Parser.Interpreter.N_separated_nonempty_list_COMMA_expression_) -> "separated_nonempty_list_COMMA_expression"
    | X (Parser.Interpreter.N_separated_nonempty_list_COMMA_addpos_typ_data__) -> "separated_nonempty_list_COMMA_addpos_typ_data"
    | X (Parser.Interpreter.N_scope_item) -> "SCOPE_ITEM"
    | X (Parser.Interpreter.N_scope_decl_item_attribute_output) -> "scope_decl_item_attribute_output"
    | X (Parser.Interpreter.N_scope_decl_item_attribute_input) -> "scope_decl_item_attribute_input"
    | X (Parser.Interpreter.N_scope_decl_item_attribute) -> "scope_decl_item_attribute"
    | X (Parser.Interpreter.N_scope_decl_item) -> "SCOPE_DECL_ITEM"
    | X (Parser.Interpreter.N_rule_expr) -> "RULE_EXPR"
    | X (Parser.Interpreter.N_rule_consequence) -> "RULE_CONSEQUENCE"
    | X (Parser.Interpreter.N_rule) -> "RULE"
    | X (Parser.Interpreter.N_quident) -> "QUIDENT"
    | X (Parser.Interpreter.N_qlident) -> "QLIDENT"
    | X (Parser.Interpreter.N_primitive_typ) -> "PRIMITIVE_TYP"
    | X (Parser.Interpreter.N_option_state_qualifier_) -> "option_state_qualifier"
    | X (Parser.Interpreter.N_option_state_) -> "OPTION_STATE"
    | X (Parser.Interpreter.N_option_scope_call_args_) -> "option_scope_call_args"
    | X (Parser.Interpreter.N_option_preceded_UNDER_CONDITION_expression__) -> "option_preceded_UNDER_CONDITION_expression"
    | X (Parser.Interpreter.N_option_preceded_MODULE_ALIAS_addpos_DIRECTIVE_ARG___) -> "option_preceded_MODULE_ALIAS_addpos_DIRECTIVE_ARG"
    | X (Parser.Interpreter.N_option_preceded_CONTENT_expression__) -> "option_preceded_CONTENT_expression"
    | X (Parser.Interpreter.N_option_preceded_CONTENT_addpos_typ___) -> "option_preceded_CONTENT_addpos_typ"
    | X (Parser.Interpreter.N_option_opt_def_) -> "option_opt_def"
    | X (Parser.Interpreter.N_option_lident_) -> "option_lident"
    | X (Parser.Interpreter.N_option_law_text_) -> "option_law_text"
    | X (Parser.Interpreter.N_option_label_) -> "option_label"
    | X (Parser.Interpreter.N_option_exception_to_) -> "option_exception_to"
    | X (Parser.Interpreter.N_option_condition_consequence_) -> "option_condition_consequence"
    | X (Parser.Interpreter.N_option_addpos_variation_type__) -> "option_addpos_variation_type"
    | X (Parser.Interpreter.N_option_addpos_unit_literal__) -> "option_addpos_unit_literal"
    | X (Parser.Interpreter.N_option_addpos_exception_to__) -> "option_addpos_exception_to"
    | X (Parser.Interpreter.N_option_addpos_definition_parameters__) -> "option_addpos_definition_parameters"
    | X (Parser.Interpreter.N_option_NOT_) -> "option_NOT"
    | X (Parser.Interpreter.N_option_MODULE_EXTERNAL_) -> "option_MODULE_EXTERNAL"
    | X (Parser.Interpreter.N_option_AT_PAGE_) -> "option_AT_PAGE"
    | X (Parser.Interpreter.N_nonempty_list_scope_item_) -> "nonempty_list_scope_item"
    | X (Parser.Interpreter.N_nonempty_list_preceded_ALT_struct_content_field__) -> "nonempty_list_preceded_ALT_struct_content_field"
    | X (Parser.Interpreter.N_nonempty_list_addpos_scope_decl_item__) -> "nonempty_list_addpos_scope_decl_item"
    | X (Parser.Interpreter.N_nonempty_list_addpos_preceded_ALT_match_arm___) -> "nonempty_list_addpos_preceded_ALT_match_arm"
    | X (Parser.Interpreter.N_nonempty_list_LAW_TEXT_) -> "nonempty_list_LAW_TEXT"
    | X (Parser.Interpreter.N_nonempty_list_DIRECTIVE_ARG_) -> "nonempty_list_DIRECTIVE_ARG"
    | X (Parser.Interpreter.N_metadata_block) -> "METADATA_BLOCK"
    | X (Parser.Interpreter.N_match_arm) -> "MATCH_ARM"
    | X (Parser.Interpreter.N_loption_separated_nonempty_list_SEMICOLON_expression__) -> "loption_separated_nonempty_list_SEMICOLON_expression"
    | X (Parser.Interpreter.N_literal) -> "LITERAL"
    | X (Parser.Interpreter.N_list_state_) -> "LIST_STATE"
    | X (Parser.Interpreter.N_list_preceded_ALT_struct_content_field__) -> "list_preceded_ALT_struct_content_field"
    | X (Parser.Interpreter.N_list_addpos_struct_scope__) -> "list_addpos_struct_scope"
    | X (Parser.Interpreter.N_list_addpos_enum_decl_line__) -> "list_addpos_enum_decl_line"
    | X (Parser.Interpreter.N_list_addpos_code_item__) -> "list_addpos_code_item"
    | X (Parser.Interpreter.N_lident) -> "LIDENT"
    | X (Parser.Interpreter.N_law_text) -> "LAW_TEXT"
    | X (Parser.Interpreter.N_law_heading) -> "LAW_HEADING"
    | X (Parser.Interpreter.N_label) -> "LABEL"
    | X (Parser.Interpreter.N_funcall_args) -> "FUNCALL_ARGS"
    | X (Parser.Interpreter.N_expression) -> "EXPRESSION"
    | X (Parser.Interpreter.N_exception_to) -> "EXCEPTION_TO"
    | X (Parser.Interpreter.N_enum_decl_line) -> "ENUM_DECL_LINE"
    | X (Parser.Interpreter.N_directive) -> "DIRECTIVE"
    | X (Parser.Interpreter.N_definition_parameters) -> "DEFINITION_PARAMETERS"
    | X (Parser.Interpreter.N_definition) -> "DEFINITION"
    | X (Parser.Interpreter.N_constructor_binding) -> "CONSTRUCTOR_BINDING"
    | X (Parser.Interpreter.N_condition_consequence) -> "CONDITION_CONSEQUENCE"
    | X (Parser.Interpreter.N_code_item) -> "CODE_ITEM"
    | X (Parser.Interpreter.N_code) -> "CODE"
    | X (Parser.Interpreter.N_assertion) -> "ASSERTION"
    | Error -> "ERROR"

  let rec show_tree d node =
    match Catala_utils.Mark.remove node with
    | Terminal token ->
      String.make (d*2) ' ' ^
      show_token token
    | NonTerminal { kind; children } ->
      String.make (d*2) ' ' ^
      show_nonterminal kind ^ "\n" ^
      String.concat "\n" (List.map (show_tree (d+1)) children)

  let show_tree { green } = show_tree 0 green

end