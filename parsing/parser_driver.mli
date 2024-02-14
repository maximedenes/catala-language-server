open Parser.Interpreter

val string_of_symbol : xsymbol -> string

exception ParserError of {
  suggestion : string list option;
  error_loc : Catala_utils.Pos.t;
  last_good_loc : Catala_utils.Pos.t option;
  token : string;
  msg : string;
}

val parse_program : Catala_utils.Cli.input_src -> Syntax.Concrete.node * Surface.Ast.program