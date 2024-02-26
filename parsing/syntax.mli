module Concrete :
  sig

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
      top: node option;
    }

    val make_terminal :
      Lexing.position -> Lexing.position -> Surface.Tokens.token -> green

    val make_nonterminal :
      xnonterminal -> green list -> green

    val fold : ('a -> node -> 'a) -> 'a -> node -> 'a

    val fold_skip_errors : ('a -> node -> 'a) -> 'a -> node -> 'a

    val mk_root : green -> node

    val show_tree : node -> string

  end
