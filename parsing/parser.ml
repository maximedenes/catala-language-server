module CatalaParser = Surface.Parser.Make(Surface.Lexer_en)

module Interpreter = CatalaParser.MenhirInterpreter