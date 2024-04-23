echo -e Building the project...
alex Lexer.x
happy Parser.y
alex InputLexer.x
happy InputParser.y
ghc GQL.hs