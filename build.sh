alex -o GoLexer.hs Alex.x && happy -o Parser.hs Happy.y && ghc --make -o go10cc *.hs
