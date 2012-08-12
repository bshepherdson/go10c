
all: go10cc

go10cc: GoParser.hs GoLexer.hs *.hs
	ghc -O2 --make -o go10cc *.hs

GoParser.hs: GoLexer.hs Happy.y
	happy -o GoParser.hs Happy.y

GoLexer.hs: Alex.x
	alex -o GoLexer.hs Alex.x

clean:
	rm -f *.o *.hi GoParser.hs GoLexer.hs
