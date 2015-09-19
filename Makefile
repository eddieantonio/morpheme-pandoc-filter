BIN = morphemes
HSC = ghc
HSFLAGS =
PREFIX = /usr/local

all: index.html $(BIN)

index.html: morphemes.lhs $(BIN)
	pandoc --filter=./$(BIN) $< -Sst html -o $@

# Compile Haskell
%: %.lhs
	$(HSC) $(HSFLAGS) $@

install: $(BIN)
	cp $< $(PREFIX)/bin/$<

clean:
	-$(RM) $(BIN)
	-$(RM) *.o
	-$(RM) *.hi
	-$(RM) index.html

.PHONY: all clean install
