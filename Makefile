index.html: morphemes.lhs morphemes
	pandoc --filter=./morphemes $< -Sst html -o $@

morphemes: morphemes.lhs
	ghc $@
