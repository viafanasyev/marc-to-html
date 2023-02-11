FLAGS				= -Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wmissing-export-lists -Wpartial-fields -Wmissing-deriving-strategies -Wunused-packages -fhide-source-paths -freverse-errors

.PHONY: default
default: all

.PHONY: all
all: release

release: Main.hs
	ghc --make Main.hs $(FLAGS)

.PHONY: clean
clean:
	rm -f *.o *.hi Main
