# Apply stylish-haskell on all *.hs files
stylish-haskell:
	find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*' -print0 | xargs -0 stylish-haskell -i

# For developing
ghci:
	@stack ghci scrapbox --haddock-deps

ghcid:
	@ghcid --command "stack ghci scrapbox"

# For testing

# Build & run test
run-test:
	@stack build --fast && \
	stack test --fast

test-ghci:
	@stack ghci scrapbox:test:scrapbox-test

test-ghcid:
	@ghcid --command "stack ghci scrapbox:test:scrapbox-test"