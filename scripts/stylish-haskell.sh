# Apply stylish-haskell on all *.hs files
find . -type f -name "*hs" -not -path '.git' -not -path '*.stack-work*' -print0 | xargs -0 stylish-haskell -i