# scrapbox

[![Build Status](https://travis-ci.org/HirotoShioi/scrapbox.svg?branch=master)](https://travis-ci.org/HirotoShioi/scrapbox)

`scrapbox` is a parser library for [scrapbox](https://scrapbox.io/product).

`scrapbox` can be used in several ways:
- Parse given scrapbox page and express them as AST
- Parse commonmark and either express them as AST or convert it into scrapbox page
- (Experimental) Convert given scrapbox page into commonmark

The library is well-tested including round-trip test which tests whether given arbitrary
AST can be rendered, then parsed as original AST.

## Known issue
- When parsing commonmark, it cannot parse strikethrough text (~~Like this~~)
- When parsing commonmark, it cannot parse `CUSTOM_INLINE`, `CUSTOM_BLOCK`, `THEMATIC_BLOCK` properly
- When parsing commonmark, it cannot render relative link correctly.
- When parsing scrapbox, some of the inline styles cannot be parsed correctly
- When using `scrapboxToCommonmark`, it will not render the `THUMBNAIL` correctly. This is due to the library
missing the url parsing logic therefore the parser cannot know whether the given url isn a image url.
Fixing this issue is the highest priority.