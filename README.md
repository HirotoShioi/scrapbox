# scrapbox

[![Build Status](https://travis-ci.org/HirotoShioi/scrapbox.svg?branch=master)](https://travis-ci.com/HirotoShioi/scrapbox.svg?branch=master)

`scrapbox` is a parser library for [scrapbox](https://scrapbox.io/product).

`scrapbox` can be used in several ways:
- Parse given scrapbox page and express them as structured tree
- Parse commonmark and either express them as structured tree or convert it into scrapbox page
- (Experimental) Convert given scrapbox page into commonmark

The library is well-tested including round-trip test which tests whether given arbitrary
tree can be rendered, then parsed as original tree.

## Known issues

- When parsing commonmark, it cannot parse `CUSTOM_INLINE`, `CUSTOM_BLOCK`, `THEMATIC_BLOCK` properly
- When parsing scrapbox, some of the inline styles cannot be parsed correctly
- When parsing scrapbox, it cannot parse correctly when user-defined styles are being used.