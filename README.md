# scrapbox

`scrapbox` is a parser library for [scrapbox](https://scrapbox.io/product).

`scrapbox` can be used in several ways:
- Parse given scrapbox page and express them as AST
- Convert given commonmark text into scrapbox page
- (TODO) Convert given scrapbox page into commonmark

The library is well-tested including round-trip test which tests whether given rendered arbitrary
AST can be parsed as original AST.

## Usage

### Convert given commonmark text into scrapbox page

Using function `commonMarkToScrapbox`, you can convert any commonmark into scrapbox format.

```
# Diary

Today, I went to an `park` with my _dog_ and found a **bone** under the tree.

## Blockquote

This is an quote from famous artist

> I've found out that the winter in `Japan` is very **very** cold
```

```
[**** Diary]
Today, I went to an `park` with my [/ dog] and found a [[bone]] under the tree.

[*** Blockquote]
This is an quote from famous artist
>I've found out that the winter in `Japan` is very [[very]] cold
```

### Convert given commonmark into scrapbox AST

Using function `commonmarkToScrapboxNode`, you can convert given commonmark into scrapbox AST.
Given a commonmark:
```
# Diary

Today, I went to an `park` with my _dog_ and found a **bone** under the tree.

## Blockquote

This is an quote from famous artist

> I've found out that the winter in `Japan` is very **very** cold
```

`commonmarkToScrapboxNode` will parse given commonmark, and converts it into scrapbox AST:
```
Scrapbox 
    [ HEADING ( Level 4 ) [ TEXT "Diary" ]
    , PARAGRAPH 
        ( ScrapText 
            [ ITEM NoStyle [ TEXT "Today, I went to an " ]
            , CODE_NOTATION "park"
            , ITEM NoStyle [ TEXT " with my " ]
            , ITEM Italic [ TEXT "dog" ]
            , ITEM NoStyle [ TEXT " and found a " ]
            , ITEM Bold [ TEXT "bone" ]
            , ITEM NoStyle [ TEXT " under the tree." ]
            ] 
        )
    , LINEBREAK
    , HEADING ( Level 3 ) [ TEXT "Blockquote" ]
    , PARAGRAPH ( ScrapText [ ITEM NoStyle [ TEXT "This is an quote from famous artist" ] ] )
    , BLOCK_QUOTE 
        ( ScrapText 
            [ ITEM NoStyle [ TEXT "I've found out that the winter in " ]
            , CODE_NOTATION "Japan"
            , ITEM NoStyle [ TEXT " is very " ]
            , ITEM Bold [ TEXT "very" ]
            , ITEM NoStyle [ TEXT " cold" ]
            ] 
        )
    ] 
```
## Known issue
- When parsing commonmark, it cannot parse strikethrough text
- When parsing commonmark, it cannot parse `CUSTOM_INLINE`, `CUSTOM_BLOCK`, `THEMATIC_BLOCK` properly