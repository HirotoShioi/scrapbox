{-| Test suites for commonmark parser
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Commonmark where

import           RIO

import           Data.Char (isLetter)
import           RIO.List (initMaybe, lastMaybe, zipWith)
import qualified CMark as C
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property,
                                  arbitraryPrintableChar, choose, elements,
                                  genericShrink, listOf, listOf1, oneof,
                                  suchThat, vectorOf, (===))

import           Data.Scrapbox (Block (..), CodeName (..), CodeSnippet (..),
                                InlineBlock (..), Level (..), ScrapText (..),
                                Scrapbox (..), Segment (..), Start (..),
                                Style (..), TableContent (..), TableName (..),
                                Url (..), commonmarkToNode, renderToCommonmark)
import           Data.Scrapbox.Internal (concatSegment, genPrintableUrl,
                                         isSized, runParagraphParser,
                                         shortListOf, unverbose)
import           Utils (propNonNull, shouldParseSpec)

-- | Generate random text
genNoSymbolText :: Gen Text
genNoSymbolText = fromString <$> listOf (arbitraryPrintableChar `suchThat` isLetter)

genNoSymbolText1 :: Gen Text
genNoSymbolText1 = fromString <$> listOf1 (arbitraryPrintableChar `suchThat` isLetter)

commonmarkSpec :: Spec
commonmarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 5000) $ do
    prop "Model test" commonmarkModelTest
    shouldParseSpec runParagraphParser
    prop "should return non-empty list of blocks if the given string is non-empty" $
        propNonNull runParagraphParser id

--------------------------------------------------------------------------------
-- Commonmark model test
--------------------------------------------------------------------------------

data CommonMark
    = ParagraphSection !Text
    | HeaderText !Int !Text
    | TableSection ![Text] ![[Text]]
    | ImageSection !Text !Text
    | OrderedListBlock ![Text]
    | UnorderedListBlock ![Text]
    | BlockQuoteText !Text
    | StyledText !TestStyle !Text
    | CodeBlockSection ![Text]
    | Link !Text !Text
    | CodeNotation !Text
    deriving (Eq, Show, Generic)

data TestStyle
    = BoldStyle
    | ItalicStyle
    | NoStyles
    | StrikeThroughStyle
    deriving (Eq, Enum, Show)

instance Arbitrary TestStyle where
    arbitrary = elements [BoldStyle .. StrikeThroughStyle]

renderCommonmark :: CommonMark -> Text
renderCommonmark = \case
    ParagraphSection text       -> text
    HeaderText level text       -> T.replicate level "#" <> " " <> text
    TableSection header content -> renderTable header content
    ImageSection title someLink -> "![" <> title <> "](" <> someLink <> ")"
    OrderedListBlock list       -> T.unlines $ zipWith
        (\num someText -> tshow num <> ". " <> someText)
        ([1..] :: [Int])
        list
    UnorderedListBlock list     -> T.unlines $ map ("- " <>) list
    BlockQuoteText text         -> ">" <> text
    StyledText style text       -> case style of
        BoldStyle          -> "**" <> text <> "**"
        ItalicStyle        -> "*" <> text <> "*"
        StrikeThroughStyle -> "~~" <> text <> "~~"
        NoStyles           -> text
    CodeBlockSection codes      -> T.unlines $ ["```"] <> codes <> ["```"]
    Link name url               -> "[" <> name <> "](" <> url <> ")"
    CodeNotation notation       -> "`" <> notation <> "`"

renderTable :: [Text] -> [[Text]] -> Text
renderTable header contents = do
    let renderedHeader   = renderColumn header
    let between          = renderBetween' (length header)
    let renderedContents = renderTableContent contents
    T.unlines $ [renderedHeader] <> [between] <> renderedContents
  where
    renderColumn :: [Text] -> Text
    renderColumn = foldl' (\acc a -> acc <> a <> " | ") "| "

    renderBetween' :: Int -> Text
    renderBetween' rowNum' = T.replicate rowNum' "|- " <> "|"

    renderTableContent :: [[Text]] -> [Text]
    renderTableContent = map renderColumn

instance Arbitrary CommonMark where
    arbitrary =
        oneof
          [ ParagraphSection <$> genNoSymbolText
          , HeaderText <$> choose (-6, 6) <*> genNoSymbolText
          , BlockQuoteText <$> genNoSymbolText
          , tableGenerator
          , ImageSection <$> genNoSymbolText <*> genPrintableUrl
          , OrderedListBlock <$> listOf genNoSymbolText1
          , UnorderedListBlock <$> listOf genNoSymbolText1
          , StyledText <$> elements [BoldStyle .. StrikeThroughStyle] <*> genNoSymbolText
          , CodeBlockSection <$> listOf genNoSymbolText
          , Link <$> genNoSymbolText <*> genPrintableUrl
          , CodeNotation <$> genNoSymbolText
          ]
      where
        tableGenerator = do
            rowNum   <- choose (2,10)
            header   <- vectorOf rowNum genNoSymbolText
            contents <- shortListOf $ vectorOf rowNum genNoSymbolText
            return $ TableSection header contents
    shrink = genericShrink

toScrapbox :: CommonMark -> [Block]
toScrapbox = \case
    ParagraphSection text ->
        if T.null text
            then mempty
            else [PARAGRAPH (ScrapText [SPAN [] [TEXT text]])]
    HeaderText hlevel text -> modelHeader hlevel text
    TableSection header content ->
        if null header || null content
            then mempty
            else [TABLE (TableName "table") (TableContent $ [header] <> content)]
    OrderedListBlock content ->
        if null content
            then mempty
            else [BULLET_POINT (Start 1) (mkParagraphs content)]
    UnorderedListBlock content ->
        if null content
            then mempty
            else [BULLET_POINT (Start 1) (mkParagraphs content)]
    ImageSection _t url -> [THUMBNAIL (Url url)]
    StyledText style text -> modelStyledText style text
    BlockQuoteText text ->
        if T.null text
            then [BLOCK_QUOTE (ScrapText [])]
            else [BLOCK_QUOTE (ScrapText [SPAN [] [TEXT text]])]
    CodeBlockSection codeBlocks ->
        [CODE_BLOCK (CodeName "code") (CodeSnippet codeBlocks)]
    Link name url ->
        if T.null name
            then [PARAGRAPH (ScrapText [SPAN [] [LINK Nothing (Url url)]])]
            else [PARAGRAPH (ScrapText [SPAN [] [LINK (Just name) (Url url)]])]
    CodeNotation notation ->
        if T.null notation
            then [PARAGRAPH ( ScrapText [SPAN [] [ TEXT "``" ]])]
            else [PARAGRAPH (ScrapText [CODE_NOTATION notation])]
  where
    mkParagraphs :: [Text] -> [Block]
    mkParagraphs = map toParagraph

    toParagraph c =
        let scrapText =  ScrapText [SPAN [] [TEXT c]]
        in PARAGRAPH scrapText

    toStyle :: TestStyle -> [Style]
    toStyle = \case
        BoldStyle          -> [Bold]
        ItalicStyle        -> [Italic]
        StrikeThroughStyle -> [StrikeThrough]
        NoStyles           -> []

    modelHeader :: Int -> Text -> [Block]
    modelHeader hlevel text
        | hlevel <= 0 && T.null text = mempty
        | hlevel <= 0 = [PARAGRAPH (ScrapText [SPAN [] [TEXT text]])]
        | T.null text = [HEADING (toLevel hlevel) mempty]
        | otherwise = [HEADING (toLevel hlevel) [TEXT text]]

    toLevel :: Int -> Level
    toLevel = \case
        1 -> Level 4
        2 -> Level 3
        3 -> Level 2
        4 -> Level 1
        _ -> Level 1

    modelStyledText :: TestStyle -> Text -> [Block]
    modelStyledText style text        -- Needs to take a look
        | style == BoldStyle && T.null text =
            [PARAGRAPH (ScrapText [SPAN [] [TEXT "\n"]])]
        | style == ItalicStyle && T.null text =
            [ PARAGRAPH (ScrapText [SPAN [] [ TEXT "**" ]])]
        | style == StrikeThroughStyle && T.null text =
            [CODE_BLOCK ( CodeName "code" ) ( CodeSnippet [] )]
        | style == NoStyles && T.null text =
            mempty
        | otherwise = [PARAGRAPH (ScrapText [SPAN (toStyle style) [TEXT text]])]

commonmarkModelTest :: CommonMark -> Property
commonmarkModelTest commonmark =
    let (Scrapbox content) = commonmarkToNode [] . renderCommonmark $ commonmark
    in content === toScrapbox commonmark

commonmarkRoundTripTest :: Block -> Property
commonmarkRoundTripTest block =
    let rendered = renderToCommonmark [] (Scrapbox [block])
        parsed   = commonmarkToNode [] rendered
    in parsed === unverbose (Scrapbox (toModel block))

toModel :: Block -> [Block]
toModel = \case
    BLOCK_QUOTE (ScrapText [SPAN [UserStyle "!?%"] []]) ->
        [BLOCK_QUOTE (ScrapText [])]
    b@(BLOCK_QUOTE (ScrapText [SPAN [] [TEXT text]])) ->
        if T.null (T.stripStart text)
            then [BLOCK_QUOTE (ScrapText [])]
            else [b]
    BLOCK_QUOTE (ScrapText inlines) ->
        if null inlines
            then [BLOCK_QUOTE (ScrapText [])]
            else [BLOCK_QUOTE $ ScrapText $ toInlineModel inlines]
    BULLET_POINT s blocks ->
        [BULLET_POINT s (concatMap toModel blocks)]
    HEADING level segments ->
        [HEADING (toLevel level) (toSegmentModel segments)]
    LINEBREAK -> []

    -- Paragraph
    p@(PARAGRAPH (ScrapText [SPAN [] [TEXT text]])) -> 
        if T.null (T.stripStart text)
            then []
            else [p]
    PARAGRAPH (ScrapText [SPAN [StrikeThrough] []]) ->
        [CODE_BLOCK (CodeName "code") (CodeSnippet [])]
    p@(PARAGRAPH (ScrapText [SPAN [StrikeThrough] [TEXT text]])) ->
        if T.null text
            then [CODE_BLOCK (CodeName "code") (CodeSnippet [])]
            else [p]
    PARAGRAPH (ScrapText [SPAN [Bold] []]) -> emtpyText
    p@(PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]])) ->
        if T.null (T.stripStart text)
            then emtpyText
            else [p]
    PARAGRAPH (ScrapText [SPAN [UserStyle _u] [TEXT ""]]) -> emtpyText
    PARAGRAPH (ScrapText [SPAN [UserStyle "!?%"] [TEXT text]]) ->
        if T.null (T.stripStart text)
            then emtpyText
            else [PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]])]
    PARAGRAPH (ScrapText inlines) ->
        [PARAGRAPH (ScrapText (toInlineModel inlines))]

    -- Table
    TABLE (TableName name) (TableContent [[""]]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT (name <> "|  ||--|")]])]
    TABLE n@(TableName name) c@(TableContent content) ->
        if null content
            then [PARAGRAPH (ScrapText [SPAN [] [TEXT name]])]
            else [TABLE n c]
    THUMBNAIL (Url url) ->
        if isImageUrl url
            then [THUMBNAIL (Url url)]
            else [PARAGRAPH (ScrapText [SPAN [] [LINK Nothing (Url url)]])]
    others    -> [others]
  where
    emtpyText = [PARAGRAPH (ScrapText [SPAN [] [TEXT "\n"]])]


    toInlineModel = foldr (\inline acc -> case inline of
        CODE_NOTATION expr ->
            if T.null expr
                then [SPAN [] [TEXT "``"]] <> acc
                else [CODE_NOTATION expr] <> acc
        MATH_EXPRESSION expr ->
            if T.null expr
                then [SPAN [] [TEXT "``"]] <> acc
                else [CODE_NOTATION expr] <> acc
        SPAN [UserStyle _s] [] -> [SPAN [] [TEXT "\n"]] <> acc
        SPAN [UserStyle _s] content -> [SPAN [Bold] content] <> acc
        SPAN [style] [] -> [SPAN [] [TEXT (renderStyle style)]] <> acc
        SPAN [] [] -> acc
        SPAN [Sized level, someStyle] [] ->
            let renderedText = mconcat
                    [ "<span style=\"font-size:"
                    , tshow (toSize level)
                    , "em\">"
                    , renderStyle someStyle
                    , "</span>"
                    ]
            in [SPAN [] [TEXT renderedText]] <> acc
        SPAN styles [] -> renderEmptyStyledSpan styles acc
        SPAN styles segments ->
            if concatSegment segments == [TEXT ""]
                then renderEmptyStyledSpan styles acc
                else renderStyledSpan styles segments acc
        ) mempty

    renderStyledSpan styles segments acc =
        let sizeSum = 
                foldr (\style acc' -> case style of
                    Sized (Level num) -> num + acc'
                    _others           -> acc'
                    ) 0 
                    $ filter isSized styles   
            size = toSize (Level sizeSum)
            restStyle = filter (not . isSized) styles
        in if sizeSum > 0
            then 
                [ SPAN [] [TEXT ("<span style=\"font-size:" <> tshow size <> "em\">")]
                , SPAN restStyle segments
                , SPAN [] [TEXT "</span>"]
                ] <> acc
            else [SPAN styles segments] <> acc
    renderEmptyStyledSpan styles acc =
        let sizeSum = 
                foldr (\style acc' -> case style of
                    Sized (Level num) -> num + acc'
                    _others           -> acc'
                    ) 0 
                    $ filter isSized styles

            restStyle = filter (not . isSized) styles
        in maybe
            acc
            (\(lastEle, rest) -> if sizeSum > 0
                then 
                    let renderedText = mconcat
                            [ "<span style=\"font-size:"
                            , tshow (toSize (Level sizeSum))
                            , "em\">"
                            , renderStyle lastEle
                            , "</span>"
                            ]
                    in [SPAN rest [TEXT renderedText]] <> acc
                else [SPAN rest [TEXT (renderStyle lastEle)]] <> acc)
            (do
                last <- lastMaybe restStyle
                rest <- initMaybe restStyle
                return (last, rest)
            )
    renderStyle = \case
        Bold -> "****"
        Italic -> "__"
        StrikeThrough -> "~~~~"
        Sized lvl -> "<span style=\"font-size:" <> tshow (toSize lvl) <> "em\"></span>"
        UserStyle _s -> "****"

    toLevel (Level lvl)= case lvl of
            4 -> Level 4
            3 -> Level 3
            2 -> Level 2
            1 -> Level 2
            _ -> Level 1

    toSegmentModel = foldr (\segment acc -> case segment of
        TEXT ""      -> acc
        TEXT " "     -> acc
        TEXT text    -> [TEXT (T.stripStart text)] <> acc
        HASHTAG text -> [TEXT ("#" <> text <> "")] <> acc
        others       -> [others] <> acc ) mempty

    isImageUrl url = any (`T.isSuffixOf` url)
        [ ".bmp"
        , ".gif"
        , ".jpg"
        , ".jpeg"
        , ".png"
        ]

    toSize (Level lvl) = fromIntegral lvl * 0.5 :: Double

-- PARAGRAPH (ScrapText [SPAN [Italic,StrikeThrough] [TEXT "a"]])
-- BLOCK_QUOTE (ScrapText [SPAN [Sized (Level 3),Italic,StrikeThrough] []])
checkRoundTrip :: Block -> (Block, Text, C.Node, Scrapbox)
checkRoundTrip block = 
    let rendered = renderToCommonmark [] (Scrapbox [block])
        parsed   = commonmarkToNode [] rendered
        parsed'  = C.commonmarkToNode [] rendered
    in (block, rendered, parsed', parsed)