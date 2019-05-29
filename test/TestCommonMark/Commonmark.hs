{-| Test suites for commonmark parser
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Commonmark where

import           RIO

import qualified CMark as C
import           Data.Char (isLetter, isSpace)
import           RIO.List (zipWith, lastMaybe, initMaybe)
import qualified RIO.Text as T
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property,
                                  arbitraryPrintableChar, choose, elements,
                                  genericShrink, listOf, listOf1, oneof,
                                  suchThat, vectorOf, (===), (==>))

import           Data.Scrapbox (Block (..), CodeName (..), CodeSnippet (..),
                                InlineBlock (..), Level (..), ScrapText (..),
                                Scrapbox (..), Segment (..), Start (..),
                                Style (..), TableContent (..), TableName (..),
                                Url (..), commonmarkToNode, renderToCommonmark)
import           Data.Scrapbox.Internal (concatSegment, genPrintableUrl,
                                         isBulletPoint, isTable,
                                         isText, renderInlineBlock,
                                         runParagraphParser, shortListOf,
                                         unverbose, isSized)
import           Utils (propNonNull, shouldParseSpec)

commonmarkSpec :: Spec
commonmarkSpec = describe "CommonMark parser" $ modifyMaxSuccess (const 5000) $ do
    prop "Model test" commonmarkModelTest
    shouldParseSpec runParagraphParser
    prop "should return non-empty list of blocks if the given string is non-empty" $
        propNonNull runParagraphParser id

--------------------------------------------------------------------------------
-- Commonmark model test
--------------------------------------------------------------------------------

-- | Syntaxes in commonmark
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

-- | Styles in commonmark
data TestStyle
    = BoldStyle
    | ItalicStyle
    | NoStyles
    | StrikeThroughStyle
    deriving (Eq, Enum, Show)

--------------------------------------------------------------------------------
-- Defining generators
--------------------------------------------------------------------------------

-- | Generate random text
genNoSymbolText :: Gen Text
genNoSymbolText = fromString <$> listOf (arbitraryPrintableChar `suchThat` isLetter)

-- | Generate random text withoug null
genNoSymbolText1 :: Gen Text
genNoSymbolText1 = fromString <$> listOf1 (arbitraryPrintableChar `suchThat` isLetter)

instance Arbitrary TestStyle where
    arbitrary = elements [BoldStyle .. StrikeThroughStyle]

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

--------------------------------------------------------------------------------
-- Renderer
--------------------------------------------------------------------------------

-- | Renderer for @Commonmark@
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
  where
    renderTable :: [Text] -> [[Text]] -> Text
    renderTable header contents = do
        let renderedHeader   = renderColumn header
        let between          = renderBetween' (length header)
        let renderedContents = renderTableContent contents
        T.unlines $ [renderedHeader] <> [between] <> renderedContents
    renderColumn :: [Text] -> Text
    renderColumn = foldl' (\acc a -> acc <> a <> " | ") "| "

    renderBetween' :: Int -> Text
    renderBetween' rowNum' = T.replicate rowNum' "|- " <> "|"

    renderTableContent :: [[Text]] -> [Text]
    renderTableContent = map renderColumn

--------------------------------------------------------------------------------
-- Modeling
--------------------------------------------------------------------------------

modelScrapbox :: CommonMark -> [Block]
modelScrapbox = \case
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
        | T.null text = [HEADING (headerLevel hlevel) mempty]
        | otherwise = [HEADING (headerLevel hlevel) [TEXT text]]

    headerLevel :: Int -> Level
    headerLevel = \case
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

--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

commonmarkModelTest :: CommonMark -> Property
commonmarkModelTest commonmark =
    let (Scrapbox content) = commonmarkToNode [] . renderCommonmark $ commonmark
    in content === modelScrapbox commonmark

--------------------------------------------------------------------------------
-- Commonmark roundtrip test
--------------------------------------------------------------------------------

commonmarkRoundTripTest :: Block -> Property
commonmarkRoundTripTest block = ((not . isTable) block && (not . isBulletPoint) block) ==>
    let rendered = renderToCommonmark [] (Scrapbox [block])
        parsed   = commonmarkToNode [] rendered
    in parsed === unverbose (Scrapbox (toRoundTripModel block))

toRoundTripModel :: Block -> [Block]
toRoundTripModel = \case
    BLOCK_QUOTE (ScrapText [SPAN [Bold] []]) -> emptyQuote
    b@(BLOCK_QUOTE (ScrapText [SPAN [Bold] segments])) ->
        if isEmptySegments segments
            then emptyQuote
            else [b]
    BLOCK_QUOTE (ScrapText [SPAN [UserStyle _u] []]) -> emptyQuote
    BLOCK_QUOTE (ScrapText [SPAN [UserStyle _u] segments]) ->
        if isEmptySegments segments
            then emptyQuote
            else [BLOCK_QUOTE (ScrapText (toInlineModel [SPAN [Bold] segments]))]
     -- StrikeThrough parsed as CODE_BLOCKa
    BLOCK_QUOTE (ScrapText [SPAN [StrikeThrough] []]) -> emptyQuote
    BLOCK_QUOTE (ScrapText (SPAN [] (TEXT text : rest) : rest'))->
        [ BLOCK_QUOTE
            ( ScrapText $ toInlineModel
                ( SPAN []
                    ( toSegmentModel (TEXT (T.stripStart text) : rest) )
                : rest'
                )
            )
        ]
    BLOCK_QUOTE (ScrapText inlines) ->
        if null inlines
            then emptyQuote
            else [BLOCK_QUOTE $ ScrapText $ toInlineModel inlines]

    BULLET_POINT s blocks ->
        [BULLET_POINT s (concatMap toRoundTripModel blocks)]

    -- HEADING
    HEADING level (TEXT text:rest) ->
        let head | not . T.null . T.stripStart $ text = 
                    HEADING (toLevel level) (toSegmentModel (TEXT (T.stripStart text) : rest))
                 | isEmptySegments (TEXT text : rest) = HEADING (toLevel level) []
                 | otherwise =  HEADING (toLevel level) (toSegmentModel rest)
        in [head]
    HEADING level segments ->
        if isEmptySegments segments
            then [HEADING (toLevel level) []]
            else [HEADING (toLevel level) (toSegmentModel segments)]
    LINEBREAK -> []

    -- Paragraph
    PARAGRAPH (ScrapText [SPAN [Sized _level,Italic] [TEXT ""]]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "__"]])]
    p@(PARAGRAPH (ScrapText [SPAN [] [LINK (Just name) url]])) ->
        if T.all isSpace name || T.null name || isImageUrl url
            then [THUMBNAIL url]
            else [p]
    PARAGRAPH (ScrapText [SPAN [] segments]) ->
        if isEmptySegments segments
            then []
            else [PARAGRAPH (ScrapText (toInlineModel [SPAN [] segments]))]
    PARAGRAPH (ScrapText [SPAN [UserStyle "!?%"] []]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "\n"]])]
    PARAGRAPH (ScrapText [SPAN [Bold] []]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "\n"]])]
    PARAGRAPH (ScrapText [SPAN [StrikeThrough] []]) ->
        [CODE_BLOCK (CodeName "code") (CodeSnippet [])]
    p@(PARAGRAPH (ScrapText [SPAN [StrikeThrough] [TEXT text]])) ->
        if T.null text
            then [CODE_BLOCK (CodeName "code") (CodeSnippet [])]
            else [p]
    PARAGRAPH (ScrapText (SPAN [StrikeThrough] []:rest)) ->
        [CODE_BLOCK (CodeName (foldr ((<>) . renderInlineBlock) mempty rest)) (CodeSnippet [])]
    PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]]) ->
        if T.null (T.stripStart text)
            then emptyText
            else [PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]])]
    PARAGRAPH (ScrapText [SPAN [UserStyle _s] [TEXT text]]) ->
        if T.null (T.stripStart text)
            then emptyText
            else [PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]])]
    PARAGRAPH (ScrapText (SPAN [UserStyle _s] []:rest)) ->
        [PARAGRAPH (ScrapText (SPAN [] [TEXT "****"] : toInlineModel rest))]

    PARAGRAPH (ScrapText [MATH_EXPRESSION "", CODE_NOTATION ""]) ->
        [PARAGRAPH (ScrapText [CODE_NOTATION ""])]
    PARAGRAPH (ScrapText [CODE_NOTATION "", MATH_EXPRESSION ""]) ->
        [PARAGRAPH (ScrapText [CODE_NOTATION ""])]
    PARAGRAPH (ScrapText [CODE_NOTATION "", CODE_NOTATION ""]) ->
        [PARAGRAPH (ScrapText [CODE_NOTATION ""])]
    PARAGRAPH (ScrapText [MATH_EXPRESSION "", MATH_EXPRESSION ""]) ->
        [PARAGRAPH (ScrapText [CODE_NOTATION ""])]

    PARAGRAPH (ScrapText [CODE_NOTATION "", CODE_NOTATION text2]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "`` "],CODE_NOTATION text2])]
    PARAGRAPH (ScrapText [MATH_EXPRESSION "", MATH_EXPRESSION text2]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "`` "],CODE_NOTATION text2])]
    PARAGRAPH (ScrapText [MATH_EXPRESSION "", CODE_NOTATION text2]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "`` "],CODE_NOTATION text2])]
    PARAGRAPH (ScrapText [CODE_NOTATION "", MATH_EXPRESSION text2]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "`` "],CODE_NOTATION text2])]

    PARAGRAPH (ScrapText inlines) ->
        [PARAGRAPH (ScrapText (toInlineModel inlines))]

    -- Table
    TABLE (TableName name) (TableContent [[""]]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT (name <> "|  ||--|")]])]
    TABLE n@(TableName name) c@(TableContent content) ->
        if null content
            then [PARAGRAPH (ScrapText [SPAN [] [TEXT name]])]
            else [TABLE n c]
    THUMBNAIL url ->
        if isImageUrl url
            then [THUMBNAIL url]
            else [PARAGRAPH (ScrapText [SPAN [] [LINK Nothing url]])]
    others    -> [others]
  where
    emptyQuote = [BLOCK_QUOTE (ScrapText [])]
    emptyText = [PARAGRAPH (ScrapText [SPAN [] [TEXT "\n"]])]

toInlineModel :: [InlineBlock] -> [InlineBlock]
toInlineModel [SPAN [Sized _level] segments] =
    if isEmptySegments segments
        then []
        else [SPAN [] segments]
toInlineModel inlines
    | isAllBolds inlines = [SPAN [] [TEXT "\n"]] -- [SPAN [Bold] [],SPAN [UserStyle "!?%"] [TEXT ""]]
    | otherwise          =
    let spaceAddedInlines = removeTrailingSpaces . filterSize . addSpaces $ inlines
    in foldr (\inline acc -> case inline of
        CODE_NOTATION expr ->
            if T.null expr
                then [SPAN [] [TEXT "``"]] <> acc
                else [CODE_NOTATION expr] <> acc
        MATH_EXPRESSION expr ->
            if T.null expr
                then [SPAN [] [TEXT "``"]] <> acc
                else [CODE_NOTATION expr] <> acc
        -- SPAN
        SPAN [] [] -> acc
        SPAN [UserStyle _s] segments ->
            if isEmptySegments segments
                then [SPAN [] [TEXT "****"]] <> acc
                else modelSpan segments [Bold] <> acc
        SPAN styles []  -> maybe
                ([SPAN styles []] <> acc)
                (\(last, init) -> [SPAN init [TEXT (renderStyle last)]] <> acc)
                (do
                    last <- lastMaybe styles
                    init <- initMaybe styles
                    return (last, init)
                )
        SPAN styles segments -> modelSpan segments styles <> acc
    ) mempty spaceAddedInlines
  where
    addSpaces :: [InlineBlock] -> [InlineBlock]
    addSpaces []  = []
    addSpaces [x] = [x]
    addSpaces (SPAN [] segments : xs ) = if isSpaces segments
        then SPAN [] segments : xs
        else SPAN [] (segments <> [TEXT " "]) : addSpaces xs
    addSpaces (x:xs) = x : SPAN [] [TEXT " "] : addSpaces xs

    filterSize :: [InlineBlock] -> [InlineBlock]
    filterSize [] = []
    filterSize (SPAN styles segments : xs) = 
        SPAN (filter (not . isSized) styles) segments : filterSize xs
    filterSize (x : xs)                    = x : filterSize xs

    isSpaces = all (\case
        TEXT text -> T.null (T.strip text)
        _others    -> False
        )

removeTrailingSpaces :: [InlineBlock] -> [InlineBlock]
removeTrailingSpaces is = maybe
    is
    (\(initSeg, lastSeg, initInline) ->
        let lastSeg' = case lastSeg of
                        TEXT text -> if T.null (T.stripEnd text)
                            then mempty
                            else [TEXT (T.stripEnd text)]
                        others    -> [others]
        in initInline <> [SPAN [] (initSeg <> lastSeg')]
    )
    (do
        lastInline <- lastMaybe is
        segments   <- getSegments lastInline
        initSeg    <- initMaybe segments
        lastSeg    <- lastMaybe segments
        initInline <- initMaybe is
        return (initSeg, lastSeg, initInline)
    )

getSegments :: InlineBlock -> Maybe [Segment]
getSegments (SPAN [] segments) = Just segments
getSegments _                  = Nothing

-- Need to remove trailing spaces
toSegmentModel :: [Segment] -> [Segment]
toSegmentModel segments =
    let spaceAddedSegments = adjustSpaces segments
    in foldr (\segment acc -> case segment of
        HASHTAG text -> [TEXT ("#" <> text)] <> acc
        others       -> [others] <> acc
        )
        mempty
        spaceAddedSegments
  where
    adjustSpaces :: [Segment] -> [Segment]
    adjustSpaces [] = []
    adjustSpaces (h1@(HASHTAG _t1) : h2@(HASHTAG _t2) : rest) = h1 : TEXT " " : adjustSpaces (h2 : rest)
    adjustSpaces (h1@(HASHTAG _t) : rest) = if isEmptySegments rest
        then [h1]
        else h1 : adjustSpaces rest
    adjustSpaces (x:xs) = x : adjustSpaces xs

modelSpan :: [Segment] -> [Style] -> [InlineBlock]
modelSpan segments styles = foldr (\segment acc -> case segment of
    TEXT text -> if T.null text
        then acc
        else [SPAN styles [TEXT text]] <> acc
    HASHTAG tag -> [SPAN (Bold : styles) [TEXT ("#" <> tag)]] <> acc
    others -> [SPAN styles [others]] <> acc
    ) mempty segments

styledTextModel :: [Style] -> [Segment] -> [InlineBlock]
styledTextModel styles segments
  | isEmptySegments segments            = []
  | otherwise                           = modelSpan segments styles

renderStyle :: Style -> Text
renderStyle = \case
    Bold          -> "****"
    Italic        -> "__"
    StrikeThrough -> "~~~~"
    Sized _lvl    -> ""
    UserStyle _s  -> "****"

toLevel :: Level -> Level
toLevel (Level lvl)= case lvl of
    4 -> Level 4
    3 -> Level 3
    2 -> Level 2
    1 -> Level 2
    _ -> Level 1

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

isImageUrl :: Url -> Bool
isImageUrl (Url url) = any (`T.isSuffixOf` url)
    [ ".bmp"
    , ".gif"
    , ".jpg"
    , ".jpeg"
    , ".png"
    ]

isEmptySegments :: [Segment] -> Bool
isEmptySegments segments =
    let concatedSegments = concatSegment segments
        isAllText        = all isText concatedSegments
        someBool         = foldr (\segment acc -> case segment of
                                    TEXT text -> T.null (T.stripStart text) && acc
                                    _others   -> False
                                ) True concatedSegments
    in isAllText && someBool

isAllBolds :: [InlineBlock] -> Bool
isAllBolds = all checkBolds
    where
      checkBolds = \case
         SPAN [Bold] segments ->
            null segments || (any isText segments && all isEmptyText segments)
         SPAN [UserStyle _s] segments ->
            null segments || (any isText segments && all isEmptyText segments)
         _others -> False
      isEmptyText = \case
        TEXT text -> T.null $ T.strip text
        _others   -> False

--------------------------------------------------------------------------------
-- Checker
--------------------------------------------------------------------------------

-- BLOCK_QUOTE (ScrapText [SPAN [Sized (Level 3),Italic,StrikeThrough] []])
-- TABLE (TableName "(") (TableContent [["a"]])
-- PARAGRAPH (ScrapText [SPAN [UserStyle "!?%"] [],CODE_NOTATION ""])
-- PARAGRAPH (ScrapText [CODE_NOTATION "",CODE_NOTATION "a"])
-- PARAGRAPH (ScrapText [SPAN [UserStyle "!?%"] [],SPAN [Bold] []])
checkCommonmarkRoundTrip :: Block -> (Block, Text, C.Node, Scrapbox, Scrapbox, Bool)
checkCommonmarkRoundTrip block =
    let rendered = renderToCommonmark [] (Scrapbox [block])
        parsed   = commonmarkToNode [] rendered
        parsed'  = C.commonmarkToNode [] rendered
        modeled  = toRoundTripModel block
    in ( block
       , rendered
       , parsed'
       , parsed
       , unverbose . Scrapbox $ modeled
       , parsed == unverbose (Scrapbox modeled)
       )
