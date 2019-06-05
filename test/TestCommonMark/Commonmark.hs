{-| Test suites for commonmark parser
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestCommonMark.Commonmark
    ( commonmarkSpec
    , commonmarkRoundTripTest
    , commonmarkModelTest
    , checkCommonmarkRoundTrip
    , toBulletPointModel
    , toInlineModel
    , toRoundTripModel
    ) where

import           RIO

import qualified CMarkGFM as C
import           Data.Char (isLetter, isSpace)
import           Data.Scrapbox (Block (..), CodeName (..), CodeSnippet (..),
                                InlineBlock (..), Level (..), ScrapText (..),
                                Scrapbox (..), Segment (..), Start (..),
                                Style (..), TableContent (..), TableName (..),
                                Url (..), commonmarkToNode, renderToCommonmark)
import           Data.Scrapbox.Internal (concatSegment, genPrintableUrl, isBold,
                                         isSized, isText, shortListOf,
                                         unverbose)
import           Data.Scrapbox.Render.Commonmark (renderBlock,
                                                  renderInlineBlock)
import           RIO.List (headMaybe, initMaybe, lastMaybe, tailMaybe, zipWith, maximumMaybe)
import qualified RIO.Text as T
import           Test.Hspec (Spec)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property,
                                  arbitraryPrintableChar, choose, elements,
                                  genericShrink, listOf, listOf1, oneof,
                                  suchThat, vectorOf, (===), (==>))

commonmarkSpec :: Spec
commonmarkSpec = modifyMaxSuccess (const 5000) $ do
    prop "Model test" commonmarkModelTest
    prop "Round trip test" commonmarkRoundTripTest

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
    ImageSection t url ->
        if T.null t
            then [PARAGRAPH (ScrapText [SPAN [] [LINK Nothing (Url url)]])]
            else [PARAGRAPH (ScrapText [SPAN [] [LINK (Just t) (Url url)]])]
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
    mkParagraphs = map (\c -> PARAGRAPH $ ScrapText [SPAN [] [TEXT c]])

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
             -- **** is parsed into THEMATIC BREAK
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
    let scrapbox = commonmarkToNode [] . renderCommonmark $ commonmark
    in scrapbox === Scrapbox (modelScrapbox commonmark)

--------------------------------------------------------------------------------
-- Commonmark roundtrip test
--------------------------------------------------------------------------------

commonmarkRoundTripTest :: Block -> Property
commonmarkRoundTripTest block = lessThanOneElement block ==>
    let rendered = renderToCommonmark [] (Scrapbox [block])
        parsed   = commonmarkToNode [] rendered
    in parsed === unverbose (Scrapbox (toRoundTripModel block))
  where
    lessThanOneElement :: Block -> Bool
    lessThanOneElement (BULLET_POINT _start blocks) =
        length blocks <= 1 && all lessThanOneElement blocks
    lessThanOneElement _others = True

toRoundTripModel :: Block -> [Block]
toRoundTripModel = \case
    -- Need to fix this in the future
    BULLET_POINT _s1 [BULLET_POINT _s2 blocks] ->
        foldl' (\acc block -> case block of
            t@(TABLE _n _c)      -> acc <> toRoundTripModel t
            c@(CODE_BLOCK _n _c) -> acc <> toRoundTripModel c
            b -> let rendered = map ("- " <> ) $ renderBlock b
                     code     = CODE_BLOCK (CodeName "code") (CodeSnippet rendered)
                 in acc <> [code]
            ) mempty blocks

    BULLET_POINT _start blocks -> toBulletPointModel (Start 0) blocks

    BLOCK_QUOTE (ScrapText [SPAN [Bold] []]) -> emptyQuote
    BLOCK_QUOTE (ScrapText [SPAN [Bold] segments]) ->
        if isEmptySegments segments
            then emptyQuote
            else [BLOCK_QUOTE (ScrapText (toInlineModel [SPAN [Bold] segments]))]
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

    -- HEADING
    HEADING level (TEXT text:rest) ->
        let head | not . T.null . T.stripStart $ text =
                    HEADING (toLevelModel level) (toSegmentModel (TEXT (T.stripStart text) : rest))
                 | isEmptySegments (TEXT text : rest) = HEADING (toLevelModel level)  []
                 | otherwise =  HEADING (toLevelModel level) (toSegmentModel rest)
        in [head]
    HEADING level segments ->
        if isEmptySegments segments
            then [HEADING (toLevelModel level) []]
            else [HEADING (toLevelModel level) (toSegmentModel segments)]
    LINEBREAK -> []

    -- Paragraph
    PARAGRAPH (ScrapText inlines@[SPAN [bolds] [], SPAN styles []]) ->
        let b | (isBold bolds || isSized bolds) && any (\s -> isBold s || isSized s) styles  =
                PARAGRAPH $ ScrapText (SPAN [] [TEXT "**** "] : renderWithStyles (filter (not . isSized) styles))
              | otherwise = PARAGRAPH (ScrapText (toInlineModel inlines))
        in [b]
    PARAGRAPH (ScrapText [SPAN [Sized _level,Italic] [TEXT ""]]) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT "__"]])]
    PARAGRAPH (ScrapText [SPAN [] segments]) ->
        if isEmptySegments segments
            then []
            else [PARAGRAPH (ScrapText (toInlineModel [SPAN [] segments]))]
    PARAGRAPH (ScrapText [SPAN [UserStyle _u] []]) ->
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
        [ CODE_BLOCK
            (CodeName (foldr ((<>) . renderInlineBlock) mempty rest))
            (CodeSnippet [])
        ]
    PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]]) ->
        if T.null (T.stripStart text)
            then emptyText
            else [PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]])]
    PARAGRAPH (ScrapText [SPAN [UserStyle _s] [TEXT text]]) ->
        if T.null (T.stripStart text)
            then emptyText
            else [PARAGRAPH (ScrapText [SPAN [Bold] [TEXT text]])]

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
    TABLE (TableName name) (TableContent []) ->
        [PARAGRAPH (ScrapText [SPAN [] [TEXT name]])]
    TABLE (TableName name) (TableContent contents) ->
        [ PARAGRAPH (ScrapText [SPAN [] [TEXT name]])
        , LINEBREAK
        , TABLE (TableName "table") (TableContent $ alignTable $ map (fmap T.strip) contents)
        ]
    THUMBNAIL url -> [PARAGRAPH (ScrapText [SPAN [] [LINK Nothing url]])]
    others    -> [others]
  where
    emptyQuote = [BLOCK_QUOTE (ScrapText [])]
    emptyText  = [PARAGRAPH (ScrapText [SPAN [] [TEXT "\n"]])]
    toLevelModel (Level num)
      | num >= 5  = Level 1
      | otherwise = Level num

alignTable :: [[Text]] -> [[Text]]
alignTable rows = maybe rows align (maximumMaybe $ map length rows)
  where
    align :: Int -> [[Text]]
    align maxRow = foldl' (\acc row -> if length row < maxRow
        then acc <> [row <> replicate (maxRow - length row) ""]
        else acc <> [row]
        ) mempty rows

toInlineModel :: [InlineBlock] -> [InlineBlock]
toInlineModel [SPAN [Sized _level] segments] =
    if isEmptySegments segments
        then []
        else modelSpan [] segments
toInlineModel inlines
    | isAllBolds inlines  = [SPAN [] [TEXT "\n"]] -- [SPAN [Bold] [],SPAN [UserStyle "!?%"] [TEXT ""]]
    | otherwise =
    let modifiedInlines = removeTrailingSpaces . filterSize . addSpaces . filterHead $ inlines
    in foldr (\inline acc -> case inline of
        CODE_NOTATION expr -> toExpr expr <> acc
        MATH_EXPRESSION expr -> toExpr expr <> acc
        -- SPAN
        SPAN [] [] -> acc
        SPAN [Italic] [TEXT ""] ->
            [SPAN [] [TEXT "__"]] <> acc
        SPAN [Bold] segments ->
            if isEmptySegments segments
                then [SPAN [] [TEXT "****"]] <> acc
                else modelSpan [Bold] segments <> acc
        SPAN [UserStyle _s] segments ->
            if isEmptySegments segments
                then [SPAN [] [TEXT "****"]] <> acc
                else modelSpan [Bold] segments <> acc
        SPAN styles []       -> renderWithStyles styles <> acc
        SPAN styles segments -> modelSpan styles segments <> acc
    ) mempty modifiedInlines
  where

    addSpaces :: [InlineBlock] -> [InlineBlock]
    addSpaces []                       = []
    addSpaces [x]                      = [x]
    addSpaces (SPAN [] [] : xs)        = addSpaces xs
    addSpaces (SPAN [] [TEXT ""] : xs) = addSpaces xs
    addSpaces (x:xs)                   = x : SPAN [] [TEXT " "] : addSpaces xs

    filterSize :: [InlineBlock] -> [InlineBlock]
    filterSize [] = []
    filterSize (SPAN styles segments : xs) =
        SPAN (filter (not . isSized) styles) segments : filterSize xs
    filterSize (x : xs)                    = x : filterSize xs

    filterHead :: [InlineBlock] -> [InlineBlock]
    filterHead [] = []
    filterHead (SPAN [] (TEXT text : rest) : xs) =
        SPAN [] (TEXT (T.stripStart text) : rest) : xs
    filterHead others = others

    toExpr :: Text -> [InlineBlock]
    toExpr expr
        | T.null expr        = [SPAN [] [TEXT "``"]]
        | T.all isSpace expr = [CODE_NOTATION (T.filter (/= ' ') expr)]
        | otherwise          = (:[]) $ CODE_NOTATION expr

removeTrailingSpaces :: [InlineBlock] -> [InlineBlock]
removeTrailingSpaces inlines = maybe
    inlines
    (\(initSeg, lastSeg, initInline) ->
        let lastSeg' = case lastSeg of
                        TEXT text -> if T.null (T.stripEnd text)
                            then mempty
                            else [TEXT (T.stripEnd text)]
                        others    -> [others]
        in initInline <> [SPAN [] (initSeg <> lastSeg')]
    )
    (do
        lastInline <- lastMaybe inlines
        segments   <- getSegments lastInline
        initSeg    <- initMaybe segments
        lastSeg    <- lastMaybe segments
        initInline <- initMaybe inlines
        return (initSeg, lastSeg, initInline)
    )

getSegments :: InlineBlock -> Maybe [Segment]
getSegments (SPAN [] segments) = Just segments
getSegments _                  = Nothing

-- Need to remove trailing spaces
toSegmentModel :: [Segment] -> [Segment]
toSegmentModel segments =
    let spaceAddedSegments = remove . adjustSpaces $ segments
    in foldr (\segment acc -> case segment of
        HASHTAG text -> [TEXT ("#" <> text)] <> acc
        TEXT ""      -> acc
        others       -> [others] <> acc
        )
        mempty
        spaceAddedSegments
  where
    adjustSpaces :: [Segment] -> [Segment]
    adjustSpaces [] = []
    adjustSpaces [x] = [x]
    adjustSpaces (h1@(HASHTAG _t1) : h2@(HASHTAG _t2) : rest) = h1 : TEXT " " : adjustSpaces (h2 : rest)
    adjustSpaces (h1@(HASHTAG _t) : rest) = if isEmptySegments rest
        then [h1]
        else h1 : adjustSpaces rest
    adjustSpaces (x:xs) = x : adjustSpaces xs

    remove :: [Segment] -> [Segment]
    remove [] = []
    remove [TEXT text] = [TEXT (T.stripEnd text)]
    remove xs = maybe
        xs
        (\(init, last) -> case last of
            TEXT text -> if T.null (T.stripEnd text)
                then init
                else init <> [TEXT (T.stripEnd text)]
            _others   -> xs
        )
        (do
            last <- lastMaybe xs
            init <- initMaybe xs
            return (init, last)
        )

modelSpan :: [Style] -> [Segment] -> [InlineBlock]
modelSpan styles segments = foldr (\segment acc -> case segment of
    TEXT text ->
      let b | T.null text
            && (not . isEmptySegments $ segments) = acc
            | T.null text && (not . null) styles && isEmptySegments segments
                = renderWithStyles styles <> acc
            | otherwise = [SPAN styles [TEXT text]] <> acc
      in b
    HASHTAG tag -> [SPAN styles [TEXT ("#" <> tag)]] <> acc
    LINK (Just "") url -> [SPAN styles [LINK Nothing url]] <> acc
    others -> [SPAN styles [others]] <> acc
    ) mempty segments

renderStyle :: [Style] -> Text
renderStyle = foldr (\style acc -> case style of
    Bold          -> "**" <> acc
    Italic        -> "_" <> acc
    StrikeThrough -> "~~" <> acc
    Sized _style  -> acc
    UserStyle _s  -> "**" <> acc
    ) mempty

renderWithStyles :: [Style] -> [InlineBlock]
renderWithStyles styles = maybe
    [SPAN styles []]
    (\(last, init) -> [SPAN init [TEXT (renderStyle [last] <> T.reverse (renderStyle [last]))]])
    (do
        last <- lastMaybe styles
        init <- initMaybe styles
        return (last, init)
    )

toBulletPointModel :: Start -> [Block] -> [Block]
toBulletPointModel (Start num) = foldr (\block acc -> case block of
    c@(CODE_BLOCK _name _snippet) ->
        if null acc
            then toRoundTripModel c <> acc
            else toRoundTripModel c <> [LINEBREAK] <> acc
    t@(TABLE _name _content) ->
        let b | null acc = toRoundTripModel t <> acc
              | otherwise = toRoundTripModel t <> [LINEBREAK] <> acc
        in b
    LINEBREAK -> maybe
        [BULLET_POINT (Start 1) []]
        (\(head, rest) -> case head of
            BULLET_POINT _s _b -> head : rest
            _others            -> BULLET_POINT (Start 1) [] : head : rest
        )
        ((,) <$> headMaybe acc <*> tailMaybe acc)
    BULLET_POINT _s blocks ->
        toBulletPointModel (Start $ num + 1) blocks
    others -> maybe
        [BULLET_POINT (Start 1) (toRoundTripModel others)]
        (\(head, rest) -> case head of
            BULLET_POINT s b   -> BULLET_POINT s (toRoundTripModel others <> b) : rest
            _others            -> BULLET_POINT (Start 1) (toRoundTripModel others) : head : rest
        )
        ((,) <$> headMaybe acc <*> tailMaybe acc)
    ) mempty

--------------------------------------------------------------------------------
-- Predicates
--------------------------------------------------------------------------------

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
         SPAN [] segments -> null segments || concatSegment segments == [TEXT ""]
         _others -> False
      isEmptyText = \case
        TEXT text -> T.null $ T.strip text
        _others   -> False

--------------------------------------------------------------------------------
-- Checker
--------------------------------------------------------------------------------

-- (BULLET_POINT (Start 3) [BULLET_POINT (Start 1) [TABLE (TableName "a1a39YiPa") (TableContent [["e"]]),LINEBREAK]])
checkCommonmarkRoundTrip :: Block -> (Block, Text, C.Node, Scrapbox, Scrapbox, Bool)
checkCommonmarkRoundTrip block =
    let rendered = renderToCommonmark [] (Scrapbox [block])
        parsed   = commonmarkToNode [] rendered
        parsed'  = C.commonmarkToNode [] exts rendered
        modeled  = toRoundTripModel block
    in ( block
       , rendered
       , parsed'
       , parsed
       , unverbose . Scrapbox $ modeled
       , parsed == unverbose (Scrapbox modeled)
       )
  where
    exts :: [C.CMarkExtension]
    exts = [
          C.extStrikethrough
        , C.extTable
        , C.extAutolink
        , C.extTagfilter
        ]

