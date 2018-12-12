{-# LANGUAGE OverloadedStrings #-}

module Example where

import RIO

import Types
import Lib

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

simpleTextBlock :: Text -> Block
simpleTextBlock text = Simple $ ScrapText [Scrap NoStyle [PlainText text]]

bulletLineWithText :: Int -> Text -> Block
bulletLineWithText num text = BulletLine num $ ScrapText $ [Scrap NoStyle [PlainText text]]

scrapWithPlainText :: Text -> Scrap
scrapWithPlainText text = Scrap NoStyle [PlainText text]

breakLine :: Block
breakLine = BreakLine

--------------------------------------------------------------------------------
-- Blocks
--------------------------------------------------------------------------------

-- "Get started",
getStarted :: Block
getStarted = simpleTextBlock "Get started"

-- "[https://gyazo.com/5f93e65a3b979ae5333aca4f32600611]",
startedThumbnail :: Block
startedThumbnail = Thumbnail (Url "https://gyazo.com/5f93e65a3b979ae5333aca4f32600611")

-- "Welcome to your new Scrapbox project!",
welcome :: Block
welcome = simpleTextBlock "Welcome to your new Scrapbox project!"

-- "[** 📝Everything is editable]"
everythingIs :: Block
everythingIs = Header 2 [PlainText "Everything is editable"]

-- "\tClick on any line and start typing to edit. "
clickOn :: Block
clickOn = bulletLineWithText 1 "Click on any line and start typing to edit. "

-- "\t\tPress tab at the beginning of a line to indent and add a bullet point."
pressTab :: Block
pressTab = bulletLineWithText 2 "Press tab at the beginning of a line to indent and add a bullet point."

-- " Highlight text to make it a [new link], [* bold], [- and] [/ more]."
highlightText :: Block
highlightText = BulletLine 1 $ ScrapText [highlight, newLink, column, bold, column, crossed, space, italic, period]
  where
    highlight :: Scrap
    highlight = scrapWithPlainText " Highlight text to make it a "
    newLink :: Scrap
    newLink = Scrap NoStyle [Link Nothing (Url "New link")]
    column :: Scrap
    column = scrapWithPlainText ", "
    space :: Scrap
    space = scrapWithPlainText " "
    period :: Scrap
    period = scrapWithPlainText "."
    bold :: Scrap
    bold = Scrap Bold [PlainText "bold"]
    crossed :: Scrap
    crossed = Scrap StrikeThrough [PlainText "and"]
    italic :: Scrap
    italic = Scrap Italic [PlainText "more"]

-- "\t\tAdd links while typing with a `#` before or brackets around `[`words you want to link `]` "
addLinks :: Block 
addLinks = BulletLine 2 $ ScrapText [addingLinks, symbol, beforeOr, column1, wordsYouWant, column2] 
  where
    addingLinks  = scrapWithPlainText "Add links while typing with a "
    symbol       = Scrap NoStyle [CodeNotation "#"]
    beforeOr     = scrapWithPlainText " before or brackets around "
    column1      = Scrap NoStyle [CodeNotation "["]
    wordsYouWant = scrapWithPlainText "words you want to link "
    column2      = Scrap NoStyle [CodeNotation "]"]

-- "[** 🎯 Here is where it gets interesting ]",
hereIs :: Block
hereIs = Header 2 [PlainText "Here is where it gets interesting "]

-- "\tClick a [new link] to create a new page with that title and open it.",
clickNewLink :: Block
clickNewLink = BulletLine 1 $ ScrapText [clickA, newLink, toCreate]
  where
    clickA   = scrapWithPlainText "Click a "
    newLink  = Scrap NoStyle [Link Nothing (Url "New Link")]
    toCreate = scrapWithPlainText " to create a new page with that title and open it."

-- Click related thumbnails in the footer of any page to explore ideas you have linked.
clickRelated :: Block
clickRelated = simpleTextBlock "Click related thumbnails in the footer of any page to explore ideas you have linked."

-- " \tPages that are directly linked or two steps away from the current page will be displayed.",
pagesThat :: Block
pagesThat = bulletLineWithText 2 "Pages that are directly linked or two steps away from the current page will be displayed."

-- "\tSee images, videos, and external links added inside `[` brackets`]` on the page",
seeImages :: Block
seeImages = BulletLine 1 $ ScrapText [seeImagesText, column1, brackets, column2, onThePage]
  where
    seeImagesText = scrapWithPlainText "See images, videos, and external links added inside "
    column1       = Scrap NoStyle [CodeNotation "["]
    brackets      = scrapWithPlainText " brackets"
    column2       = Scrap NoStyle [CodeNotation "]"]
    onThePage     = scrapWithPlainText " on the page"

-- "> Our goal is to help you build a map of your ideas that gains clarity and context with every scrap you add. ",
ourGoalIs :: Block
ourGoalIs = BlockQuote $ ScrapText [ourGoalIsText]
  where
    ourGoalIsText = scrapWithPlainText "Our goal is to help you build a map of your ideas that gains\
    \ clarity and context with every scrap you add. "

getStartedBlock :: [Block]
getStartedBlock = 
    [ getStarted
    , startedThumbnail
    , welcome
    , breakLine
    , everythingIs
    , breakLine
    , clickOn
    , breakLine
    , pressTab
    , breakLine
    , highlightText
    , addLinks
    , breakLine
    ]

getsInterestingBlock :: [Block]
getsInterestingBlock = 
    [ hereIs
    , breakLine
    , clickNewLink
    , breakLine
    , clickRelated
    , pagesThat
    , breakLine
    , seeImages
    , breakLine
    , ourGoalIs
    , breakLine
    , breakLine
    ]

onceStartedBlock :: [Block]
onceStartedBlock = []

getStartedMarkdown :: Markdown
getStartedMarkdown = mkMarkdown $ getStartedBlock <> getsInterestingBlock <> onceStartedBlock

-- "Get started",
-- "[https://gyazo.com/5f93e65a3b979ae5333aca4f32600611]",
-- "Welcome to your new Scrapbox project!",
-- "",
-- "[** 📝Everything is editable]",
-- "",
-- "\tClick on any line and start typing to edit. ",
-- "",
-- "\t\tPress tab at the beginning of a line to indent and add a bullet point.",
-- "",
-- " Highlight text to make it a [new link], [* bold], [- and] [/ more].",
-- "\t\tAdd links while typing with a `#` before or brackets around `[`words you want to link `]` ",
-- "",


-- "[** 🎯 Here is where it gets interesting ]",
-- "",
-- "\tClick a [new link] to create a new page with that title and open it.",
-- "",
-- "\tClick related thumbnails in the footer of any page to explore ideas you have linked.",
-- " \tPages that are directly linked or two steps away from the current page will be displayed.",
-- "",
-- "\tSee images, videos, and external links added inside `[` brackets`]` on the page",
-- "",
-- "> Our goal is to help you build a map of your ideas that gains clarity and context with every scrap you add. ",
-- "",
-- "",
-- "[* What can you put in a Scrapbox project?]",
-- "\tUse Scrapbox to outline ideas, discuss `code blocks`, give feedback, and brainstorm. ",
-- "",
-- "[* For example]",
-- "\tLets say you are working on developing a new website. You might want to discuss ideas with your team before and while you execute the plan.  First create a page `Site plan` to start a conversation about the site requirements and link some useful resources. On that page you might add a link for a new page called `Social media buttons`.",
-- "",
-- "\tYou can immediately click on that link to `Social media buttons` and start editing.  There you may add links to `Twitter`, `Facebook`, etc.  Next you can click on `Twitter` and you'll see a related link that will take you back to `Site Plan`. ",
-- "",
-- "Once you can easily and directly type your ideas while also building context ideas become more clear the more you use it. No more folders full of dead text means no more teams isolated from their own ideas.",
-- "",
-- ">  [/ What ideas in your head could your team benefit from you putting down right now? Go create your first three or so pages and add a few links. From 3 to 3,000 pages your ideas will only grow in context.]",
-- "",
-- "",
-- "[** 📌 Once you've got the basics, here are ways to dig deeper and get the most out of your new project ]",
-- " See a list of all the [https://scrapbox.io/help/Things%20you%20can%20do Things you can do] ",
-- " \tIncludes more syntax, inviting team members, and creating profiles",
-- "",
-- "\tSee some [https://scrapbox.io/help/examples Example projects] ",
-- " \tIncludes a SaaS startup, design agency, and more",
-- "",
-- "\tSee [https://scrapbox.io/help/ How-tos and support] ",
-- " \tFor detailed instructions and answers to FAQs",
-- "",
-- "",
-- "[* We would love to hear any questions or feedback you may have]",
-- "Please let us know if you have any suggestions, questions, or points of friction.You can contact us directly by email: contact@scrapbox.io, [twitter https://twitter.com/scrapboxapp], and [https://facebook.com/scrapboxapp facebook]",
-- "",
-- "[/ Thank you for using Scrapbox!]",
-- "[https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58]",
-- "",
-- "",
-- ">Note: When you're done reading you might change the title of this page to 'Welcome to project-name' and add some personalized instructions for your team.",