{-# LANGUAGE OverloadedStrings #-}

module Example where

import RIO

import Types
import Lib

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

textBlock :: Text -> Block
textBlock text = Simple $ ScrapText [Scrap NoStyle [PlainText text]]

bulletLine :: Int -> Text -> Block
bulletLine num text = BulletLine num $ ScrapText $ [Scrap NoStyle [PlainText text]]

plainText :: Text -> Scrap
plainText text = Scrap NoStyle [PlainText text]

codeNotation :: Text -> Scrap
codeNotation text = Scrap NoStyle [CodeNotation text]

breakLine :: Block
breakLine = BreakLine

--------------------------------------------------------------------------------
-- Blocks
--------------------------------------------------------------------------------

-- "Get started",
getStarted :: Block
getStarted = textBlock "Get started"

-- "[https://gyazo.com/5f93e65a3b979ae5333aca4f32600611]",
startedThumbnail :: Block
startedThumbnail = Thumbnail (Url "https://gyazo.com/5f93e65a3b979ae5333aca4f32600611")

-- "Welcome to your new Scrapbox project!",
welcome :: Block
welcome = textBlock "Welcome to your new Scrapbox project!"

-- "[** 📝Everything is editable]"
everythingIs :: Block
everythingIs = Header 2 [PlainText "Everything is editable"]

-- "\tClick on any line and start typing to edit. "
clickOn :: Block
clickOn = bulletLine 1 "Click on any line and start typing to edit. "

-- "\t\tPress tab at the beginning of a line to indent and add a bullet point."
pressTab :: Block
pressTab = bulletLine 2 "Press tab at the beginning of a line to indent and add a bullet point."

-- " Highlight text to make it a [new link], [* bold], [- and] [/ more]."
highlightText :: Block
highlightText = BulletLine 1 $ ScrapText [highlight, newLink, column, bold, column, crossed, space, italic, period]
  where
    highlight :: Scrap
    highlight = plainText " Highlight text to make it a "
    newLink :: Scrap
    newLink = Scrap NoStyle [Link Nothing (Url "New link")]
    column :: Scrap
    column = plainText ", "
    space :: Scrap
    space = plainText " "
    period :: Scrap
    period = plainText "."
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
    addingLinks  = plainText "Add links while typing with a "
    symbol       = codeNotation "#"
    beforeOr     = plainText " before or brackets around "
    column1      = codeNotation "["
    wordsYouWant = plainText "words you want to link "
    column2      = codeNotation "]"

-- "[** 🎯 Here is where it gets interesting ]",
hereIs :: Block
hereIs = Header 2 [PlainText "Here is where it gets interesting "]

-- "\tClick a [new link] to create a new page with that title and open it.",
clickNewLink :: Block
clickNewLink = BulletLine 1 $ ScrapText [clickA, newLink, toCreate]
  where
    clickA   = plainText "Click a "
    newLink  = Scrap NoStyle [Link Nothing (Url "New Link")]
    toCreate = plainText " to create a new page with that title and open it."

-- Click related thumbnails in the footer of any page to explore ideas you have linked.
clickRelated :: Block
clickRelated = textBlock "Click related thumbnails in the footer of any page to explore ideas you have linked."

-- " \tPages that are directly linked or two steps away from the current page will be displayed.",
pagesThat :: Block
pagesThat = bulletLine 2 "Pages that are directly linked or two steps away from the current page will be displayed."

-- "\tSee images, videos, and external links added inside `[` brackets`]` on the page",
seeImages :: Block
seeImages = BulletLine 1 $ ScrapText [seeImagesText, column1, brackets, column2, onThePage]
  where
    seeImagesText = plainText "See images, videos, and external links added inside "
    column1       = codeNotation "["
    brackets      = plainText " brackets"
    column2       = codeNotation  "]"
    onThePage     = plainText " on the page"

-- "> Our goal is to help you build a map of your ideas that gains clarity and context with every scrap you add. ",
ourGoalIs :: Block
ourGoalIs = BlockQuote $ ScrapText [ourGoalIsText]
  where
    ourGoalIsText = plainText "Our goal is to help you build a map of your ideas that gains\
    \ clarity and context with every scrap you add. "

-- "[* What can you put in a Scrapbox project?]",
whatCan :: Block
whatCan = Header 1 $ [PlainText "What can you put in a Scrapbox project?"]

-- "\tUse Scrapbox to outline ideas, discuss `code blocks`, give feedback, and brainstorm. ",
useScrapbox :: Block
useScrapbox = BulletLine 1 $ ScrapText [useScrapText, codeBlocksText, giveFeedback]
  where
    useScrapText   = plainText "Use Scrapbox to outline ideas, discuss "
    codeBlocksText = codeNotation "code blocks"
    giveFeedback   = plainText ", give feedback, and brainstorm. "

-- "[* For example]",
forExample :: Block
forExample = bulletLine 1 "For example"

-- "\tLets say you are working on developing a new website. You might want to discuss ideas with your team before and while you execute the plan.  First create a page `Site plan` to start a conversation about the site requirements and link some useful resources. On that page you might add a link for a new page called `Social media buttons`.",
letsSay :: Block
letsSay = BulletLine 1 $ ScrapText [letsSayText, sitePlan, toStart, socialMedia, period]
  where
    letsSayText = plainText "Lets say you are working on developing a new website. \
    \You might want to discuss ideas with your team before and while you execute the plan.  First create a page "
    sitePlan    = codeNotation "Site plan"
    toStart     = plainText " to start a conversation about the site requirements and link \
    \some useful resources. On that page you might add a link for a new page called "
    socialMedia = codeNotation "Social media buttons"
    period      = plainText "."

-- "\tYou can immediately click on that link to `Social media buttons` and start editing.  There you may add links to `Twitter`, `Facebook`, etc.  Next you can click on `Twitter` and you'll see a related link that will take you back to `Site Plan`. ",
youCanImmediately :: Block
youCanImmediately = BulletLine 1 $ ScrapText 
  [youcan, socialMedia, andStart, twitter, column, faceBook, nextYoucan, twitter, relatedLink, sitePlan, period]
  where
    youcan = plainText "You can immediately click on that link to `Social media buttons` and start editing.  There you may add links to "
    socialMedia = codeNotation "Social media buttons"
    twitter     = codeNotation "Twitter"
    faceBook    = codeNotation "Facebook"
    andStart    = plainText " and start editing.  There you may add links to "
    column      = plainText ", "
    nextYoucan  = plainText ", etc.  Next you can click on "
    relatedLink = plainText " and you'll see a related link that will take you back to "
    sitePlan    = codeNotation "Site Plan"
    period      = plainText ". "

-- Once you can easily and directly type your ideas while also building context ideas become more
-- clear the more you use it. No more folders full of dead text means no more teams isolated from their own ideas.
onceYoucan :: Block
onceYoucan = textBlock "Once you can easily and directly type your ideas while also building \
    \ context ideas become more clear the more you use it. No more folders full of dead text means no \
    \more teams isolated from their own ideas."

-- >  [/ What ideas in your head could your team benefit from you putting down right now? Go create
--  your first three or so pages and add a few links. From 3 to 3,000 pages your ideas will only grow in context.]
whatIdeas :: Block
whatIdeas = BlockQuote $ ScrapText [Scrap Italic [PlainText "What ideas in your head could your team \
    \ benefit from you putting down right now? Go create your first three or so pages and add a few \
    \links. From 3 to 3,000 pages your ideas will only grow in context."]]

-- "[** 📌 Once you've got the basics, here are ways to dig deeper and get the most out of your new project ]",
onceYouGot :: Block
onceYouGot = Header 2 $ [PlainText "📌 Once you've got the basics, here are ways to dig deeper and get the most out of your new project "]


-- " See a list of all the [https://scrapbox.io/help/Things%20you%20can%20do Things you can do] ",
seeAList :: Block
seeAList = Simple $ ScrapText [seeAListText, thingsYouCando, space]
  where
    seeAListText = plainText " See a list of all the "
    thingsYouCando = Scrap NoStyle [Link (Just "Things you can do") (Url "https://scrapbox.io/help/Things%20you%20can%20do")]
    space = plainText " "

-- " \tIncludes more syntax, inviting team members, and creating profiles",
includesMore :: Block
includesMore = bulletLine 1 "Includes more syntax, inviting team members, and creating profiles"


-- "\tSee some [https://scrapbox.io/help/examples Example projects] ",
seeSome :: Block
seeSome = BulletLine 1 $ ScrapText [seeSomeText, exampleProjects, space]
  where
    seeSomeText = plainText "See some "
    exampleProjects = Scrap NoStyle [Link (Just "Example projects") (Url "https://scrapbox.io/help/exampless")]
    space           = plainText " "

-- " \tIncludes a SaaS startup, design agency, and more",
includesSaas :: Block
includesSaas = bulletLine 2 "Includes a SaaS startup, design agency, and more"


-- "\tSee [https://scrapbox.io/help/ How-tos and support] ",
howTos :: Block
howTos = BulletLine 1 $ ScrapText [see, howTosText, space]
  where
    see        = plainText "See "
    howTosText = Scrap NoStyle [Link (Just "How-tos and support") (Url "https://scrapbox.io/help/")]
    space      = plainText " "

-- " \tFor detailed instructions and answers to FAQs",
forDetails :: Block
forDetails = bulletLine 2 "For detailed instructions and answers to FAQs"

-- "[* We would love to hear any questions or feedback you may have]",
weWouldLove :: Block
weWouldLove = Header 1 $ [PlainText "We would love to hear any questions or feedback you may have"]

-- "Please let us know if you have any suggestions, questions, or points of friction.You can contact us directly by email: contact@scrapbox.io, [twitter https://twitter.com/scrapboxapp], and [https://facebook.com/scrapboxapp facebook]",
pleaseLet :: Block
pleaseLet = Simple $ ScrapText [pleaseLetText, twitter, andText, faceBook]
  where
    pleaseLetText = plainText "Please let us know if you have any suggestions, questions, or \
      \points of friction.You can contact us directly by email: contact@scrapbox.io, "
    andText       = plainText ", and "
    twitter       = Scrap NoStyle [Link (Just "twitter") (Url "https://twitter.com/scrapboxapp")]
    faceBook      = Scrap NoStyle [Link (Just "facebook") (Url "https://facebook.com/scrapboxapp")]

-- "[/ Thank you for using Scrapbox!]",
thankYouFor :: Block
thankYouFor = Simple $ ScrapText [thankyou]
  where
    thankyou = Scrap Italic [PlainText "Thank you for using Scrapbox!"]

-- "[https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58]",
thankYouThumbnail :: Block
thankYouThumbnail = Thumbnail (Url "https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58")

-- ">Note: When you're done reading you might change the title of this page to 'Welcome to project-name' and add some personalized instructions for your team.",
noteWhen :: Block
noteWhen = BlockQuote $ ScrapText [noteWhenText]
  where
    noteWhenText =  plainText "Note: When you're done reading you might change the title of \
    \ this page to 'Welcome to project-name' and add some personalized instructions for your team."
----------------------------------------------------------------------------------------------------
-- Accumulated blocks
----------------------------------------------------------------------------------------------------

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
onceStartedBlock = 
    [ whatCan
    , useScrapbox
    , breakLine
    , forExample
    , letsSay
    , breakLine
    , youCanImmediately
    , breakLine
    , onceYoucan
    , breakLine
    , whatIdeas
    , breakLine
    , breakLine
    , onceYouGot
    , seeAList
    , includesMore
    , breakLine
    , breakLine
    , seeSome
    , includesSaas
    , breakLine
    , howTos
    , forDetails
    , breakLine
    , breakLine
    , weWouldLove
    , pleaseLet
    , breakLine
    , thankYouFor
    , breakLine
    , breakLine
    , noteWhen
    ]

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