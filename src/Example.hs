{-# LANGUAGE OverloadedStrings #-}

module Example
  ( getStartedMarkdown
  , getsInterestingMd
  , onceStartedMd
  , getStartedMd
  ) where

import           RIO   hiding (link)

import           Lib
import           Types

--------------------------------------------------------------------------------
-- SECTION: Get started
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
everythingIs = Header (HeaderSize 2) [simpleText "📝 Everything is editable"]

-- "\tClick on any line and start typing to edit. "
clickOn :: Block
clickOn = bulletPoint 1 "Click on any line and start typing to edit. "

-- "\t\tPress tab at the beginning of a line to indent and add a bullet point."
pressTab :: Block
pressTab = bulletPoint 2 "Press tab at the beginning of a line to indent and add a bullet point."

-- " Highlight text to make it a [new link], [* bold], [- and] [/ more]."
highlightText :: Block
highlightText = BulletPoint (BulletSize 1) $ ScrapText
    [ noStyle [highlight, newLink, column]
    , bold [boldTxt]
    , noStyle [column]
    , strikeThrough [crossed]
    , noStyle [space]
    , italic [more]
    , noStyle [period]
    ]
  where
    highlight = simpleText " Highlight text to make it a "
    newLink   = link Nothing (Url "New link")
    column    = simpleText ", "
    space     = simpleText " "
    period    = simpleText "."
    boldTxt   = simpleText "bold"
    crossed   = simpleText "and"
    more      = simpleText "more"

-- "\t\tAdd links while typing with a `#` before or brackets around `[`words you want to link `]` "
addLinks :: Block
addLinks = BulletPoint (BulletSize 2) $ 
    ScrapText [ noStyle [addingLinks, symbol, beforeOr, column1, wordsYouWant, column2]]
  where
    addingLinks  = simpleText "Add links while typing with a "
    symbol       = codeNotation "#"
    beforeOr     = simpleText " before or brackets around "
    column1      = codeNotation "["
    wordsYouWant = simpleText "words you want to link "
    column2      = codeNotation "]"

----------------------------------------------------------------------------------------------------
-- SECTION: Here is where it gets interesting
----------------------------------------------------------------------------------------------------

-- "[** 🎯 Here is where it gets interesting ]",
hereIs :: Block
hereIs = Header (HeaderSize 2) [simpleText "🎯 Here is where it gets interesting "]

-- "\tClick a [new link] to create a new page with that title and open it.",
clickNewLink :: Block
clickNewLink = BulletPoint (BulletSize 1) $ ScrapText [noStyle [clickA, newLink, toCreate]]
  where
    clickA   = simpleText "Click a "
    newLink  = link Nothing (Url "New Link")
    toCreate = simpleText " to create a new page with that title and open it."

-- Click related thumbnails in the footer of any page to explore ideas you have linked.
clickRelated :: Block
clickRelated = textBlock "Click related thumbnails in the footer of any page to explore ideas \
    \you have linked."

-- " \tPages that are directly linked or two steps away from the current page will be displayed.",
pagesThat :: Block
pagesThat = bulletPoint 2 "Pages that are directly linked or two steps away from the current page \
    \will be displayed."

-- "\tSee images, videos, and external links added inside `[` brackets`]` on the page",
seeImages :: Block
seeImages = BulletPoint (BulletSize 1) $ ScrapText 
    [noStyle [seeImageSimpleText, column1, brackets, column2, onThePage]]
  where
    seeImageSimpleText = simpleText "See images, videos, and external links added inside "
    column1            = codeNotation "["
    brackets           = simpleText " brackets"
    column2            = codeNotation  "]"
    onThePage          = simpleText " on the page"

-- "> Our goal is to help you build a map of your ideas that gains clarity and context with every scrap you add. ",
ourGoalIs :: Block
ourGoalIs = BlockQuote $ ScrapText [noStyle [ourGoalISimpleText]]
  where
    ourGoalISimpleText = simpleText " Our goal is to help you build a map of your ideas that gains\
    \ clarity and context with every scrap you add. "

----------------------------------------------------------------------------------------------------
-- SECTION: What can you put in a Scrapbox project?
----------------------------------------------------------------------------------------------------

-- "[* What can you put in a Scrapbox project?]",
whatCan :: Block
whatCan = Header (HeaderSize 1) $ [simpleText "What can you put in a Scrapbox project?"]

-- "\tUse Scrapbox to outline ideas, discuss `code blocks`, give feedback, and brainstorm. ",
useScrapbox :: Block
useScrapbox = BulletPoint (BulletSize 1) $ 
    ScrapText [noStyle [useScrapText, codeBlockSimpleText, giveFeedback]]
  where
    useScrapText        = simpleText "Use Scrapbox to outline ideas, discuss "
    codeBlockSimpleText = codeNotation "code blocks"
    giveFeedback        = simpleText ", give feedback, and brainstorm. "

-- "[* For example]",
forExample :: Block
forExample = bulletPoint 1 "For example"

-- "\tLets say you are working on developing a new website. You might want to discuss ideas with
-- your team before and while you execute the plan.  First create a page `Site plan` to start a
-- conversation about the site requirements and link some useful resources. On that page you might
-- add a link for a new page called `Social media buttons`.",
letsSay :: Block
letsSay = BulletPoint (BulletSize 1) $ ScrapText [ noStyle [letsSayText, sitePlan, toStart, socialMedia, period]]
  where
    letsSayText = simpleText "Lets say you are working on developing a new website. \
    \You might want to discuss ideas with your team before and while you execute the plan.  First create a page "
    sitePlan    = codeNotation "Site plan"
    toStart     = simpleText " to start a conversation about the site requirements and link \
    \some useful resources. On that page you might add a link for a new page called "
    socialMedia = codeNotation "Social media buttons"
    period      = simpleText "."

-- "\tYou can immediately click on that link to `Social media buttons` and start editing.
-- There you may add links to `Twitter`, `Facebook`, etc.  Next you can click on `Twitter` and you'll
-- see a related link that will take you back to `Site Plan`. ",
youCanImmediately :: Block
youCanImmediately = BulletPoint (BulletSize 1) $ 
    ScrapText [noStyle
        [ youcan, socialMedia, andStart, twitter, column, faceBook, nextYoucan
        , twitter, relatedLink, sitePlan, period]]
  where
    youcan      = simpleText "You can immediately click on that link to `Social media buttons` and \
    \ start editing. There you may add links to "
    socialMedia = codeNotation "Social media buttons"
    twitter     = codeNotation "Twitter"
    faceBook    = codeNotation "Facebook"
    andStart    = simpleText " and start editing.  There you may add links to "
    column      = simpleText ", "
    nextYoucan  = simpleText ", etc.  Next you can click on "
    relatedLink = simpleText " and you'll see a related link that will take you back to "
    sitePlan    = codeNotation "Site Plan"
    period      = simpleText ". "

-- Once you can easily and directly type your ideas while also building context ideas become more
-- clear the more you use it. No more folders full of dead text means no more teams isolated from their own ideas.
onceYoucan :: Block
onceYoucan = textBlock "Once you can easily and directly type your ideas while also building \
    \ context ideas become more clear the more you use it. No more folders full of dead text means no \
    \more teams isolated from their own ideas."

-- >  [/ What ideas in your head could your team benefit from you putting down right now? Go create
--  your first three or so pages and add a few links. From 3 to 3,000 pages your ideas will only grow in context.]
whatIdeas :: Block
whatIdeas = BlockQuote $ ScrapText [italic [simpleText "What ideas in your head could your team \
    \ benefit from you putting down right now? Go create your first three or so pages and add a few \
    \links. From 3 to 3,000 pages your ideas will only grow in context."]]

-- "[** 📌 Once you've got the basics, here are ways to dig deeper and get the most out of your new project ]",
onceYouGot :: Block
onceYouGot = Header (HeaderSize 2) $ [simpleText "📌 Once you've got the basics, here are ways to dig deeper and \
  \get the most out of your new project "]


-- " See a list of all the [https://scrapbox.io/help/Things%20you%20can%20do Things you can do] ",
seeAList :: Block
seeAList = Document $ ScrapText [noStyle [seeAListText, thingsYouCando, space]]
  where
    seeAListText   = simpleText " See a list of all the "
    thingsYouCando = link (Just "Things you can do") (Url "https://scrapbox.io/help/Things%20you%20can%20do")
    space          = simpleText " "

-- " \tIncludes more syntax, inviting team members, and creating profiles",
includesMore :: Block
includesMore = bulletPoint 1 "Includes more syntax, inviting team members, and creating profiles"

-- "\tSee some [https://scrapbox.io/help/examples Example projects] ",
seeSome :: Block
seeSome = BulletPoint (BulletSize 1) $ ScrapText [noStyle [seeSomeText, exampleProjects, space]]
  where
    seeSomeText     = simpleText "See some "
    exampleProjects = link (Just "Example projects") (Url "https://scrapbox.io/help/exampless")
    space           = simpleText " "

-- " \tIncludes a SaaS startup, design agency, and more",
includesSaas :: Block
includesSaas = bulletPoint 2 "Includes a SaaS startup, design agency, and more"


-- "\tSee [https://scrapbox.io/help/ How-tos and support] ",
howTos :: Block
howTos = BulletPoint (BulletSize 1) $ ScrapText [noStyle [see, howToSimpleText, space]]
  where
    see             = simpleText "See "
    howToSimpleText = link (Just "How-tos and support") (Url "https://scrapbox.io/help/")
    space           = simpleText " "

-- " \tFor detailed instructions and answers to FAQs",
forDetails :: Block
forDetails = bulletPoint 2 "For detailed instructions and answers to FAQs"

-- "[* We would love to hear any questions or feedback you may have]",
weWouldLove :: Block
weWouldLove = Header (HeaderSize 1) $ [simpleText "We would love to hear any questions or feedback you may have"]

-- "Please let us know if you have any suggestions, questions, or points of friction.You can contact us directly by email: contact@scrapbox.io, [twitter https://twitter.com/scrapboxapp], and [https://facebook.com/scrapboxapp facebook]",
pleaseLet :: Block
pleaseLet = Document $ ScrapText [noStyle [pleaseLetText, twitter, andText, faceBook]]
  where
    pleaseLetText = simpleText "Please let us know if you have any suggestions, questions, or \
      \points of friction.You can contact us directly by email: contact@scrapbox.io, "
    andText       = simpleText ", and "
    twitter       = link (Just "twitter") (Url "https://twitter.com/scrapboxapp")
    faceBook      = link (Just "facebook") (Url "https://facebook.com/scrapboxapp")

-- "[/ Thank you for using Scrapbox!]",
thankYouFor :: Block
thankYouFor = Document $ ScrapText [italic [thankYou]]
  where
    thankYou = simpleText "Thank you for using Scrapbox!"

-- "[https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58]",
thankYouThumbnail :: Block
thankYouThumbnail = Thumbnail (Url "https://gyazo.com/5aeffb3e8a6561ae78430664d8257f58")

-- ">Note: When you're done reading you might change the title of this page to 'Welcome to
-- project-name' and add some personalized instructions for your team.",
noteWhen :: Block
noteWhen = BlockQuote $ ScrapText [noStyle [noteWhenText]]
  where
    noteWhenText =  simpleText "Note: When you're done reading you might change the title of \
    \ this page to 'Welcome to project-name' and add some personalized instructions for your team."

----------------------------------------------------------------------------------------------------
-- Accumulated blocks
----------------------------------------------------------------------------------------------------

getStartedBlock :: [Block]
getStartedBlock =
    [ getStarted
    , startedThumbnail
    , welcome
    , lineBreak
    , everythingIs
    , lineBreak
    , clickOn
    , lineBreak
    , pressTab
    , lineBreak
    , highlightText
    , addLinks
    , lineBreak
    ]

getsInterestingBlock :: [Block]
getsInterestingBlock =
    [ hereIs
    , lineBreak
    , clickNewLink
    , lineBreak
    , clickRelated
    , pagesThat
    , lineBreak
    , seeImages
    , lineBreak
    , ourGoalIs
    , lineBreak
    , lineBreak
    ]

onceStartedBlock :: [Block]
onceStartedBlock =
    [ whatCan
    , useScrapbox
    , lineBreak
    , forExample
    , letsSay
    , lineBreak
    , youCanImmediately
    , lineBreak
    , onceYoucan
    , lineBreak
    , whatIdeas
    , lineBreak
    , lineBreak
    , onceYouGot
    , seeAList
    , includesMore
    , lineBreak
    , lineBreak
    , seeSome
    , includesSaas
    , lineBreak
    , howTos
    , forDetails
    , lineBreak
    , lineBreak
    , weWouldLove
    , pleaseLet
    , lineBreak
    , thankYouFor
    , thankYouThumbnail
    , lineBreak
    , lineBreak
    , noteWhen
    ]

getStartedMd :: Markdown
getStartedMd = mkMarkdown getStartedBlock

getsInterestingMd :: Markdown
getsInterestingMd = mkMarkdown getsInterestingBlock

onceStartedMd :: Markdown
onceStartedMd = mkMarkdown onceStartedBlock

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
