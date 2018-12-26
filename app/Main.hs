module Main where

import           RIO

import           CMark

--------------------------------------------------------------------------------
-- Test files
--------------------------------------------------------------------------------

-- | Test data for example.md
test :: IO Node
test = testWith "./docs/example.md"

-- | Test data for Header
testHeader :: IO Node
testHeader = testWith "./docs/headers.md"

-- | Test data for nested List
testNestedList :: IO Node
testNestedList = testWith "./docs/nestedList.md"

-- | Test table
testTable :: IO Node
testTable = testWith "./docs/table.md"

testWith :: FilePath -> IO Node
testWith filePath = do
    markDown <- readFileUtf8 filePath
    let options = [optSafe, optHardBreaks]
    let parsed = commonmarkToNode options markDown
    return parsed

main :: IO ()
main = undefined