{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import System.IO
import Text.HTML.TagSoup.Tree

removeReaderMode :: [TagTree B.ByteString] -> [TagTree B.ByteString]
removeReaderMode tree = transformTree f tree
  where
    f (TagBranch _ attrs _)
      | any ((==) ("class", "toolbar reader-toolbar")) attrs = []
    f x = [x]

main :: IO ()
main = B.interact (renderTree . removeReaderMode . parseTree)

