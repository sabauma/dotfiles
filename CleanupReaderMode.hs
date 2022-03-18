{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import System.IO
import Text.HTML.TagSoup.Tree

removeReaderMode :: [TagTree T.Text] -> [TagTree T.Text]
removeReaderMode tree = transformTree f tree
  where
    f (TagBranch _ attrs _)
      | any ((==) ("class", "toolbar reader-toolbar")) attrs = []
    f x = [x]

main :: IO ()
main = T.interact (renderTree . removeReaderMode . parseTree)

