
import System.IO
import Text.HTML.TagSoup.Tree

removeReaderMode :: [TagTree String] -> [TagTree String]
removeReaderMode tree = transformTree f tree
  where
    f (TagBranch _ attrs _)
      | any ((==) ("class", "toolbar reader-toolbar")) attrs = []
    f x = [x]

main :: IO ()
main = interact (renderTree . removeReaderMode . parseTree)

