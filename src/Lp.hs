module Lp  where

import           System.Environment

run :: IO ()
run = do
  fpaths <- getArgs
  mapM_ runOne fpaths

runOne :: FilePath -> IO ()
runOne fpath = do
  firstLine <- words .  head . lines <$> readFile fpath
  if head firstLine == "#!lp"
    then do
      let path = last firstLine
      readFile fpath >>= writeFile (path <> "/" <> last (wordsBy '/' fpath))
    else putStrLn $ "File '" <> fpath <> "' does not contain lp pragma"


wordsBy :: Char -> [Char] -> [[Char]]
wordsBy c s =  case dropWhile {-partain:Char.-}(==c) s of
  "" -> []
  s' -> w : wordsBy c s''
    where (w, s'') = break {-partain:Char.-} (==c) s'
