module Lp  where

import           Control.Monad      (when)
import           Data.List          (isPrefixOf)
import           System.Directory   (removeFile)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

run :: IO ()
run = do
  fpaths <- getArgs
  let removeAfter = "--rm" `elem` fpaths
      fs = filter (not . isPrefixOf "-") fpaths
  mapM_ (runOne removeAfter) fs

runOne :: Bool -> FilePath -> IO ()
runOne removeAfter fpath = do
  firstLine <- words .  head . lines <$> readFile fpath
  if head firstLine == "#!lp"
    then do
      let path = last firstLine
      readFile fpath >>= writeFile (path <> "/" <> last (wordsBy '/' fpath)) . unlines . tail . lines
      when removeAfter (removeFile fpath)
    else do
      putStrLn $ "File '" <> fpath <> "' does not contain lp pragma"
      exitFailure

wordsBy :: Char -> [Char] -> [[Char]]
wordsBy c s =  case dropWhile {-partain:Char.-}(==c) s of
  "" -> []
  s' -> w : wordsBy c s''
    where (w, s'') = break {-partain:Char.-} (==c) s'

