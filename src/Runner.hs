module Runner
  ( run
  ) where

import           Control.Monad    (forM)
import           Data.List
import           Sequence
import           System.Directory
import           System.IO

run :: FilePath -> IO ()
run directory = do
  print $ "Scanning directory " ++ directory
  files <- listDirectory directory
  let filesToConvert = map (\x -> directory ++ "/" ++ x) $ filter (isSuffixOf ".fasta") files
  print $ "Found " ++ show (length filesToConvert) ++ " sequence files in " ++ directory
  res <- forM filesToConvert updateFile
  print $ "Number of converted sequences : " ++ show (sum res)
  return ()

updateFile :: FilePath -> IO Int
updateFile filePath = do
  print $ "Updating file " ++ filePath
  withFile filePath ReadWriteMode (updateFileContent filePath)

updateFileContent :: FilePath -> Handle -> IO Int
updateFileContent filePath h = do
  hSetNewlineMode h universalNewlineMode
  contents <- hGetContents h
  let sequences = contentsToSequences contents
  let correctedSequences = map renameSequence sequences
  let newContent = concatMap sequenceToContent correctedSequences
  let newFileName = filePath ++ ".converted"
  writeFile newFileName newContent
  print $ filePath ++ " converted in " ++ newFileName
  return (length sequences)
