module Lib
      ( 
         prompt
      ) where

import qualified Api as A
import qualified Format.GitBranch as GF
import qualified Colourista.Pure  as Colour

-- Need to add Colour support
prompt :: IO String
prompt = do
  localTime    <- processTime                        <$> A.getLocalTime
  user         <- processUser                        <$> A.getUser
  path         <- (colourPath . processPath)         <$> A.getCurrentDirectory
  isGitRepo    <- A.isGitRepo
  case isGitRepo of
    Just True -> do
      (branch, modified) <- processGitRepo
      pure (
              localTime <> 
              ":"       <> 
              user      <> 
              "@mbp"    <> 
              ":"       <> 
              path      <>
              ":"       <>
              branch    <>
              modified  <>
              promptStart
            )
    _         -> 
      pure (
              localTime <> 
              ":"       <> 
              user      <> 
              "@mbp"    <> 
              ":"       <> 
              path      <>
              promptStart
        )

processGitRepo :: IO (String, String)
processGitRepo = do
  branch       <- GF.processGitRepo <$> A.gitBranchVerbose
  status       <- GF.isModified     <$> A.gitStatusShort
  pure (branch, GF.processModified $ status)

-- push to a config object
maxPathLength :: Int
maxPathLength = 40

processPath :: Maybe A.CurrentDirectory  -> String
processPath (Just (A.CurrentDirectory pwd)) = 
  let pathLength = length pwd
  in if pathLength > maxPathLength then
    "..." <> takeR (maxPathLength - 3) pwd
  else
    pwd
processPath Nothing = "-"

colourPath :: String -> String
colourPath = Colour.formatWith [Colour.cyan] 

promptStart :: String
promptStart = "> "

takeR :: Int -> [a] -> [a]
takeR n xs 
  | n <= 0    = xs
  | otherwise = reverse . take n . reverse $ xs

processTime :: Maybe A.LocalTime  -> String
processTime (Just (A.LocalTime localTime)) = "[" <> (take 19 localTime) <> "]"
processTime Nothing = "-"

processUser :: Maybe A.User  -> String
processUser (Just (A.User user)) = user
processUser Nothing = "-"
