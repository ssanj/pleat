module Commands.Model 
(
 -- * Data types
     Command(..)
   , CommandDescription(..)
   , Args(..)
)
where

newtype Command = Command { _cmd :: String }
newtype Args = Args { _args :: [String] }

data CommandDescription = CommandDescription { _commandDescCommand :: Command, _commandDescArgs :: Args }
