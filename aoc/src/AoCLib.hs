module AoCLib
  ( turnFileStringIntoInputArray
  ) where

turnFileStringIntoInputArray :: String -> IO (String)
turnFileStringIntoInputArray inp = do
  content <- readFile inp
  return content

