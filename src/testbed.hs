-- Flexibility conditions --
{-# LANGUAGE DeriveGeneric #-}      
{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Process
import System.IO
import System.Exit
import Control.Monad (forM, when)
import System.Info (os)
import System.Environment (getArgs)
import Control.Concurrent.Async (mapConcurrently)   -- THIS LIBRARY!
import Data.Yaml (decodeFileThrow)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import GHC.Generics (Generic)                       -- AND THIS LIBRARY! Allows for generics to be derived
import Data.Aeson (FromJSON,ToJSON)
import System.FilePath (takeExtension)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.List (dropWhileEnd, isPrefixOf,intercalate)
import Data.Char (isSpace)                           -- for better trim

-- Generic type derivation allows
-- Test data type  
data TestCase = TestCase
  { name            :: String  
  , command         :: String                      
  , input           :: String
  , expected_output :: String
  , result          :: Bool
  , signature       :: Maybe String
  , normalized      :: Maybe Bool
  , isTrim          :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON TestCase  -- technically parses yaml as well
instance ToJSON TestCase  

-- Normalize line endings -> platform agnostic results
normalize :: String -> String
normalize = filter (/= '\r')

-- Normalizes path for Windows
normalizeCommand :: String -> String
normalizeCommand cmd
  | os == "mingw32" = 
      let cmd' = if "./" `isPrefixOf` cmd then drop 2 cmd else cmd
      in map slashToBackslash cmd'
  | otherwise = cmd
  where
    slashToBackslash '/' = '\\'
    slashToBackslash c   = c
    
---------------------------------------------------------------------------------
--- Test Procedure --------------------------------------------------------------
---------------------------------------------------------------------------------

runTest :: TestCase -> IO Bool                                        -- run test procedure (returns True/False)
runTest (TestCase name cmd input expected _ _ _ _) = do
  let cmdStr = normalizeCommand (trim cmd)                            -- normalize path if Windows
  (Just hin, Just hout, _, ph) <- createProcess (shell cmdStr)
    { std_in = CreatePipe, std_out = CreatePipe }

  hPutStr hin input                -- fxn() <- input
  hClose hin                       -- close input handle
  output <- hGetContents hout      -- optimized lazy read          
  length output `seq` return ()    -- wait for whole output
  exitCode <- waitForProcess ph  

  let passed = exitCode == ExitSuccess && normalize output == normalize expected  -- pass/fail handling
  if passed
    then do
      putStrLn $ name ++ ": PASS"
      return True
    else do
      putStrLn $ name ++ ": FAIL"
      putStrLn $ "  Command:  " ++ cmdStr
      putStrLn $ "  Expected: " ++ show expected
      putStrLn $ "  Got:      " ++ show output
      return False


-- helper trim function (removes leading/trailing whitespace)
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

---------------------------------------------------------------------------------
--- Test Load Procedure ---------------------------------------------------------
---------------------------------------------------------------------------------

loadTests :: FilePath -> IO [TestCase]
loadTests file = case takeExtension file of
  ".yaml" -> decodeFileThrow file
  ".yml"  -> decodeFileThrow file
  ".json" -> do
    content <- BS.readFile file
    case Aeson.eitherDecodeStrict content of
      Left err -> error $ "JSON parse error: " ++ err
      Right ts -> return ts
  ext -> error $ "Unsupported file extension: " ++ ext

---------------------------------------------------------------------------------
--- MAIN ENTRY POINT ------------------------------------------------------------
---------------------------------------------------------------------------------
--- this should run any json/yaml test set --------------------------------------
---------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs  
  case args of
    [filename] -> do                         -- confirms single yaml/json
      tests <- loadTests filename
      startTime <- getCurrentTime            -- START TIMING
      results <- mapConcurrently runTest tests  -- runs tests parallel
      endTime <- getCurrentTime              -- END TIMING
      let passedCount = length (filter id results)
          totalCount = length results
          runtime = diffUTCTime endTime startTime
      putStrLn $ "\nSummary: " ++ show passedCount ++ " / " ++ show totalCount ++ " tests passed."
      putStrLn $ "Runtime: " ++ show runtime    -- PRINT RUNTIME
      if passedCount == totalCount then exitSuccess else exitFailure
    _ -> putStrLn "Usage: htest <tests.yaml|tests.json>"   
