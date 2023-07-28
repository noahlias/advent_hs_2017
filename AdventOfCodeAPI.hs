{-# LANGUAGE OverloadedStrings #-}

module AdventOfCodeAPI
  ( getInput,
  )
where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import Network.HTTP.Types.Status (statusCode)


-- Read the session value from the .env file
getSessionFromEnv :: IO (Maybe BSC.ByteString)
getSessionFromEnv = do
  _ <- loadFile defaultConfig
  session <- lookupEnv "SESSION"
  return $ BSC.pack <$> session


-- Function to remove the last character if it's a newline
removeLastNewline :: String -> String
removeLastNewline str =
  if last str == '\n'
    then init str
    else str


-- Function to fetch the input file for a given day from Advent of Code
getInput :: Int -> Int -> IO (Either String String)
getInput year day = do
  -- Attempt to get the session key from the environment variable
  session <- getSessionFromEnv
  case session of
    Just c -> do
      let url = "https://adventofcode.com/" ++ show year ++ "/day/" ++ show day ++ "/input"
      -- putStrLn $ "Fetching data from: " ++ url
      initialRequest <- parseRequest url
      let request = setRequestHeader "Cookie" [BSC.append "session=" c] initialRequest
      -- putStrLn $ "Request prepared with session key: " ++ BSC.unpack c
      response <- httpLBS request
      -- putStrLn "Received response..."
      let status = getResponseStatus response
      if statusCode status == 200
        then do
            -- putStrLn "Request was successful, processing response..."
            return $ Right (removeLastNewline . BSC.unpack . BSC.concat . LBS.toChunks $ getResponseBody response)
        else do
            -- putStrLn "Request failed..."
            return $ Left ("Failed to fetch the input file. Status code: " ++ show (statusCode status))
    Nothing -> do
        -- putStrLn "No session key found..."
        return $ Left "No session key found. Set the SESSION environment variable or provide it in the .env file."
