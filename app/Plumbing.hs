{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad.IO.Class     (liftIO)
import           Data.Int                   (Int64)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection, execute, connectPostgreSQL)
import           Options.Applicative
import           System.IO
import           Web.Users.Types            (PasswordPlain (..), User (..),
                                             UserStorageBackend (..),
                                             makePassword)

import qualified Api                        as A
import qualified Config                     as C
import           Models.Author              (Author (..))

data App = App { makeTables     :: Bool
                , cleanSessions :: Bool
                , newUser       :: Bool}

parser :: Parser App
parser = App
  <$> switch
      ( long "maketables"
      <> short 'm'
      <> help "Whether to make new tables for the app")
  <*> switch
      ( long "clean"
      <> short 'c'
      <> help "Whether to clean up old user sessions")
  <*> switch
      ( long "user"
      <> short 'u'
      <> help "Whether to enter user-creation loop" )

main :: IO ()
main = do
  connString <- C.makeConnString
  connection <- case connString of
    Nothing -> throwIO (userError "Database Configuration not present in environment.")
    Just connInfo -> connectPostgreSQL connInfo
  execParser opts >>= runWithOptions connection
  where
    opts = info parser mempty

userLoop :: Connection -> Bool -> IO ()
userLoop _ False = return ()
userLoop conn _  = do
  _ <- makeUser conn
  putStrLn "Would you like to make another user? y|n"
  hFlush stdout
  answer <- getLine
  if answer == "y" then userLoop conn True else return ()

makeUser :: Connection -> IO User
makeUser conn = do
    putStrLn "Making new active user"
    putStrLn "Enter username: "
    hFlush stdout
    uname <- T.pack <$> getLine
    putStrLn "Enter email address: "
    hFlush stdout
    email <- T.pack <$> getLine
    putStrLn "Enter password: "
    hFlush stdout
    passwd <- (makePassword . PasswordPlain . T.pack) <$> withEcho False getLine
    let user = User uname email passwd True
    res <- createUser conn user
    case res of
      Left err -> throwIO (userError $ show err)
      Right userId -> makeAuthor userId conn
    return user

makeAuthor :: Int64 -> Connection -> IO ()
makeAuthor userId conn = do
  putStrLn "Enter User First Name"
  hFlush stdout
  fname <- T.pack <$> getLine
  putStrLn "Enter User Last Name"
  hFlush stdout
  lname <- T.pack <$> getLine
  let q = "insert into author (userid, firstName, lastName) values (?, ?, ?)"
  _ <- liftIO $ execute conn q (userId, fname, lname) :: IO Int64
  return ()

runWithOptions :: Connection -> App -> IO ()
runWithOptions conn (App True True mkUser) =
  A.createAllTables conn >>
    housekeepBackend conn >>
      userLoop conn mkUser
runWithOptions conn (App True False mkUser) = A.createAllTables conn >> userLoop conn mkUser
runWithOptions conn (App False True mkUser) = housekeepBackend conn >> userLoop conn mkUser
runWithOptions conn (App False False mkUser) = userLoop conn mkUser

withEcho :: Bool -> IO a -> IO a
withEcho echo action' = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action'
