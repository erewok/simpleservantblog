{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad.IO.Class     (liftIO)
import           Data.Int                   (Int64)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL,
                                             execute)
import           Options.Applicative
import           System.IO
import qualified Web.Users.Types            as WU
import           Web.Users.Types            (PasswordPlain (..),
                                             UserStorageBackend (..),
                                             makePassword)

import qualified Api                        as A
import qualified Config                     as C
import qualified Models                     as M

data App = App { makeTables      :: Bool
                , cleanSessions  :: Bool
                , newUser        :: Bool
                , changePass     :: Bool
                , makeMigrations :: Bool}

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
  <*> switch
      ( long "passwd"
      <> short 'p'
      <> help "Change User password" )
  <*> switch
      ( long "migrations"
      <> help "Create new tables" )

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

makeUser :: Connection -> IO WU.User
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
    passwd <- (WU.makePassword . PasswordPlain . T.pack) <$> withEcho False getLine
    let user = WU.User uname email passwd True
    res <- WU.createUser conn user
    case res of
      Left err     -> throwIO (userError $ show err)
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
  pure ()


updateUserPass :: WU.Password -> WU.User -> WU.User
updateUserPass pass user = user { WU.u_password = pass }

changePasswd :: Connection -> Bool -> IO ()
changePasswd _ False = pure ()
changePasswd conn True = do
  putStrLn "Enter username: "
  hFlush stdout
  uname <- T.pack <$> getLine
  putStrLn "Enter current password: "
  hFlush stdout
  passwd <- (WU.PasswordPlain . T.pack) <$> withEcho False getLine
  authResult <- liftIO $ WU.authUser conn uname passwd 12000000
  case authResult of
    Nothing -> putStrLn "No Such User Exists"
    Just _ -> do
      userid <- liftIO $ WU.getUserIdByName conn uname
      case userid of
        Nothing -> putStrLn "No Such User Exists"
        Just uid -> do
          putStrLn "Enter new password: "
          hFlush stdout
          passwd <- (WU.makePassword . PasswordPlain . T.pack) <$> withEcho False getLine
          putStrLn "Enter new password again: "
          verify <- (WU.makePassword . PasswordPlain . T.pack) <$> withEcho False getLine
          if verify == passwd
            then WU.updateUser conn uid (updateUserPass passwd) >> pure ()
            else putStrLn "Passwords don't match: FAIL"
  return ()

migrate :: Connection -> Bool -> IO ()
migrate _ False   = pure ()
migrate conn True = M.makeMigrations conn


runUserPasswdMigrations :: Connection -> Bool -> Bool -> Bool -> IO ()
runUserPasswdMigrations conn mkUser changePass migrations =
  userLoop conn mkUser >>
    changePasswd conn changePass >>
       migrate conn migrations

runWithOptions :: Connection -> App -> IO ()
runWithOptions conn (App True True mkUser changePass migrations) =
  A.createAllTables conn >>
    housekeepBackend conn >> runUserPasswdMigrations conn mkUser changePass migrations

runWithOptions conn (App True False mkUser changePass migrations) =
  A.createAllTables conn >> runUserPasswdMigrations conn mkUser changePass migrations
runWithOptions conn (App False True mkUser changePass migrations) =
  housekeepBackend conn >> runUserPasswdMigrations conn mkUser changePass migrations
runWithOptions conn (App False False mkUser changePass migrations) = runUserPasswdMigrations conn mkUser changePass migrations

withEcho :: Bool -> IO a -> IO a
withEcho echo action' = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action'
