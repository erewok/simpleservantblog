module Models where

import Control.Exception          (catch, displayException, SomeException)
import Database.PostgreSQL.Simple (Connection, execute_)
import GHC.Int

import qualified Models.Media as M
import qualified Models.Projects as P

data ModelType = Authors
               | Series
               | Posts
               | Media
               | Projects
               deriving (Eq, Show)


execHandler :: SomeException -> IO ()
execHandler e =  do
  print "It Blew up!\n"
  putStrLn $ displayException e

makeMigrations :: Connection -> IO ()
makeMigrations conn = do
  catch (execute_ conn P.createProjectsTable >> print "creating projects table") execHandler
  catch (execute_ conn M.createMediaTable >> print "creating media table") execHandler
  catch (execute_ conn M.createPostMediaTable >> print "creating post-media relationship table") execHandler
  pure ()
