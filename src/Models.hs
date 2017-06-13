module Models where

import           Database.PostgreSQL.Simple (Connection, execute_)

import qualified Models.Media as M
import qualified Models.Projects as P

makeMigrations :: Connection -> IO ()
makeMigrations conn = do
  print "creating projects table"
  execute_ conn P.createProjectsTable
  print "creating media table"
  execute_ conn M.createMediaTable
  print "creating post-media relationship table"
  execute_ conn M.createPostMediaTable
  pure ()
