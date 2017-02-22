{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Models where

import           Database.PostgreSQL.Simple (Connection, execute_)

import qualified Models.Media as M

makeMigrations :: Connection -> IO ()
makeMigrations conn = print "creating media table"
  >> execute_ conn M.createMediaTable
    >> print "creating post-media relationship table"
      >> execute_ conn M.createPostMediaTable
        >> pure ()
