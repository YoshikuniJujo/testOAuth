{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Lib

import Database.Persist
import Database.Persist.TH
	(share, mkPersist, sqlSettings, mkMigrate, persistUpperCase)
import Database.Persist.Sql (runMigration, runSqlConn)
import Database.Persist.Sqlite (withSqliteConn)

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
	name		String
	age		Int Maybe
BlogPost
	title		String
	authorId	PersonId
|]

database = ":memory:"

main :: IO ()
main = runNoLoggingT $ runResourceT $ withSqliteConn database $ runSqlConn $ do
	runMigration migrateAll

	johnId <- insert $ Person "John Doe" $ Just 35
	janeId <- insert $ Person "Jane Doe" $ Nothing

	insert $ BlogPost "My fr1st p0st" johnId
	insert $ BlogPost "One more for good measure" johnId

	oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 2]
	liftIO $ print $ map (blogPostTitle . entityVal) oneJohnPost

	john <- get johnId
	liftIO $ print $ fmap personName john

	delete janeId
	deleteWhere [BlogPostAuthorId ==. johnId]

	return ()
