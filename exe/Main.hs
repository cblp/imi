{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString              as BS
import           Data.Text
import           Data.Yaml                    as Yaml
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           NeatInterpolation

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
        Part
            Id    Int
            name  Text

            deriving Show

        Supplier  json
            Id    Int
            name  Text

            deriving Show

        Order
            part PartId
            supplier SupplierId
    |]

main :: IO ()
main = runDB $ do
    -- fill test data
    p1 <- insert $ Part "A"
    p2 <- insert $ Part "B"
    p3 <- insert $ Part "C"
    s1 <- insert $ Supplier "Завод1"
    s2 <- insert $ Supplier "Завод2"
    s3 <- insert $ Supplier "Завод3"
    insert_ $ Order p1 s1
    insert_ $ Order p2 s1
    insert_ $ Order p3 s1
    insert_ $ Order p2 s2
    insert_ $ Order p3 s2
    insert_ $ Order p3 s3

    -- run test query
    suppliers :: [Entity Supplier] <-
        rawSql
            [text|
                select ?? from supplier where not exists (
                    select id from part where not exists (
                        select id from 'order'
                        where supplier.id = 'order'.supplier
                            and part.id = 'order'.part
                    )
                )
            |]
            []
    liftIO $ yprint suppliers

runDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runDB action =
    runSqlite ":memory:" $ do
    -- runSqlite "test.db" $ do
        runMigration migrateAll
        action

yprint :: ToJSON a => a -> IO ()
yprint = BS.putStr . Yaml.encode
