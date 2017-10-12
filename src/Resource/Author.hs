{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Resource.Author
  ( getAuthorResources
  , getAuthorResource
  , createAuthorResources
  , createAuthorResource
  , updateAuthorResources
  , updateAuthorResource
  , deleteAuthorResources
  , deleteAuthorResource
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)
import Network.JSONApi
  ( Document
  , ErrorDocument(..)
  , MetaObject(..)
  , Links
  , Meta
  , mkMeta
  , mkLinks
  , mkDocument
  , singleton
  , mkDocument'
  )

import Utils (toURL)
import Init (WithConfig)
import Type.Author
import Storage.Author
import Exception.AppError (APIError, ClientError(..), toErrorDoc)


-- CREATE

createAuthorResource :: AuthorInsert -> WithConfig (Document Author)
createAuthorResource =
  fmap indexDocument' . createAuthor


createAuthorResources :: [AuthorInsert] -> WithConfig (Document Author)
createAuthorResources =
  fmap docMulti . createAuthors



-- RETRIVE

getAuthorResources :: WithConfig (Document Author)
getAuthorResources =
  docMulti <$> getAuthors


getAuthorResource :: Int -> WithConfig (Either (ErrorDocument Author) (Document Author))
getAuthorResource =
  fmap docOrError . getAuthor



-- UPDATE

updateAuthorResource :: AuthorPut -> WithConfig (Either (ErrorDocument Author) (Document Author))
updateAuthorResource = 
  fmap docOrError . updateAuthor


updateAuthorResources :: [AuthorPut] -> WithConfig (Document Author)
updateAuthorResources =
  fmap docMulti . updateAuthors



-- DELETE

deleteAuthorResource :: Int -> WithConfig (Either (ErrorDocument Author) (Document Author))
deleteAuthorResource = fmap docMetaOrError . deleteAuthor


deleteAuthorResources :: [Int] -> WithConfig (Document Author)
deleteAuthorResources = fmap (docMeta . fromIntegral) . deleteAuthors


-- HELPERS

-- JSON API Related

data AuthorMetaData = AuthorMetaData
  { count :: Int
  } deriving (Eq, Show, Generic)

instance MetaObject AuthorMetaData where
  typeName _ = "authorCount"

instance ToJSON AuthorMetaData
instance FromJSON AuthorMetaData

-- Builds the Links data for the 'index' action
indexLinks :: Links
indexLinks = mkLinks [("self", selfLink)]
  where
    selfLink = toURL "/author"


-- Builds the Meta data for the 'index' action
indexMetaData :: [Author] -> Meta
indexMetaData authors = mkMeta AuthorMetaData
  { count = length authors
  }


-- Builds the repsonse Document for the 'index' action
indexDocument :: [Author] -> Links -> Meta -> Document Author
indexDocument authors links meta =
  mkDocument authors (Just links) (Just meta)


indexDocument' :: Author -> Document Author
indexDocument' story' =
  mkDocument' (singleton story') Nothing Nothing


docMulti :: [Author] -> Document Author
docMulti authors =
  indexDocument authors indexLinks $ indexMetaData authors


docMetaOrError :: Int64 -> Either (ErrorDocument Author) (Document Author)
docMetaOrError 0 = Left $ docError ResourceNotFound
docMetaOrError 1 = Right $ indexDocument [] indexLinks $ mkMeta $ AuthorMetaData 1
docMetaOrError _ = error "Impossible"


docMeta :: Int -> Document Author
docMeta = indexDocument [] indexLinks . mkMeta . AuthorMetaData


docError :: APIError e => e -> ErrorDocument a
docError e =
  ErrorDocument (toErrorDoc e) Nothing Nothing


docOrError :: Maybe Author -> Either (ErrorDocument a) (Document Author)
docOrError Nothing = Left $ docError ResourceNotFound
docOrError (Just at) = Right $ indexDocument' at