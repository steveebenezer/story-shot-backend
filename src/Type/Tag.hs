{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Type.Tag
  ( Tag
  , TagInsert
  , TagPut
  , TagWrite
  , TagRead
  , tagTable
  , mkTagPut
  , mkTagWrite
  , mkTagWrite'
  , tagID
  , tagColID
  , tagName
  , tagColName
  , tagGenre
  , tagLinks
  , tagIdentifier
  ) where

import Data.Monoid ((<>))
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Data.Text (pack)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , object
  , withObject
  , (.=)
  , (.:)
  )
import Network.JSONApi
  ( Links
  , ResourcefulEntity(..)
  , Identifier(..)
  , mkLinks
  )
import Opaleye
  ( Column
  , PGInt4
  , PGText
  , Table(Table)
  , PGTimestamptz
  , constant
  , required
  , optional
  )

import Utils (toURL)
import Type.Genre
import Class.Versioned


-- Strangely Polymorphic data type (Internal Use)

data Tag' id' name genre createdAt updatedAt =
  Tag
    { _tagID :: id'
    , _tagName :: name
    , _tagGenre :: genre
    , _createdAt :: createdAt
    , _updatedAt :: updatedAt
    } deriving (Eq, Show, Generic)


-- Types that Will be used
type Tag = Tag' Int Text Genre UTCTime UTCTime
type TagPut = Tag' Int Text Genre () ()
type TagInsert = Tag' () Text Genre () ()
type TagRead = Tag'
  (Column PGInt4)
  (Column PGText)
  (Column PGText)
  (Column PGTimestamptz)
  (Column PGTimestamptz)
type TagWrite = Tag'
  (Maybe (Column PGInt4))
  (Column PGText)
  (Column PGText)
  (Maybe (Column PGTimestamptz))
  (Maybe (Column PGTimestamptz))


instance Versioned Tag where
  createdAt = _createdAt
  updatedAt = _updatedAt


-- Magic
$(makeAdaptorAndInstance "pTag" ''Tag')

-- Opaleye table binding
tagTable :: Table TagWrite TagRead
tagTable = Table "tags" $
  pTag
    Tag
      { _tagID = optional "id"
      , _tagName = required "name"
      , _tagGenre = required "genre"
      , _createdAt = optional "created_at"
      , _updatedAt = optional "updated_at"
      }


-- Some Helpers

mkTagPut :: Int -> Text -> Genre -> TagPut
mkTagPut tid name genre = Tag
  { _tagID = tid
  , _tagName = name
  , _tagGenre = genre
  , _createdAt = ()
  , _updatedAt = ()
  }

mkTagWrite' :: TagInsert -> TagWrite
mkTagWrite' Tag{..} = Tag
  { _tagID = Nothing
  , _tagName = constant _tagName
  , _tagGenre = constant _tagGenre
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }

mkTagWrite :: TagPut -> TagWrite
mkTagWrite Tag{..} = Tag
  { _tagID = constant $ Just _tagID
  , _tagName = constant _tagName
  , _tagGenre = constant _tagGenre
  , _createdAt = Nothing
  , _updatedAt = Nothing
  }

tagID :: Tag' Int b c d e -> Int
tagID = _tagID

tagName :: Tag' a Text c d e -> Text
tagName = _tagName

tagGenre :: Tag' a b Genre d e -> Genre
tagGenre = _tagGenre

tagColID :: TagRead -> Column PGInt4
tagColID = _tagID

tagColName :: TagRead -> Column PGText
tagColName = _tagName


-- JSON

instance ToJSON Tag where
  toJSON Tag{..} = object
    [ "id" .= _tagID
    , "name" .= _tagName
    , "genre" .= _tagGenre
    , "created-at" .= _createdAt
    , "updated-at" .= _updatedAt
    ]

instance FromJSON Tag where
  parseJSON = withObject "tag" $ \o -> Tag
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "genre"
    <*> o .: "created-at"
    <*> o .: "updated-at"

instance FromJSON TagInsert where
  parseJSON = withObject "tag" $ \o -> Tag
    <$> pure ()
    <*> o .: "name"
    <*> o .: "genre"
    <*> pure ()
    <*> pure ()

instance FromJSON TagPut where
  parseJSON = withObject "tag" $ \o -> Tag
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "genre"
    <*> pure ()
    <*> pure ()

-- JSON API

tagIdentifier :: Tag -> Identifier
tagIdentifier tag = Identifier
  (pack . show . tagID $ tag)
  "Tag"
  (resourceMetaData tag)

instance ResourcefulEntity Tag where
  resourceIdentifier = pack . show . tagID
  resourceType _ = "Tag"
  resourceLinks = Just . tagLinks
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing

tagLinks :: Tag -> Links
tagLinks tag = mkLinks [("self", selfLink)]
  where
    selfLink = toURL selfPath
    selfPath = "/tag/" <> show (tagID tag)