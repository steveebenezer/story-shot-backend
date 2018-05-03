{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}


module Resource.User
  ( getUserResources
  , getUserResource
  , createUserResources
  , createUserResource
  , updateUserResources
  , updateUserResource
  , deleteUserResources
  , deleteUserResource
  ) where

import qualified Data.Map as M
import qualified Data.Maybe as DM
import qualified Data.Either as DEither

import qualified Data.Text as Text

import qualified Init as I
import qualified Class.Includes as CI
import qualified Type.Pagination as TP
import qualified Type.Meta as TM
import qualified Type.Or as Or
import qualified Type.Doc as TD
import qualified Type.User as TU
import qualified Type.Author as TA
import qualified Type.AppError as TAe
import qualified Storage.User as SU
import qualified Storage.Author as SA
import qualified Library.Link as LL



-- CREATE

createUserResource :: Either TAe.ClientError [UserIncludes] -> TU.UserInsert -> I.AppT (TD.MaybeResource TU.User)
createUserResource (Left e) _ = return . Left $ TAe.docError e
createUserResource (Right includes) user = do
  author <- SA.createAuthor $ TA.mkAuthorInsert $ TU.userName user
  user <- SU.createUser user author
  return . Right . TM.indexDoc' . head $ _linkAll includes [user] [author]


createUserResources :: Either TAe.ClientError [UserIncludes] -> [TU.UserInsert] -> I.AppT (TD.MaybeResource TU.User)
createUserResources (Left e) _ = return . Left $ TAe.docError e
createUserResources (Right includes) users = do
  authors <- SA.createAuthors $ fmap (TA.mkAuthorInsert . TU.userName) users
  users <- SU.createUsers users authors
  return . Right . TM.docMulti $ _linkAll includes users authors



-- RETRIVE

getUserResources :: TP.CursorParam -> Either TAe.ClientError [UserIncludes]-> I.AppT (TD.MaybeResource TU.User)
getUserResources _ (Left e) = return . Left $ TAe.docError e
getUserResources cur (Right includes) = do
  users <- SU.getUsers cur
  (Right . TM.docMulti) <$> _fromPGUsers includes users


getUserResource :: Int -> Either TAe.ClientError [UserIncludes] -> I.AppT (TD.MaybeResource TU.User)
getUserResource _ (Left e) = return $ Left $ TAe.docError e
getUserResource sid (Right includes) = do
  mstory <- SU.getUser sid
  case mstory of
    Nothing      -> return $ Left $ TAe.docError TAe.ResourceNotFound
    Just pgstory -> Right . TM.indexDoc' <$> _fromPGUser includes pgstory



_linkAll :: [UserIncludes] -> [TU.PGUser] -> [TA.Author] -> [TU.User]
_linkAll [] users _ = zipWith TU.mkUserFromDB users $ fmap (Or.Or . Left . TA.mkAuthorS . TU.userAuthorID) users
_linkAll [IAuthor] users authors =
  let
    aMap = M.fromList [(TA.authorID author, author) | author <- authors]
    authors' = fmap (Or.Or . Right . (aMap M.!) . TU.userAuthorID) users
  in
    zipWith TU.mkUserFromDB users authors'


-- _linkAll' :: [TU.PGUser] -> Either (M.Map Int TA.AuthorS) (M.Map Int TA.Author) -> [TU.User]
-- _linkAll' users idAuthorMap =
--   let
--     _convert :: Either (M.Map k a) (M.Map k b) -> M.Map k (Or.Or a b)
--     _convert (Left m) = fmap (Or.Or . Left) m
--     _convert (Right m) = fmap (Or.Or . Right) m
--
--     getAuthorFor :: Int -> Or.Or TA.AuthorS TA.Author
--     getAuthorFor pid = DM.fromJust $ M.lookup pid $ _convert idAuthorMap
--
--     getUser' sg = TU.mkUserFromDB sg (getAuthorFor pid)
--       where pid = TU.pgUserID sg
--   in
--     map getUser' users


_fromPGUsers :: [UserIncludes] -> [TU.PGUser] -> I.AppT [TU.User]
_fromPGUsers includes users = do
  let
    userIDs = map TU.pgUserID users
    authorIDs = map TU.userAuthorID users
    userAuthorIDMap = M.fromList $ zip userIDs authorIDs

  authorsMap <- if IAuthor `elem` includes
                   then do
                     authors <- SA.getMultiAuthors authorIDs
                     let
                       authorIDToAuthorMap = M.fromList [(TA.authorID a, a) | a <- authors]
                     return . Right $ fmap (authorIDToAuthorMap M.!) userAuthorIDMap
                   else return . Left $ fmap TA.mkAuthorS userAuthorIDMap

  return $ LL._linkAll users [authorsMap] []


_fromPGUser :: [UserIncludes] -> TU.PGUser -> I.AppT TU.User
_fromPGUser includes pguser = do
  let
    sid = TU.pgUserID pguser
    aid = TU.userAuthorID pguser
  eauthor <- if IAuthor `elem` includes
                then do
                  mauthor <- SA.getAuthor aid
                  case mauthor of
                    Nothing     -> error $ "An author should exist with ID: " ++ show aid
                    Just author -> return . Or.Or $ Right author
                else return . Or.Or . Left $ TA.mkAuthorS aid
  return $ TU.mkUserFromDB pguser eauthor





-- UPDATE

updateUserResource :: TU.UserPut -> I.AppT (TD.MaybeResource TU.User)
updateUserResource = undefined -- fmap TM.docOrError . SU.updateUser


updateUserResources :: [TU.UserPut] -> I.AppT (TD.MaybeResource TU.User)
updateUserResources = undefined -- fmap TM.docMulti . SU.updateUsers



-- DELETE

deleteUserResource :: Int -> I.AppT (TD.MaybeResource TU.User)
deleteUserResource = fmap TM.docMetaOrError . SU.deleteUser


deleteUserResources :: [Int] -> I.AppT (TD.Doc TU.User)
deleteUserResources = fmap (TM.docMeta . fromIntegral) . SU.deleteUsers



-- Query Params Processing

data UserIncludes
  = IAuthor
  deriving (Show, Eq)


instance CI.Includes UserIncludes where
  getAll = [IAuthor]

  fromCSV :: Text.Text -> Either TAe.ClientError [UserIncludes]
  fromCSV =
    let
      fromString :: Text.Text -> Either TAe.ClientError UserIncludes
      fromString "author" = Right IAuthor
      fromString _ = Left TAe.InvalidQueryParams

      f :: ([TAe.ClientError], [UserIncludes]) -> Either TAe.ClientError [UserIncludes]
      f (x:_, _) = Left x
      f (_, ys) = Right ys
   in
      f . DEither.partitionEithers . map fromString . Text.splitOn ","
