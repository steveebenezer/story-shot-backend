module Library.Link
  where

import qualified Data.Map as M
import qualified Data.Maybe as DM

import qualified Init as I
import qualified Class.Includes as CI
import qualified Type.Or as Or
import qualified Class.Resource as CR


-- _linkAll :: (CR.UnlinkedResource pg, CR.LinkedResource lr, CR.Resource r, CR.ShortResource sr)
--          => [pg]
--          -> [Either (M.Map Int sr) (M.Map Int r)] -> [Either (M.Map Int [sr]) (M.Map Int [r])]
--          -> [lr]
_linkAll pgs rMaps rlMaps =
  let
    _convert :: Either (M.Map k a) (M.Map k b) -> M.Map k (Or.Or a b)
    _convert (Left m) = fmap (Or.Or . Left) m
    _convert (Right m) = fmap (Or.Or . Right) m

    -- getResourceList :: (CR.ShortResource sr, CR.Resource r) => Int -> [Or.Or sr r]
    getResourceList pid = fmap (DM.fromJust . M.lookup pid . _convert) rMaps

    -- getResources :: (CR.ShortResource sr, CR.Resource r) => Int -> [Or.Or [sr] [r]]
    getResources pid = fmap (DM.fromMaybe (Or.Or $ Left []) . M.lookup pid . _convert) rlMaps

    -- mkLinkedResource :: (CR.UnlinkedResource ur, CR.LinkedResource rl) => ur -> rl
    mkLinkedResource pg = 
      CR.mkLinkedResource pg (getResourceList pid) (getResources pid)
        where pid = CR.urid pg
  in
    fmap mkLinkedResource pgs


_fromPG :: (CI.Includes cis, CR.UnlinkedResource pg, CR.LinkedResource lr)
        => [cis] -> pg -> I.AppT lr
_fromPG includes pg = undefined

