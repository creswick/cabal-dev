{-|

Merge configuration files for Cabal (and cabal-install).

-}
module Distribution.Dev.MergeCabalConfig
    ( mergeFields
    , isFlaggedForRemoval
    , filterFields
    , removeFlaggedFields
    )
where

import Data.Maybe              ( fromMaybe, mapMaybe )
import Control.Monad           ( guard )
import Control.Applicative     ( Applicative, Alternative, pure, empty, (<|>), (<$>) )
import Distribution.ParseUtils ( Field(..) )

-- |Merge two lists of fields.
--
-- The fields from the second list are merged into the first list of
-- fields.
--
-- >>> mergeFields [F 1 "foo" "bar"] [F 2 "baz" "quux"]
-- [F 2 "baz" "quux", F 1 "foo" "bar"]
-- >>> mergeFields [F 1 "foo" "bar"] [F 2 "foo" "quux"]
-- [F 2 "foo" "quux"]
mergeFields :: [Field] -- ^Starting fields
            -> [Field] -- ^New fields
            -> [Field] -- ^Merged fields (preferring fields from the
                       -- second argument)
mergeFields = foldr $ \f fs ->
              fromMaybe (f:fs) $ replace (mergeField mergeFields f) fs

-- |Filter a configuration based on a predicate. Filters at the top
-- level as well as inside of Sections.
--
-- >>> let p f = case f of { F _ _ "bar" -> False; _ -> True; }
-- >>> filterFields p [F 0 "foo" "bar", Section 1 "quux" "beep" [], F 2 "arf" "baz"]
-- [Section 1 "quux" "beep" [], Field 2 "arf" "baz"]
filterFields :: (Field -> Bool) -> [Field] -> [Field]
filterFields p = mapMaybe $ \f -> guard (p f) >> return (recurseSection f)
    where
      recurseSection :: Field -> Field
      recurseSection (Section l n a fs) = Section l n a $ filterFields p fs
      recurseSection f                  = f

-- |Look for the magic value "USE-DEFAULT" as the value of a leaf
-- (Field). This means that there is no way to set a field to the
-- value "USE-DEFAULT" in a cabal configuration for use with
-- cabal-dev.
--
-- >>> isFlaggedForRemoval (F 0 "root-cmd" "sudo")
-- False
-- >>> isFlaggedForRemoval (F 0 "root-cmd" "USE-DEFAULT")
-- True
isFlaggedForRemoval :: Field -> Bool
isFlaggedForRemoval (F _ _ "USE-DEFAULT") = True
isFlaggedForRemoval _                     = False

-- |Recursively remove all fields whose value is "USE-DEFAULT" from
-- the configuration.
--
-- >>> removeFlaggedFields [F 0 "foo" "USE-DEFAULT", Section 1 "quux" "beep" [], F 2 "arf" "baz"]
-- [Section 1 "quux" "beep" [], F 2 "arf" "baz"]
removeFlaggedFields :: [Field] -> [Field]
removeFlaggedFields = filterFields $ not . isFlaggedForRemoval

-- |Attempt to merge two fields.
--
-- * Two fields can be merged if they have the same type and the same
--   identifiers.
-- * If two sections match, their fields are merged recursively.
-- * If two fields match, the value of the first field is taken.
--
-- >>> let m = mergeField mergeFields :: Field -> Field -> Maybe Field
-- >>> let fooF1 = F 1 "foo" "bar"
-- >>> let fooF2 = F 2 "foo" "baz"
-- >>> let fooSec1 = Section 3 "foo" "bar" [F 4 "a" "b"]
-- >>> let fooSec2 = Section 5 "foo" "bar" [F 6 "x" "y"]
-- >>> m fooF1 fooF2
-- Just (F 1 "foo" "bar")
-- >>> m fooF2 fooF1
-- Just (F 2 "foo" "baz")
-- >>> m fooF1 fooSec1
-- Nothing
-- >>> m fooSec1 fooF1
-- Nothing
-- >>> m fooSec1 fooSec2
-- Just (Section "foo" "bar" [F 2 "x" "y", F 1 "a" "b"])
-- >>> m fooSec2 fooSec1
-- Just (Section "foo" "bar" [F 1 "a" "b", F 2 "x" "y"])
mergeField :: (Applicative f, Alternative f) =>
              ([Field] -> [Field] -> [Field])
           -> Field -> Field -> f Field
mergeField _ (F l k v)           (F _ k' _)
    | k == k'            = pure $ F l k v
mergeField m (Section l n a fs') (Section _ n' a' fs'')
    | n == n' && a == a' = pure $ Section l n a $ m fs'' fs'
mergeField _ _                   _
    = empty

-- | Attempt to replace a value in a list with a given a replacement
-- function.
--
-- The first value that does not yield 'empty' for the replacement
-- function is replaced by the value. At this point, the computation
-- short-circuits. If no value yields a replacement, empty is returned.
--
-- >>> replace (`lookup` [(3,5)]) [1..5] :: Maybe [Int]
-- Just [1,2,5,4,5]
-- >>> replace (`lookup` [(3,5)]) [3,3,3] :: Maybe [Int]
-- Just [5,3,3]
-- >>> replace (`lookup` [(11,5)]) [1..5] :: Maybe [Int]
-- Nothing
-- >>> replace (\x -> if x == 11 then [5,9] else []) [1..5] :: [[Int]]
-- []
-- >>> replace (\x -> if x == 3 then [5,9] else []) [1..5] :: [[Int]]
-- [[1,2,5,4,5],[1,2,9,4,5]]
-- >>> replace (const $ Just "hello") [undefined, "world"] :: Maybe [String]
-- Just ["hello","world"]
replace :: (Applicative f, Alternative f) => (a -> f a) -> [a] -> f [a]
replace f = go id
    where
      go _   []     = empty
      go acc (x:xs) = (rebuild <$> f x) <|> go (acc . (x:)) xs
          where
            rebuild x' = acc $ x' : xs
