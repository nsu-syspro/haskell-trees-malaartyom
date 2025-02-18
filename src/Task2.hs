{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

 module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (compare, foldl, foldr, Ordering(..))

import Task1 (Tree(..), torder, Order(InOrder))
import Data.Maybe (fromMaybe)

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
--
compare :: Ord a => Cmp a
compare x y | x < y      = LT
            | x == y     = EQ
            | otherwise  = GT

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
--
listToBST :: Cmp a -> [a] -> Tree a
listToBST cmp = foldr (tinsert cmp) Leaf

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
--
bstToList :: Tree a -> [a]
bstToList = torder InOrder Nothing

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
--
isBST :: Cmp a -> Tree a -> Bool
isBST cmp tree = case tree of
      Leaf           -> True
      (Branch x l r) -> isCorrectLeft cmp x l && isCorrectRight cmp x r


isCorrectLeft :: Cmp a -> a -> Tree a -> Bool
isCorrectLeft cmp x l = case l of
      Leaf             -> True
      (Branch y yl yr) -> cmp x y == GT && isCorrectLeft cmp y yl && isCorrectRight cmp y yr

isCorrectRight :: Cmp a -> a -> Tree a -> Bool
isCorrectRight cmp x r = case r of
      Leaf             -> True
      (Branch y yl yr) -> cmp x y == LT && isCorrectLeft cmp y yl && isCorrectRight cmp y yr


-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
--
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup cmp x tree = case tree of
            Leaf             -> Nothing
            (Branch val l r) -> case cmp x val of
                  LT -> tlookup cmp x l
                  GT -> tlookup cmp x r
                  EQ -> Just val


-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
--
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert cmp x tree = case tree of
      Leaf             -> Branch x Leaf Leaf
      (Branch val l r) -> case cmp x val of
            LT -> Branch val (tinsert cmp x l) r
            GT -> Branch val l (tinsert cmp x r) -- TODO: Compare and insert 
            EQ -> Branch x l r

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
--
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete cmp x tree = case tree of
      Leaf                    -> Leaf
      (Branch val Leaf r)     -> case cmp x val of
            LT -> Branch val (tdelete cmp x Leaf) r
            GT -> Branch val Leaf (tdelete cmp x r) -- TODO: Compare and delete
            EQ -> r
      (Branch val l Leaf)      -> case cmp x val of
            LT -> Branch val (tdelete cmp x l) Leaf
            GT -> Branch val l (tdelete cmp x Leaf)
            EQ -> l
      (Branch val l r)        -> case cmp x val of
            LT -> Branch val (tdelete cmp x l) r
            GT -> Branch val l (tdelete cmp x r)
            EQ -> Branch (mostLeft r) l (tdelete cmp (mostLeft r) r)


-- I'm sorry i'm tired of leaving comments :(



foldr:: (a -> b -> b) -> b -> [a] -> b
foldr _ s []     = s
foldr f s (x:xs) = foldr f (x `f` s) xs

-- compareAndThen ::Cmp a -> a -> a -
mostLeft :: Tree a -> a
mostLeft Leaf              = error ""
mostLeft (Branch x Leaf _) = x
mostLeft (Branch _ l _)    = mostLeft l 

-- In this version of the solution these functions aren't used, but i don't want to delete them 
defaultLeft :: Tree a -> Tree a
defaultLeft tree = fromMaybe Leaf (left tree)


defualtRight :: Tree a -> Tree a
defualtRight tree = fromMaybe Leaf (right tree)

defaultValue :: a -> Tree a -> a
defaultValue val tree = fromMaybe val (value tree)


left :: Tree a -> Maybe (Tree a)
left Leaf             = Nothing
left (Branch _ l _)   = Just l


right :: Tree a -> Maybe (Tree a)
right Leaf           = Nothing
right (Branch _ _ r) = Just r

value :: Tree a -> Maybe a
value Leaf             = Nothing
value (Branch val _ _) = Just val

