{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving Show

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."
-- >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."
-- >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "...BA"
--
torder :: Order    -- ^ Order of resulting traversal
       -> Maybe a  -- ^ Optional leaf value
       -> Tree a   -- ^ Tree to traverse
       -> [a]      -- ^ List of values in specified order
      
torder order leaf tree = case order of
        PreOrder -> pretorder leaf tree
        InOrder -> itorder leaf tree
        PostOrder -> postorder leaf tree

-- | Returns empty list of Maybe is Nothing, othrwise (in case of Just x) returns [x]
-- 
-- Usage example:
-- 
-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- | Returns list of tree's nodes in pre-order
-- 
-- Usage example:
-- 
-- >>> pretorder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."  

pretorder :: Maybe a -> Tree a -> [a]
pretorder leaf tree = case tree of
        Leaf           -> maybeToList leaf
        (Branch a l r) -> [a] ++ pretorder leaf l ++ pretorder leaf r



-- | Returns list of tree's nodes in in-order 
--
-- Usage example:
--
-- >>> itorder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."

itorder :: Maybe a -> Tree a -> [a]
itorder leaf tree = case tree of
        Leaf           -> maybeToList leaf
        (Branch a l r) -> itorder leaf l ++ [a] ++ itorder leaf r


-- | Returns list of tree's nodes in post-order
--
-- Usage example:
--
-- >>> postorder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "...BA"
 

postorder :: Maybe a -> Tree a -> [a]
postorder leaf tree = case tree of
        Leaf           -> maybeToList leaf
        (Branch a l r) -> postorder leaf l ++ postorder leaf r ++ [a]

-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|C..|A.B.."
-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|.C.|.A.B."
-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|..C|...BA"
--

forder :: Order     -- ^ Order of tree traversal
       -> Maybe a   -- ^ Optional separator between resulting tree orders
       -> Maybe a   -- ^ Optional leaf value
       -> Forest a  -- ^ List of trees to traverse
       -> [a]       -- ^ List of values in specified tree order
forder order sep leaf forest = intercalate (maybeToList sep) (map (torder order leaf) forest)
-- | intercalate xs xss is equivalent to (concat (intersperse xs xss)).
--  It inserts the list xs in between the lists in xss and concatenates the result.
--
-- Usage example:
-- 
-- >>> intercalate ", " ["Lorem", "ipsum", "dolor"]
-- "Lorem, ipsum, dolor"
-- >>> intercalate [0, 1] [[2, 3], [4, 5, 6], []]
-- [2,3,0,1,4,5,6,0,1]
-- >>> intercalate [1, 2, 3] [[], []]
-- [1, 2, 3]

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs
