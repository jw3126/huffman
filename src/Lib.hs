module Lib
    ( encodeList
    , decodeList
    , huffmanTree
    ) where

import Control.Applicative
import Control.Monad

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
--import Data.List

type Weight = Double
type BitList = [Bool]
type Distribution t = [(Weight, t)]

data CodeTree a = Leaf a | Branch (CodeTree a) (CodeTree a)
   deriving (Eq, Show)

instance Functor CodeTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch s t) = Branch (fmap f s) (fmap f t)

instance Applicative CodeTree where
    pure x = Leaf x
    (Leaf f) <*> t = fmap f t
    (Branch af ag) <*> t = Branch (af <*> t) (ag <*> t)
    
_join :: CodeTree (CodeTree t) -> CodeTree t
_join (Leaf t) = t
_join (Branch s t) = Branch (_join s) (_join t)

instance Monad CodeTree where
    return x = pure x
    t >>= f = _join $ fmap f t

decodeFirst :: CodeTree t -> BitList -> Maybe (BitList, t)
decodeFirst (Leaf x) bs = Just (bs, x)
decodeFirst (Branch left right) (b:bs) = let
    tree = if b then right else left
    in
    decodeFirst tree bs
decodeFirst (Branch l r) [] = Nothing

decodeList :: CodeTree t -> BitList -> (BitList, [t])
decodeList ct [] = ([], [])
decodeList ct bs = case (decodeFirst ct bs) of
    Nothing -> (bs, [])
    Just (bs_1, x) -> let
        (bs_2, xs) = decodeList ct bs_1
        in
        (bs_2, x:xs)

encodingDict :: (Ord t) => CodeTree t -> Map t BitList
encodingDict (Leaf t) = Map.fromList [(t, [])]
encodingDict (Branch l r) = let 
    leftMap = fmap (False:) (encodingDict l) 
    rightMap = fmap (True:) (encodingDict r) 
    in
    Map.union leftMap rightMap

encodeList :: (Ord t) => CodeTree t -> [t] -> (BitList, [t])
encodeList codeTree [] = ([], [])
encodeList codeTree (x:xs) = case Map.lookup x $ encodingDict codeTree of
    Nothing -> ([], (x:xs))
    Just codeFront -> let
        (codeRest, xRest) = encodeList codeTree xs
        in
        (codeFront ++ codeRest, xRest)

-- compute optimal code tree
huffmanTree :: Distribution t -> CodeTree t
huffmanTree dist = let
    -- toWTree :: (Weight, t) -> (Weight, CodeTree t)
    toWTree (w, x) = (w, Leaf x)
    -- wTrees :: Distribution (CodeTree t)
    wTrees = fmap toWTree dist
    in snd $ mergeLightest wTrees

merge2 :: (Weight, CodeTree t) -> (Weight, CodeTree t) -> (Weight, CodeTree t)
merge2 (w1, t1) (w2, t2) = (w1+w2, Branch t1 t2)

mergeLightest :: Distribution (CodeTree t) -> (Weight, CodeTree t)
mergeLightest [] = error("Empty distribution, nothing to encode")
mergeLightest [x] = x
mergeLightest wtrees = let
    [wt1, wt2] = take 2 $ sortOn fst wtrees
    rest = drop 2 $ sortOn fst wtrees
    in
    mergeLightest $ (merge2 wt1 wt2):rest 
