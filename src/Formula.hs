{-# LANGUAGE InstanceSigs #-}
module Formula where

import Data.HashMap.Strict as Map
import Data.HashSet as Set
import Data.Hashable

type Node = Int
type Color = Int
type NumOfNodes = Int
type NumOfColors = Int

class (Eq a, Hashable a, Enum a, Show a) => NameRepr a where
    makeRepr :: Node -> Node -> Color -> a

instance (NameRepr Int) where
    -- if big formula will be passed to cnfTseitin == gg
    -- collisions will be possible
    -- there is space for 1000 nodes graph and 1000 colors
    makeRepr i j c = 1000000 * i + 1000 * j + c

data Formula a
    = Var a
    | Not (Formula a)
    | And [(Formula a)]
    | Or [(Formula a)] deriving (Eq)

instance (Show a) => Show (Formula a) where
    show (Var x)  = show x
    show (Not f)  = "!(" ++ show f ++ ")"
    show (Or []) = error "Empty Or"
    show (And []) = error "Empty And"
    show (Or (_:[])) = error "Or should be binary, not unary"
    show (And (_:[])) = error "And should be binary, not unary"
    show (And (f:fs)) = "(" ++ show f ++ (foldl (\acc frm -> acc ++ " & " ++ (show frm)) "" fs) ++ ")"
    show (Or (f:fs))  = "(" ++ show f ++ (foldl (\acc frm -> acc ++ " | " ++ (show frm)) "" fs) ++ ")"

type Ctx a = HashMap a Bool

eval :: (NameRepr a) => Ctx a -> Formula a -> Bool
eval ctx (Var x)  = lookupDefault False x ctx
eval ctx (Not f)  = not $ eval ctx f
eval ctx (And fs) = all (eval ctx) fs
eval ctx (Or fs)  = any (eval ctx) fs

data Literal a = PosVar a | NegVar a deriving (Eq)
instance (Hashable a) => Hashable (Literal a) where
    hashWithSalt :: (Hashable a) => Int -> Literal a -> Int
    hashWithSalt salt (PosVar x) = hashWithSalt salt x
    hashWithSalt salt (NegVar x) = -1 * hashWithSalt salt x

instance (Show a) => Show (Literal a) where
    show :: (Show a) => Literal a -> String
    show (PosVar x) = show x
    show (NegVar x) = '!' : show x

type LiteralSet a = HashSet (Literal a)
type Cnf a = [LiteralSet a]

getNewVar :: a -> Formula a
getNewVar x = Var x

getAllLiterals :: (Eq a, Hashable a) => Formula a -> LiteralSet a
getAllLiterals (Var x) = Set.singleton (PosVar x)
getAllLiterals (Not x) = 
    case x of
        Var y -> Set.singleton (NegVar y)
        _ -> getAllLiterals x
getAllLiterals (And fs) = foldl (\acc f -> Set.union acc (getAllLiterals f)) Set.empty fs
getAllLiterals (Or fs)  = foldl (\acc f -> Set.union acc (getAllLiterals f)) Set.empty fs

safelyAddNot :: Formula a -> Formula a
safelyAddNot (Not (Not x)) = safelyAddNot x
safelyAddNot (Not x) = x
safelyAddNot x = Not x

getNegated :: Literal a -> Literal a
getNegated (PosVar x) = NegVar x
getNegated (NegVar x) = PosVar x
