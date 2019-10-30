{-# LANGUAGE DeriveGeneric #-}
module Formula where

import Data.HashMap.Strict as Map
import Data.HashSet as Set
import Data.Hashable
import GHC.Generics (Generic)

data Formula 
    = Var String
    | Not Formula
    | And [Formula]
    | Or [Formula] deriving (Eq)

instance Show Formula where
    show (Var x)  = x
    show (Not f)  = "!(" ++ show f ++ ")"
    show (And fs) = "(" ++ show (head fs) ++ (foldl (\acc f -> acc ++ " & " ++ (show f)) "" fs) ++ ")"
    show (Or fs) = "(" ++ show (head fs) ++ (foldl (\acc f -> acc ++ " | " ++ (show f)) "" fs) ++ ")"

type Ctx = HashMap String Bool    

eval :: Ctx -> Formula -> Bool
eval ctx (Var x)  = lookupDefault False x ctx
eval ctx (Not f)  = not $ eval ctx f
eval ctx (And fs) = all (eval ctx) fs
eval ctx (Or fs)  = any (eval ctx) fs

data Literal = PosVar String | NegVar String deriving (Eq, Generic)
instance Hashable Literal
instance Show Literal where
    show (PosVar x) = x
    show (NegVar x) = '!' : x

type LiteralSet = HashSet Literal
type Cnf = HashSet LiteralSet
type NameGenHelper = Int

getNewVar :: NameGenHelper -> Formula
getNewVar x = Var $ 'p' : show x

getAllLiterals :: Formula -> HashSet Literal
getAllLiterals (Var x) = Set.singleton (PosVar x)
getAllLiterals (Not x) = 
    case x of
        Var y -> Set.singleton (NegVar y)
        _ -> getAllLiterals x
getAllLiterals (And fs) = foldl (\acc f -> Set.union acc (getAllLiterals f)) Set.empty fs
getAllLiterals (Or fs)  = foldl (\acc f -> Set.union acc (getAllLiterals f)) Set.empty fs

safelyAddNot :: Formula -> Formula
safelyAddNot (Not (Not x)) = safelyAddNot x
safelyAddNot (Not x) = x
safelyAddNot x = Not x

getNegated :: Literal -> Literal
getNegated (PosVar x) = NegVar x
getNegated (NegVar x) = PosVar x

