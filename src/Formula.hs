{-# LANGUAGE DeriveGeneric #-}
module Formula where

import Data.HashMap.Strict as Map
import Data.HashSet as Set
import Data.Hashable
import GHC.Generics (Generic)

data Formula 
    = Var String
    | Not Formula
    | And Formula Formula
    | Or Formula Formula deriving (Eq)

instance Show Formula where
    show (Var x) = x
    show (Not x) = "!(" ++ show x ++ ")"
    show (And e1 e2) = "(" ++ show e1  ++ " & " ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1  ++ " | " ++ show e2 ++ ")"

type Ctx = HashMap String Bool    

eval :: Formula -> Ctx -> Bool
eval (Var x) ctx     = lookupDefault False x ctx
eval (Not e) ctx     = not $ eval e ctx
eval (And e1 e2) ctx = eval e1 ctx && eval e2 ctx
eval (Or e1 e2) ctx  = eval e1 ctx || eval e2 ctx

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
getAllLiterals (And e1 e2) = Set.union (getAllLiterals e1) (getAllLiterals e2)
getAllLiterals (Or e1 e2) = Set.union (getAllLiterals e1) (getAllLiterals e2)

safelyAddNot :: Formula -> Formula
safelyAddNot (Not (Not x)) = safelyAddNot x
safelyAddNot (Not x) = x
safelyAddNot x = Not x

getNegated :: Literal -> Literal
getNegated (PosVar x) = NegVar x
getNegated (NegVar x) = PosVar x

