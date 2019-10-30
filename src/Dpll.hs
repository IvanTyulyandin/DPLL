module Dpll where

import Formula
import Data.HashMap.Strict as Map
import Data.HashSet as Set

isContainsEmptyDisjoint :: LiteralSet -> Bool
isContainsEmptyDisjoint literalSet = result
    where
        (result, _) = Set.foldl' emptyDisjointChecker (False, Set.empty) literalSet

        emptyDisjointChecker :: (Bool, LiteralSet) -> Literal -> (Bool, LiteralSet)
        emptyDisjointChecker (curRes, acc) literal = 
            if not curRes
            then (Set.member (getNegated literal) acc, Set.insert literal acc)
            else (curRes, Set.empty)

unitPropagate :: (Cnf, Ctx) -> Literal -> (Cnf, Ctx)
unitPropagate (cnf, ctx) l = 
    let withoutTrueDisjoints = Prelude.filter (not . Set.member l) cnf
        newCnf = Prelude.map (Set.filter (/= getNegated l)) withoutTrueDisjoints
    in case l of
           PosVar x -> (newCnf, Map.insert x True ctx) 
           NegVar x -> (newCnf, Map.insert x False ctx) 

getPureLiterals :: Cnf -> LiteralSet
getPureLiterals cnf = pure
    where
        allLiterals = Prelude.foldl Set.union Set.empty cnf
        pure = Set.foldl' 
            (\acc l ->  if Set.member (getNegated l) acc
                        then Set.delete l $ Set.delete (getNegated l) acc
                        else acc) 
            allLiterals allLiterals

runDpll :: Cnf -> Maybe Ctx
runDpll cnf = dpll cnf Map.empty

dpll :: Cnf -> Ctx -> Maybe Ctx
dpll cnf ctx
    | Prelude.null cnf                   = Just ctx
    | isContainsEmptyDisjoint setOfUnits = Nothing
    | otherwise =
        let (cnf1, ctx1) = Set.foldl' unitPropagate (cnf, ctx) setOfUnits
            (cnf2, ctx2) = Set.foldl' unitPropagate (cnf1, ctx1) (getPureLiterals cnf1)
            cnf3 = Prelude.filter (not . Set.null) cnf2
        in if Prelude.null cnf3
           then Just ctx2
           else let newLiteral = head $ Set.toList $ head cnf3
                    newToCnf = (Set.singleton newLiteral) : cnf3
                    negNewToCnf = (Set.singleton (getNegated newLiteral)) : cnf3
                in case dpll newToCnf ctx2 of
                        Just model -> Just model
                        Nothing    -> dpll negNewToCnf ctx2
    where
        oneLiteralDisjoints = Prelude.filter ((==) 1 . Set.size) cnf
        setOfUnits = Prelude.foldl Set.union Set.empty oneLiteralDisjoints
