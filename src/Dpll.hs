module Dpll where

import Formula
import Data.HashMap.Lazy as Map
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
    let withoutTrueDisjoints = Set.filter (not . Set.member l) cnf
        newCnf = Set.map (Set.filter (/= getNegated l)) withoutTrueDisjoints
    in case l of
           PosVar x -> (newCnf, Map.insert x True ctx) 
           NegVar x -> (newCnf, Map.insert x False ctx) 

getPureLiterals :: Cnf -> LiteralSet
getPureLiterals cnf = pure
    where
        allLiterals = Set.foldl' Set.union Set.empty cnf
        pure = Set.foldl' 
            (\acc l ->  if Set.member (getNegated l) acc
                        then Set.delete l $ Set.delete (getNegated l) acc
                        else acc) 
            allLiterals allLiterals

runDpll :: Cnf -> Maybe Ctx
runDpll cnf = dpll cnf Map.empty

dpll :: Cnf -> Ctx -> Maybe Ctx
dpll cnf ctx = 
    let oneLiteralDisjoints = Set.filter (\d -> (Set.size d) == 1) cnf 
        setOfUnits = Set.foldl' (\acc dis -> Set.union acc dis) Set.empty oneLiteralDisjoints 
    in if Set.null cnf
       then Just ctx 
       else if isContainsEmptyDisjoint setOfUnits
            then Nothing
            else let (cnf1, ctx1) = Set.foldl' unitPropagate (cnf, ctx) setOfUnits 
                     (cnf2, ctx2) = Set.foldl' unitPropagate (cnf1, ctx1) (getPureLiterals cnf1)
                     cnf3 = Set.filter (not . Set.null) cnf2
                 in if Set.null cnf3
                    then Just ctx2
                    else let
                            -- take "first" literal in "first" disjoint
                            newLiteral = head $ Set.toList $ head $ Set.toList cnf3
                            newToCnf = Set.insert (Set.singleton newLiteral) cnf3 
                            -- ctx2 will be updated during unit propagation at next iteration
                            tryNewLiteral = dpll newToCnf ctx2
                            negNewToCnf = Set.insert (Set.singleton (getNegated newLiteral)) cnf3
                        in if tryNewLiteral /= Nothing
                           then tryNewLiteral
                           else dpll negNewToCnf ctx2
                            