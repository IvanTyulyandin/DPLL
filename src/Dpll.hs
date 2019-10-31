module Dpll where

import Formula
import Data.Hashable
import Data.HashMap.Strict as Map
import Data.HashSet as Set

containsEmptyClause :: NameRepr a => LiteralSet a -> Bool
containsEmptyClause literalSet = result
    where
        (result, _) = Set.foldl' emptyClauseChecker (False, Set.empty) literalSet

        emptyClauseChecker :: NameRepr a =>
            (Bool, LiteralSet a) -> Literal a -> (Bool, LiteralSet a)
        emptyClauseChecker (metEmpty, acc) literal =
            if not metEmpty
            then (Set.member (getNegated literal) acc, Set.insert literal acc)
            else (metEmpty, Set.empty)

unitPropagate :: NameRepr a => (Cnf a, Ctx a) -> Literal a -> (Cnf a, Ctx a)
unitPropagate (cnf, ctx) l =
    let withoutTrueClauses = Prelude.filter (not . Set.member l) cnf
        newCnf = Prelude.map (Set.filter (/= getNegated l)) withoutTrueClauses
    in case l of
           PosVar x -> (newCnf, Map.insert x True ctx)
           NegVar x -> (newCnf, Map.insert x False ctx)

getPureLiterals :: NameRepr a => Cnf a -> LiteralSet a
getPureLiterals cnf = pureLiterals
    where
        allLiterals = Prelude.foldl Set.union Set.empty cnf
        pureLiterals = Set.foldl'
            (\acc l ->  if Set.member (getNegated l) acc
                        then Set.delete l $ Set.delete (getNegated l) acc
                        else acc) 
            allLiterals allLiterals

runDpll :: NameRepr a => Cnf a -> Maybe (Ctx a)
runDpll cnf = dpll cnf Map.empty

dpll :: NameRepr a => Cnf a -> Ctx a -> Maybe (Ctx a)
dpll cnf ctx
    | Prelude.null cnf               = Just ctx
    | containsEmptyClause setOfUnits = Nothing
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
        oneLiteralClauses = Prelude.filter ((==) 1 . Set.size) cnf
        setOfUnits = Prelude.foldl Set.union Set.empty oneLiteralClauses
