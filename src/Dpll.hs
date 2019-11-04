module Dpll where

import Formula
import Data.HashMap.Strict as Map
import Data.HashSet as Set

containsEmptyClause :: (NameRepr a) => [Literal a] -> Bool
containsEmptyClause literalSet = result
    where
        (result, _) = Prelude.foldl emptyClauseChecker (False, Set.empty) literalSet

        emptyClauseChecker :: NameRepr a =>
            (Bool, LiteralSet a) -> Literal a -> (Bool, LiteralSet a)
        emptyClauseChecker (metEmpty, acc) literal =
            if not metEmpty
            then (Set.member (getNegated literal) acc, Set.insert literal acc)
            else (metEmpty, Set.empty)

getPureLiterals :: (NameRepr a) => Cnf a -> [Literal a]
getPureLiterals cnf = Set.toList pureLiterals
    where
        allLiterals = Prelude.foldl Set.union Set.empty cnf
        pureLiterals = Set.foldl'
            (\acc l ->  if Set.member (getNegated l) acc
                        then Set.delete l $ Set.delete (getNegated l) acc
                        else acc) 
            allLiterals allLiterals

getUnits :: (NameRepr a) => Cnf a -> [Literal a]
getUnits cnf =
    let oneLiteralSet = Prelude.filter ((==) 1 . Set.size) cnf
    in Prelude.foldl (\acc clause -> (Set.toList clause) ++ acc) [] oneLiteralSet

getLiteralsToPropagate :: (NameRepr a) => Cnf a -> [Literal a]
getLiteralsToPropagate cnf =
    let units = getUnits cnf
    in (Prelude.filter (\lit -> notElem lit units) $ getPureLiterals cnf) ++ units

type DpllConf a = (Cnf a, Ctx a, [Literal a])

propagateNext :: (NameRepr a) => DpllConf a -> DpllConf a
propagateNext (_, _, []) = error "Nothing to propagate"
propagateNext (cnf, ctx, l:ls) =
    let withoutTrueClauses = Prelude.filter (not . Set.member l) cnf
        newCnf = Prelude.map (Set.filter (/= getNegated l)) withoutTrueClauses
        -- seems not optimal to find out newToPropagate
        -- since most of (getLiteralsToPropagate newCnf) already in ls
        newToPropagate = Prelude.filter (\lit -> notElem lit ls) $ getLiteralsToPropagate newCnf
    in case l of
        PosVar x -> (newCnf, Map.insert x True ctx, newToPropagate ++ ls)
        NegVar x -> (newCnf, Map.insert x False ctx, newToPropagate ++ ls)

runDpll :: (NameRepr a) => Cnf a -> Maybe (Ctx a)
runDpll cnf = dpll (cnf, Map.empty, getLiteralsToPropagate cnf)

dpll :: (NameRepr a) => DpllConf a -> Maybe (Ctx a)
dpll (cnf, ctx, []) =
    let cnf1 = Prelude.filter (not . Set.null) cnf
    in  if Prelude.null cnf1
        then Just ctx
        else
            let newLiteral = head $ Set.toList $ head cnf1
            in case dpll (cnf1, ctx, [newLiteral]) of
                Nothing    -> dpll (cnf1, ctx, [getNegated newLiteral])
                Just model -> Just model
dpll (cnf, ctx, ll)
    | containsEmptyClause ll = Nothing
    | otherwise =
        if Prelude.null cnf
        then Just ctx
        else dpll $ propagateNext (cnf, ctx, ll)
