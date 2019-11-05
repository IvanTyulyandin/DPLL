module Tseitin where

import Formula

{-# SPECIALISE cnfTseitin :: Formula Int -> Cnf Int #-}
cnfTseitin :: (NameRepr a) => Formula a -> Cnf a
cnfTseitin initF = cnf
    where
        (lastF, bindings, _) = cnfHelper initF [] (toEnum 0)
        cnf = foldl (\acc disjoint -> (getAllLiterals disjoint) : acc) [] (lastF : bindings)

        cnfHelper :: (NameRepr a) => Formula a -> [Formula a] -> a -> (Formula a, [Formula a], a)
        -- bad input defence
        cnfHelper (And []) _ _     = error "Tseitin: no args in And"
        cnfHelper (Or []) _ _      = error "Tseitin: no args in Or"
        cnfHelper (And (_:[])) _ _ = error "Tseitin: not enough args in And"
        cnfHelper (Or (_:[])) _ _  = error "Tseitin: not enough args in Or"
        -- good input processing
        cnfHelper (Var x) cs num = (Var x, cs, num)
        cnfHelper (Not x) cs num = (safelyAddNot l, cs', newNum)
            where (l, cs', newNum) = cnfHelper x cs num
        cnfHelper (And (f:fs)) cs num = (p, resCs, succ num')
            where 
                (l1, cs1, num1) = cnfHelper f cs num
                (innerMapping, cs', num') =
                    foldl (\(ls, clauses, numGen) frm ->
                            let (newName, newClauses, newNumGen) = cnfHelper frm clauses numGen
                            in (newName : ls, newClauses, newNumGen)
                          ) ([l1], cs1, num1) fs
                p = getNewVar num'
                oneOfClauses = Or $ p : Prelude.map (\synonym -> Not synonym) innerMapping
                resCs = (oneOfClauses : (Prelude.map (\synonym -> Or [(Not p), synonym]) innerMapping)) ++ cs'
        cnfHelper (Or (f:fs)) cs num = (p, resCs, succ num')
            where 
                (l1, cs1, num1) = cnfHelper f cs num
                (innerMapping, cs', num') =
                    foldl (\(ls, clauses, numGen) frm ->
                            let (newName, newClauses, newNumGen) = cnfHelper frm clauses numGen
                            in (newName : ls, newClauses, newNumGen)
                          ) ([l1], cs1, num1) fs
                p = getNewVar num'
                oneOfClauses = Or $ Not p : innerMapping
                resCs = (oneOfClauses : (Prelude.map (\synonym -> Or [p, Not synonym]) innerMapping)) ++ cs'
