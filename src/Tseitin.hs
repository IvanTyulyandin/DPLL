module Tseitin where

import Formula
import Data.HashSet as Set

cnfTseitin :: Formula -> Cnf
cnfTseitin f = cnf
    where 
        (last, bindings, _) = cnfHelper f [] 1
        cnf = foldl (\set disjoint -> Set.insert (getAllLiterals disjoint) set) Set.empty (last : bindings)
    
        cnfHelper :: Formula -> [Formula] -> NameGenHelper -> (Formula, [Formula], NameGenHelper)
        cnfHelper (Var x) ds num = (Var x, ds, num)
        cnfHelper (Not x) ds num = (safelyAddNot l, ds', newNum)
            where (l, ds', newNum) = cnfHelper x ds num
        cnfHelper (And (f:fs)) ds num = (p, resDs, num' + 1)
            where 
                (l1, ds1, num1) = cnfHelper f ds num
                (innerMapping, ds', num') = 
                    foldl (\(ls, disjs, numGen) frm -> 
                            let (newName, newDs, newNumGen) = cnfHelper frm disjs numGen
                            in (newName : ls, newDs, newNumGen)
                          ) ([l1], ds1, num1) fs
                p = getNewVar num'
                lastDisj = Or $ p : Prelude.map (\name -> Not name) innerMapping
                resDs = lastDisj : (Prelude.map (\name -> Or [(Not p), name]) innerMapping ++ ds')
        cnfHelper (Or (f:fs)) ds num = (p, resDs, num' + 1)
            where 
                (l1, ds1, num1) = cnfHelper f ds num
                (innerMapping, ds', num') = 
                    foldl (\(ls, disjs, numGen) frm -> 
                            let (newName, newDs, newNumGen) = cnfHelper frm disjs numGen
                            in (newName : ls, newDs, newNumGen)
                          ) ([l1], ds1, num1) fs
                p = getNewVar num'
                lastDisj = Or $ Not p : innerMapping
                resDs = lastDisj : (Prelude.map (\name -> Or [p, Not name]) innerMapping ++ ds')