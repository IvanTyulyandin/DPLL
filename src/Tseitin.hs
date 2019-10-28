module Tseitin where

import Formula
import Data.HashSet as Set

cnfTseitin :: Formula -> Cnf
cnfTseitin f = cnf
    where 
        (last, bindings, _) = cnfHelper f [] 1
        cnf = foldl (\set disjoint -> Set.insert (getAllLiterals disjoint) set) Set.empty (last : bindings)
    
        cnfHelper :: Formula -> [Formula]-> NameGenHelper -> (Formula, [Formula], NameGenHelper)
        cnfHelper (Var x) fs num = (Var x, fs, num)
        cnfHelper (Not x) fs num = (safelyAddNot l, fs', newNum)
            where (l, fs', newNum) = cnfHelper x fs num
        cnfHelper (And e1 e2) fs num = (p, fs', num2 + 1)
            where 
                (l1, fs1, num1) = cnfHelper e1 fs num
                (l2, fs2, num2) = cnfHelper e2 fs1 num1
                p = getNewVar num2
                fs' = Or (Not p) l1 
                    : Or (Not p) l2 
                    : Or (Or (safelyAddNot l1) (safelyAddNot l2)) p 
                    : fs2
        cnfHelper (Or e1 e2) fs num = (p, fs', num2 + 1)
            where 
                (l1, fs1, num1) = cnfHelper e1 fs num
                (l2, fs2, num2) = cnfHelper e2 fs1 num1
                p = getNewVar num2
                fs' = Or (safelyAddNot l1) p 
                    : Or (safelyAddNot l2) p
                    : Or (Or l1 l2) (Not p)
                    : fs2