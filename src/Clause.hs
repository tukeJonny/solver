module Clause where

import Literal

type Clause = [Lit]

-- 
tryAssign :: Lit -> [Clause] -> [Clause]
tryAssign lit clauses = let invLit = (invert lit)
                        in
                            map (excludeInvLit invLit) $ filter (\clause -> not (hasLit lit clause)) clauses

hasLit :: Lit -> Clause -> Bool
hasLit l clause = l `elem` clause

excludeInvLit :: Lit -> Clause -> Clause
excludeInvLit il clause = filter (\lit -> lit /= il) clause
-- 
collectPureLits :: [Clause] -> [Lit]
collectPureLits clauses = let lits = clauses >>= id
                         in
                            filter (\lit -> not ((invert lit) `elem` lits)) lits


