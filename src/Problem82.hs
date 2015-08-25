module Problem82 where

import NNPGraphs
import Problem81

-- Not complete.  While not (as far as i can tell) explicitly stated
-- it may be that the intent is to allow crossing vertices multiple
-- times.  Ths solution cycle1 does not allow such cycles.

cycle :: (Eq a) => Graph a -> a -> [[a]]
cycle = cycle1

-- using Problem81 directly
-- We assume a graph, not a digraph (as in the problem statement)
-- The cycle returned will depend on the order of traversal
-- Note that this solution will not allow to cross the same vertex more than once
-- because of how paths works
cycle1 :: (Eq a) => Graph a -> a -> [[a]]
cycle1 g start = map (\x -> start:x)
            $ filter (\ x -> (length x) > 2)
            $ concatMap (\x -> paths1 g x start)
            $ neighbors g start
