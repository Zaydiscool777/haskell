import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
-- psqueues: Data.IntPSQ, containers: Data.Map, Data.Set
-- https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode

{-
start: beginning node
goal: ending node
h(Nod n): heuristic distance of n from start. must be lesser or equal to actual distance.
pres: Map of nodes and their precedents
open: Unsearched nodes
fs: dict of f(Nod x): best guess of cost of using the node
gs: dict of g(Nod x): current known cost of using the node
-}

data Nod = Node {i :: Int, c :: [Nod]}
instance Eq Nod where Node ai ac == Node bi bc = ai == bi
instance Ord Nod where Node ai ac <= Node bi bc = ai <= bi
nNod = Node 0 []

start :: Nod = Node 1 [Node 2 [Node 4 [goal], goal]]
goal :: Nod = Node 5 []
h :: (Nod -> Int) = (5-)

-- retraces steps to return path
reconst pres cur = reconst' pres [cur] cur
reconst' :: (M.Map Int Nod) -> [Int] -> Int -> [Int]
reconst' pres path cur =
  case (pres M.!? cur) of
    Nothing -> path
    Just (Node pre _) -> reconst' pres (pre:path) pre

-- swap keys and values
invert x = M.fromList (map swap (M.toList x))

a_star = -- initialize loop
  a_star' M.empty S.empty (M.fromList [(start, 0)]) (M.fromList [(start, (h start))])

-- while-loop container
a_star' :: (M.Map Int Nod) -> (S.Set Nod) -> (M.Map Nod Int) -> (M.Map Nod Int) -> [Int]
a_star' pres open fs gs =
  if S.null open then [] -- failure
    else a_star_w pres open fs gs

-- while-loop body
a_star_w pres open fs gs =
  if (i cur) == (i goal) -- matches
    then
      reconst pres (i cur) -- retrace your steps
    else
      a_star' npres nopen nfs ngs
    where
      cur :: Nod = snd (M.findMin (invert fs))
      openwc = M.delete (i cur) open
      (npres, nopen, nfs, ngs) =
        foldr (a_star_f cur) (pres, openwc, fs, gs) (c cur)

-- for-loop body, uses foldr
a_star_f :: Nod -> (M.Map Int Nod, S.Set Nod, M.Map Nod Int, M.Map Nod Int) -> Nod
  -> (M.Map Int Nod, S.Set Nod, M.Map Nod Int, M.Map Nod Int)
a_star_f cur (pres, open, fs, gs) sofar =
  _

main = return 0
