-- import qualified Data.Map as M
-- import qualified Data.Set as S -- -package containers
-- import Data.Tuple (swap)
-- import Debug.Trace
-- import Data.Maybe
-- -- psqueues: Data.IntPSQ, containers: Data.Map, Data.Set
-- -- https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode

-- {-
-- start: beginning node
-- goal: ending node
-- h(Nod n): heuristic distance of n from start. must be lesser or equal to actual distance.
-- pres: Map of nodes and their precedents
-- open: Unsearched nodes
-- fs: dict of f(Nod x): best guess of cost of using the node
-- gs: dict of g(Nod x): current known cost of using the node
-- -}

-- data Nod = Node {i :: Int, c :: [Nod]}
-- instance Eq Nod where Node ai ac == Node bi bc = ai == bi
-- instance Ord Nod where Node ai ac <= Node bi bc = ai <= bi
-- nNod = Node 0 []
-- instance Show Nod where show (Node ai ac) = show ai ++ ": " ++ show ac

-- start :: Nod = Node 1 [Node 2 [Node 4 [goal], goal]]
-- goal :: Nod = Node 5 []
-- h :: (Nod -> Int) = (5-) . i
-- d _ _ = 1 -- weight of a distance from one node to another

-- -- retraces steps to return path
-- reconst :: M.Map Int Nod -> Int -> [Int]
-- reconst pres cur = reconst' pres [cur] cur
-- reconst' :: M.Map Int Nod -> [Int] -> Int -> [Int]
-- reconst' pres path cur = traceShowId (
--   case pres M.!? cur of
--     Nothing -> path
--     Just (Node pre _) -> reconst' pres (pre:path) pre)

-- -- swap keys and values
-- invert :: Ord k => M.Map a k -> M.Map k a
-- invert x = M.fromList (map swap (M.toList x))

-- aStar :: [Int]
-- aStar = -- initialize loop
--   traceShowId ( aStar' M.empty (S.singleton start) (M.fromList [(start, 0)]) (M.fromList [(start, h start)]) )

-- -- while-loop container
-- aStar' :: M.Map Int Nod -> S.Set Nod -> M.Map Nod Int -> M.Map Nod Int -> [Int]
-- aStar' pres open fs gs = traceShowId (
--   if S.null open then [] -- failure
--     else aStarW pres open fs gs)

-- -- while-loop body
-- aStarW :: M.Map Int Nod -> S.Set Nod -> M.Map Nod Int -> M.Map Nod Int -> [Int]
-- aStarW pres open fs gs = traceShowId (
--   if i cur == i goal -- matches
--     then
--       reconst pres (i cur) -- retrace your steps
--     else
--       aStar' npres nopen nfs ngs )
--     where
--       cur :: Nod = snd (M.findMin (invert fs))
--       openwc = S.delete cur open
--       (npres, nopen, nfs, ngs) =
--         foldr (aStarF cur) (pres, openwc, fs, gs) (c cur)

-- -- for-loop body, uses foldr
-- aStarF :: Nod -> -- Nod -> Folder Nod State?
--   Nod -> (M.Map Int Nod, S.Set Nod, M.Map Nod Int, M.Map Nod Int)
--   -> (M.Map Int Nod, S.Set Nod, M.Map Nod Int, M.Map Nod Int)
-- aStarF cur  nei (pres, open, fs, gs) =
--   if tentg < maybenei
--     then
--       (
--         M.insert (i nei) cur pres,
--         S.insert nei open,
--         M.insert nei tentg gs,
--         M.insert nei (tentg + h nei) fs
--       )
--     else
--       (pres, open, fs, gs)
--     where
--       tentg = traceShowId maybecur + d cur nei
--       maybecur = fromMaybe maxBound (gs M.!? cur)
--       maybenei = fromMaybe maxBound (gs M.!? nei)

-- main = return ()
