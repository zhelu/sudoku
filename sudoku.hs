import Data.List
import Data.List.Split
import Data.Maybe

data Cell = Fixed Int | Candidate Int | Empty deriving (Show)

size = 3
sizeSquared = size * size

hasNum :: Cell -> Bool
hasNum Empty = False
hasNum _ = True

makeCell :: Int -> Cell
makeCell 0 = Empty
makeCell x = Fixed x

makeGrid :: [[Int]] -> Grid
makeGrid = map (map makeCell)

extract :: Cell -> Int
extract (Fixed x) = x
extract (Candidate x) = x
extract Empty = undefined

type Grid = [[Cell]]

validateInput :: [[a]] -> Bool
validateInput x = (length x == sizeSquared) && (all ((==) sizeSquared) $ map length x) 

prettify :: [[Int]] -> String
prettify x = intercalate "\n" $ map (unwords . map show) x

solve :: Grid -> String

solveHelper :: (Int, Int) -> Grid -> Maybe Grid
solve g = prettify $ map (map extract) $ fromJust $ solveHelper (0, 0) g

solveHelper (x, y) g 
 | x == sizeSquared = Just [[]]
 | y == sizeSquared = solveHelper (x + 1, 0) g
 | otherwise =
  let c = checkGrid g
      d = gridDone g
  in case (c, d) of (True, True) -> Just g
                    (True, False) ->
                      let cell = g !! x !! y
                      in case cell of Fixed _ -> solveHelper (x, y + 1) g
                                      Candidate _ ->
                                        let n = solveHelper (x, y + 1) g
                                        in case n of Nothing ->
                                                        let g' = makeNewGrid (x, y) g
                                                        in case g' of Nothing -> Nothing
                                                                      Just g'' -> solveHelper (x, y) g''
                                                     n' -> n'
                                      Empty -> solveHelper (x, y) (fromJust $ makeNewGrid (x, y) g)
                    (False, _) ->
                      let g' = makeNewGrid (x, y) g
                      in case g' of Nothing -> Nothing
                                    Just g'' -> solveHelper (x, y) g''

gridDone :: Grid -> Bool

gridDone = all $ all hasNum

checkGrid :: Grid -> Bool

checkGrid x = all ((==) True) $ map (\t -> checkSubgrid x t) allSubgrids

checkSubgrid :: Grid -> [(Int, Int)] -> Bool

checkSubgrid g sg =
  let x = map extract $
          filter hasNum $
          map (\(a, b)-> g !! a !! b) sg
  in (length x) == (length $ nub x)

allSubgrids =
  [zip (take sizeSquared $ repeat x) (take sizeSquared [0 ..]) | x <- [0..(sizeSquared - 1)]] ++
  [zip (take sizeSquared [0 ..]) (take sizeSquared $ repeat x) | x <- [0..(sizeSquared - 1)]] ++
  [[ (i, j) | i <- [x..(x + size - 1)], j <- [y..(y + size - 1)]] | x <- map ((*) size) [0..(size - 1)], y <- map ((*) size) [0..(size - 1)]]

makeNewGrid :: (Int, Int) -> Grid -> Maybe Grid
makeNewGrid (x, y) g =
  let h = take x g
      m = g !! x
      t = drop (x + 1) g
  in let h' = take y m
         m' = m !! y
         t' = drop (y + 1) m
     in case m' of Empty -> Just $ h ++ [ h' ++ [Candidate 1] ++ t'] ++ t
                   Candidate x -> 
                     if x == sizeSquared then Nothing else Just $ h ++ [ h' ++ [Candidate (x + 1)] ++ t'] ++ t
                   _ -> undefined

main = do
  contents <- getContents
  let grid = map (map read) $ map (chunksOf 1) $ lines contents :: [[Int]]
  if (validateInput grid) then putStr $ solve $ makeGrid grid else putStr "Not valid" 