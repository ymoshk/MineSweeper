
-- Exports
module Minesweeper (
    Action(..),
    GameState(..),
    Board,    
    initializeBoard,
    printBoard,
    playerAction,
    gameStep
) where
    
-- Imports
import System.Random
import Data.List


-- New data types
data Content = Mine | Clear Int deriving(Eq)
data Status = Hidden | Revealed | Flagged deriving(Show, Eq)
data Action = Dig Int Int | Flag Int Int deriving(Read)
data GameState = Won Board | Lost Board | GameOn Board deriving(Show,Eq)

type Cell = (Status, Content)
type Board = [[Cell]]


-- Implementation for showing cell content
instance Show Content where
    show Mine = "[*] "
    show (Clear k) = "[" ++ show k ++ "] "


{-----------------------------------------------------------------------------}
{------------------------- Board Update Functions-----------------------------}

-- Set new status of square in board
-- If trying to flag an already flagged cell -> set to hidden
-- If trying to flag a revealed cell -> do nothing
-- If trying to dig a flagged cell -> do nothing
-- Otherwise set status to new status
setStatus :: Status -> Cell -> Cell
setStatus newStatus tup
    | newStatus == Flagged && status == Flagged = (Hidden, content)
    | newStatus == Flagged && status == Revealed = (status, content)
    | newStatus == Revealed && status == Flagged = (status, content)
    | otherwise = (newStatus, content)
    where content = snd tup
          status = fst tup 


-- Update cell in board by given location and status
-- Only changes cell's status
updateBoardByAction :: Int -> Int -> Status -> Board -> Board
updateBoardByAction _ _ _ [] = []
updateBoardByAction i j newStatus rows = left ++ ([updateRowByACtion j newStatus (rows !! (i-1))]) ++ right
    where left = init (fst (splitAt i rows))
          right = snd (splitAt i rows)


-- Update cell in row by given location and status
-- Only changes cell's status
updateRowByACtion :: Int -> Status -> [Cell] -> [Cell]
updateRowByACtion index newStatus row = left ++ [finalCell] ++ right
    where left = init (fst (splitAt index row)) -- List left side split
          right = snd (splitAt index row) -- List right side split
          curCell = last (fst (splitAt index row)) -- Get current cell
          finalCell = setStatus newStatus curCell -- New Cell


-- Update cell in board by given new cell
-- Used in order to replace default cells when initializing new board
updateCellInBoard :: Int -> Int -> Cell -> Board -> Board
updateCellInBoard _ _ _ [] = []
updateCellInBoard row col (newStatus, newCell) board = left ++ [updateCellInRow col (newStatus ,newCell) (board !! (row-1))] ++ right
    where left = init (fst (splitAt row board))
          right = snd (splitAt row board)


-- Update cell in row by given new cell
-- Used in order to replace default cells when initializing new board
updateCellInRow :: Int -> Cell -> [Cell] -> [Cell]
updateCellInRow index (newStatus, newCell) row = left ++ [(newStatus, newCell)] ++ right
    where left = init (fst (splitAt index row))
          right = snd (splitAt index row)


{--------------------------------------------------------------------------------}
{------------------------- Mine Generation\Insertion-----------------------------}

-- Generate location for bombs as list of (x,y) coordinates
generateMines :: Int -> Int -> Int -> StdGen -> [(Int, Int)]
generateMines 0 _ _ _ = []
generateMines count h w generator = (xCoord, yCoord) : generateMines (count - 1) h w (snd generated')
    where generated =  randomR (1, h) generator -- Pass 1-h range to random given random generator
          generated' = randomR (1, w) (snd generated) -- Pass 1-w range to random generator returned
          xCoord = fst generated
          yCoord = fst generated'


-- Sort the mine list in an ascending order
sortMineList :: [(Int, Int)] -> [(Int, Int)]
sortMineList list = sortBy compareCoords list

-- Comparator for sorting the mine list
compareCoords :: (Int, Int) -> (Int, Int) -> Ordering
compareCoords (x1, y1) (x2, y2)
    | x1 < x2 = LT
    | x1 == x2 && y1 < y2 = LT
    | x1 == x2 && y1 >= y2 = GT
    | x1 > x2 = GT
    | otherwise = EQ


-- Push mine to board by its coordinate
pushMineByLocation :: (Int,Int) -> Board -> Board
pushMineByLocation (x,y) board = updateCellInBoard x y (Hidden, Mine) board

-- Push a list of generated mines to the board
pushMines :: [(Int, Int)] -> Board -> Board
pushMines [] board = board -- Base case 1 - mine list is empty
pushMines [x] board = pushMineByLocation x board -- Base case 2 - Mine list only has one element
pushMines (x:xs) board = pushMineByLocation x $ (pushMines xs board)

{------------------------------------------------------------------------}
{------------------------- Display Functions-----------------------------}


revealMine :: Cell -> Cell
revealMine (_, Mine) = (Revealed, Mine)
revealMine tup = tup

revealCell :: Cell -> Cell
revealCell (status, cell) = (Revealed, cell)


-- Reveal all cells in board
revealAll :: Board -> Board
revealAll board = map (map revealCell) $ board

revealAllMines :: Board -> Board
revealAllMines board = map (map revealMine) $ board

-- Print cell as string
printCell :: Cell -> String
printCell (status, content)
    | status == Revealed = show content
    | status == Flagged = "[!] "
    | otherwise = "[ ] "


-- Print a row in board
printRow :: [Cell] -> String
printRow [] = ""
printRow (x:xs) = printCell x ++ printRow xs


-- Display Board as string
printBoard :: Board -> Int -> Int ->  String
printBoard board rows cols = addColumnCount cols ++ ('\n' : addRowCount 1 toString)
    where toString = unlines $ (map printRow board)

{------------------------------------------------------------------------}
{------------------------- Accessory Display Functions-----------------------------}

-- Add column numbering to board display
addColumnCount :: Int -> String
addColumnCount 0 = []
addColumnCount n = addColumnCount (n-1) ++ numToString n


-- Add row numbering to board display
addRowCount :: Int -> String -> String
addRowCount n board
    | board == [] = board
    | (head board) == '\n' = numToString n ++ "\n" ++ (addRowCount (n+1) $ tail board)
    | otherwise = head board : (addRowCount n $ tail board)


-- Format number as string with leading zeroes, for example 1 will be printed as "001"
-- Used in row\column numbering
numToString :: Int -> String
numToString n
    | n == 0 = []
    | n < 10 = "00" ++ show n ++ " "
    | otherwise = "0" ++ show n ++ " "


{-------------------------------------------------------------------------------------}
{------------------------- Board Initialization Functions-----------------------------}

-- Initialize board by given height, width and mine count
-- Also receives a random generator
initializeBoard :: Int -> Int -> Int -> StdGen -> Board
initializeBoard height width count rand =
    let emptyBoard = replicate height $ replicate width (Hidden, Clear 0) -- Generate empty board
        minedBoard = pushMines (generateMines count height width rand) $ emptyBoard -- Generate random mine locations and push to board
        finalBoard = detectMinesInBoard 0 0 height width minedBoard -- Update mine perimeter count
    in finalBoard
          

-- Detect mine count in a cell's perimeter
detectMines :: Int -> Int -> Int -> Int -> Board -> Board
detectMines i j heightBound widthBound board 
    | curCell /= Mine = updateCellInBoard (i+1) (j+1) (Hidden, Clear sumPerimeter) board -- Executed only when cell is Clear
    | otherwise = board
    where upper = isMine (i-1) j  heightBound widthBound $ snd (board !! (i-1) !! j)
          upperRight = isMine (i-1) (j+1)  heightBound widthBound $ snd (board !! (i-1) !! (j+1))
          right = isMine i (j+1) heightBound widthBound $ snd ((board !! i) !! (j+1))
          lowerRight = isMine (i+1) (j+1) heightBound widthBound $ snd (board !! (i+1) !! (j+1))
          lower = isMine (i+1) j heightBound widthBound $ snd (board !! (i+1) !! j)
          lowerLeft = isMine (i+1) (j-1) heightBound widthBound $ snd ((board !! (i+1)) !! (j-1))
          left = isMine i (j-1) heightBound widthBound $ snd ((board !! i) !! (j-1))
          upperLeft = isMine (i-1) (j-1) heightBound widthBound $ snd ((board !! (i-1)) !! (j-1))
          sumPerimeter = upper + upperRight + right + lowerRight + lower + lowerLeft + left + upperLeft -- Sum mine count
          curCell = snd ((board !! i) !! j) -- Get Cell in (i, j)


-- Recursively detect mines for all cells in a row
detectMinesInRow :: Int -> Int -> Int -> Int -> Board -> Board
detectMinesInRow i j heightBound widthBound board
    | j >= widthBound = board -- Base case - column index exceeds width bound
    | otherwise = detectMines i j heightBound widthBound (detectMinesInRow i (j+1) heightBound widthBound board)


-- Recursively detect mines for all cells in board by row
detectMinesInBoard :: Int -> Int -> Int -> Int -> Board -> Board
detectMinesInBoard i j heightBound widthBound board
    | i >= heightBound = board -- Base case - row index exceeds height bound
    | otherwise = detectMinesInRow i j heightBound widthBound (detectMinesInBoard (i+1) j heightBound widthBound board)


-- Check if given cell holds a mine, return 1 if so
-- Else return 0
isMine :: Int -> Int -> Int -> Int -> Content -> Int
isMine row col heightBound widthBound cell
    | row < 0 || col < 0 = 0 -- Indexes are out of bounds
    | row >= heightBound || col >= widthBound = 0 -- Indexes are out of bounds
    | cell == Mine = 1 
    | otherwise = 0


{------------------------------------------------------------------------------}
{------------------------- Action & Game Functions-----------------------------}


-- Clear out cells after digging a spot
clearOut :: Int -> Int -> Int -> Int -> Board -> Board
clearOut row col heightBound widthBound board
    | row <= 0 || col <= 0 || row > heightBound || col > widthBound = board -- Return board if parameters exceed board's bounds
    | getCount curCell /= 0 = updateBoardByAction row col Revealed board -- 
    | getCount curCell == 0 && curStatus /= Revealed = revealedUpperLeft
    | otherwise = board
    where curCell = snd (board !! (row-1) !! (col-1))
          curStatus = fst (board !! (row-1) !! (col-1))
          updatedBoard = updateBoardByAction row col Revealed board
          revealedUpper = clearOut (row-1) col heightBound widthBound updatedBoard
          revealedUpperRight = clearOut (row-1) (col+1) heightBound widthBound revealedUpper
          revealedRight = clearOut row (col+1) heightBound widthBound revealedUpperRight
          revealedLowerRight = clearOut (row+1) (col+1) heightBound widthBound revealedRight
          revealedLower = clearOut (row+1) col heightBound widthBound revealedLowerRight
          revealedLowerLeft = clearOut (row+1) (col-1) heightBound widthBound revealedLower
          revealedLeft = clearOut row (col-1) heightBound widthBound revealedLowerLeft
          revealedUpperLeft = clearOut (row-1) (col-1) heightBound widthBound revealedLeft


-- Convert cell content to number
-- Mine is defined as -1, Clear is defined as mine count in perimeter
getCount :: Content -> Int
getCount (Clear x) = x
getCount Mine = -1


-- Check if given mine is blown (defined as revealed)
isMineBlown :: Bool -> (Status, Content) -> Bool
isMineBlown val (st, cl)
    | st == Revealed && cl == Mine = True
    | val == True = True
    | otherwise = False

-- Check if any mines in a row were set off
isMineBlownInRow :: [(Status, Content)] -> Bool
isMineBlownInRow [] = False
isMineBlownInRow row = foldl isMineBlown False row 


-- Game is over if dug a mine
isGameOver :: Board -> Bool
isGameOver [] = False
isGameOver (x:xs) = isMineBlownInRow x || isGameOver xs


-- Functions for checking if all clear cells were exposed 
isCellExposed :: Bool -> (Status, Content) -> Bool
isCellExposed val (st, cl)
    | st == Revealed && cl /= Mine = True && val
    | cl == Mine = val
    | otherwise = False

-- Check whether the row has a cell which is revealed
isRowRevealed :: [(Status, Content)] -> Bool
isRowRevealed [] = True
isRowRevealed (x:xs) = foldl isCellExposed True xs

-- Game is won if all clear cells were revealed
isGameWon :: Board -> Bool
isGameWon [] = True
isGameWon (x:xs) = isRowRevealed x && isGameWon xs


-- Execute round in game
gameStep :: Action -> Int -> Int -> Board -> GameState
gameStep action heightBound widthBound board
    | isGameWon afterActionBoard = (Won (revealAll afterActionBoard)) -- Check if game is won
    | isGameOver afterActionBoard = (Lost (revealAllMines afterActionBoard)) -- Check if game is lost
    | otherwise = (GameOn afterActionBoard) -- Otherwise game continues as before
    where afterActionBoard = playerAction action heightBound widthBound board -- Perform given action on board


-- Perform action given by player
-- Action can be either dig or flag
playerAction :: Action -> Int -> Int -> Board -> Board
playerAction (Dig i j) heightBound widthBound board = clearOut i j heightBound widthBound board
playerAction (Flag i j) heightBound widthBound board = updateBoardByAction i j Flagged board
