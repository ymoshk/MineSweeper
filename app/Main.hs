module Main where
  
import Minesweeper
import System.Environment  
import System.IO  
import System.IO.Error
import Safe (readMay)
import Data.Either
import System.Exit
import System.Random

-- Verify mine count entered by user
verifyMineCount :: Int -> Int -> Int -> Bool
verifyMineCount count w h = (count >= 4) && (count <= 199) && (count <= (w * h - 1))


-- Verify height and with entered by user
verifyHeightWidth :: Int -> Bool
verifyHeightWidth i = (i >= 10) && (i <= 20)


-- Validate the command line args received from the user.
parseArgs :: IO ()
parseArgs = do

  [h,w,b] <- getArgs
  let resW = verifyHeightWidth (read w)
  let resH = verifyHeightWidth (read h)
  let resB = verifyMineCount (read b) (read w) (read h)

  if (resW /= True) then error "Invalid width value"
  else if (resH /= True) then error "Invalid height value"
  else if (resB /= True) then error "Invalid bombs number value"
  else return ()


-- Exception handler, used by the main function when args are invalid.
handler :: IOError -> IO ()
handler e
  | isUserError e = error "You must enter exactly 3 arguments: board width, board height and number of bombs"
  | otherwise = ioError e


main :: IO ()
main = do 
  parseArgs `catchIOError` handler
  [h,w,b] <- getArgs
  g <- newStdGen
  let board = initializeBoard (read w) (read h) (read b) g
  gameLoop (read w) (read h) board


-- The main game loop. Continuous until the player quit/win/lose.
gameLoop :: Int -> Int -> Board -> IO ()
gameLoop h w board = do
  putStrLn (printBoard board h w)
  putStrLn "What is your next move?"
  action <- readAction
  case action of
    Nothing -> do -- Read action returns nothing if an invalid action string received from the user.
      putStrLn "Invalid move input, please try again."
      (gameLoop h w board) -- Continue the loop without doing anything.
    (Just act) -> do
      let verifyCord = (areCordValid h w act)
      case verifyCord of
        False -> do -- Step coordinates aren't valid.
          putStrLn "Invalid coordinates, please try again."
          (gameLoop h w board) -- Continue the loop without doing anything.
        True -> handleGameState (gameStep act h w board) h w


-- Check the status of a game state. 
-- If the game state is Won/Lost - print a relevant message and quit.
-- If the game state is GameOn - contioue the game loop.
handleGameState :: GameState -> Int -> Int -> IO ()
handleGameState (Lost board) h w = do
  putStrLn (printBoard board h w)
  putStrLn "You Lost"
handleGameState (Won board) h w = do
  putStrLn (printBoard board h w)
  putStrLn "You Won"
handleGameState (GameOn board) h w = do
  gameLoop h w board


-- Read and handle an action received by the user.
-- Return Nothing if 'readMay' couldn't parse an Action type from the input.
-- If the input is 'Quit' the game will be closed.
readAction :: IO (Maybe Action)
readAction = do
  action <- getLine
  case action of
    "Quit" -> do
      putStrLn "The game will be closed now."
      exitSuccess
    otherwise -> return (readMay action)


-- Validate if the given coordinates are in the board bounds.
areCordValid :: Int -> Int -> Action -> Bool
areCordValid h w (Dig i j) = 
  if (i > 0 && i <= h && j > 0 && j<= w) 
    then True
  else False
areCordValid h w (Flag i j) = 
  if (i > 0 && i <= h && j > 0 && j<= w) 
    then True
  else False
 
 