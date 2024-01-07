import Data.List

data Piece
  = Open Int
  | Player Char
  deriving Eq

instance Show Piece where
  show (Open n) = show n
  show (Player c) = [c]

removeNth :: Int -> [a] -> ([a], [a])
removeNth index 1st = (left, right)
  where
    (left, ys) = splitAt (index - 1) 1st
    right = drop 1 ys

placePiece :: [a] -> a -> Int -> [a]
placePiece board piece index = xs ++ [piece] ++ ys
  where
    (xs, ys) = removeNth index board

pieceIsOpen :: Piece -> Bool
pieceIsOpen (Open _) = True
pieceIsOpen _ = False

openSpace :: [Piece] -> Int -> Bool
openSpace board index
  | length board < i = False
  | pieceIsOpen (board !! i) = True
  | otherwise = False
  where
    i = index - 1

getPiecePosition :: [Piece] -> IO Int
getPiecePosition board = do
  putStrLn "Enter an open position (1-9): "
  input <- getChar
  if input `elem` ['1'..'9'] && openSpace board (read [input])
    then return (read [input])
    else getPiecePosition board

showBoardLine :: [Piece] -> String
showBoardLine (a : b : c : xs) = show a ++ "|" ++ show b ++ "|" ++ show c
showBoardLine _ = error "List must contain at least three elements"

boardBorder :: String
boardBorder = "\n-------\n"

showBoard :: [Piece] -> String
showBoard board =
  concat $ intersperse boardBorder [top, middle, bottom]
  where
    top = showBoardLine board
    middle = showBoardLine (drop 3 board)
    bottom = showBoardLine (drop 6 board)

swapPlayers :: Char -> Char
swapPlayers 'X' = 'O'
swapPlayers 'O' = 'X'
swapPlayers _ = error "swapPlayers only accepts the characters O or X"

checkWonVertically :: [Piece] -> Piece -> Int -> Bool
checkWonVertically board player index =
  topPos == player && middlePos == player && bottomPos == player
  where
    (topPos, middlePos, bottomPos) =
      (board !! index, board !! (index + 3), board !! (index + 6))

playerWonVertically :: [Piece] -> Piece -> Bool
playerWonVertically board player =
  or $ map (checkWonVertically board player) [0, 1, 2]

checkWonHorizontally :: [Piece] -> Piece -> Int -> Bool
checkWonHorizontally board player index =
  firstPos == player && secondPos == player && thirdPos == player
  where
    (firstPos, secondPos, thirdPos) =
      (board !! index, board !! (index + 1), board !! (index + 2))

playerWonHorizontally :: [Piece] -> Piece -> Bool
playerWonHorizontally board player =
  or $ map (checkWonHorizontally board player) [0, 3, 6]

checkWonDiagonally :: [Piece] -> Piece -> Bool
checkWonDiagonally board player =
  (firstPos == player && secondPos == player && thirdPos == player) ||
  (fourthPos == player && fifthPos == player && sixthPos == player)
  where
    (firstPos, secondPos, thirdPos, fourthPos, fifthPos, sixthPos) =
      (board !! 0, board !! 4, board !! 8, board !! 2, board !! 4, board !! 6)

playerWonDiagonally :: [Piece] -> Piece -> Bool
playerWonDiagonally board player =
  checkWonDiagonally board player || checkWonDiagonally (reverse board) player

playerWon :: [Piece] -> Piece -> Bool
playerWon board player =
  playerWonDiagonally board player ||
  playerWonHorizontally board player ||
  playerWonVertically board player

tieGame :: [Piece] -> Bool
tieGame board = all (not . pieceIsOpen) board

checkBoardState :: [Piece] -> Char -> IO ()
checkBoardState board playerChr
  | tieGame board = putStrLn "It's a tie!"
  | playerWon board (Player 'X') = putStrLn "Player X won!"
  | playerWon board (Player 'O') = putStrLn "Player O won!"
  | otherwise = runTicTacToe board (swapPlayers playerChr)

runTicTacToe :: [Piece] -> Char -> IO ()
runTicTacToe board playerChr = do
  putStrLn $ showBoard board
  position <- getPiecePosition board
  let newBoard = placePiece board (Player playerChr) position
  checkBoardState newBoard playerChr

main :: IO ()
main = runTicTacToe board 'X'
  where
    board = [Open 1, Open 2, Open 3, Open 4, Open 5, Open 6, Open 7, Open 8, Open 9]
