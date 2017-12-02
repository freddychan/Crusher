-- CPSC 312 - Project 2
-- by Khurram Ali Jaffery

-- Frederick Chan, 32199135 e1h0b
-- Wyan Nie, 38195137 i4v9a

-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--		 W is a piece of the White player
--		 B is a piece of the Black player
--

data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements 
-- representing what a point is occupied by
-- where the first element represents a piece 
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate 
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
-- 
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--		 newBoard is the next board to add to the tree
-- 		 seenBoards is the updated history to avoid possible future trouble boards
-- 		 cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of 
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
-- 		 board is the game state at that node
-- 		 nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move over
--		 the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
-- 		 the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--		 is that a jump can be reduced to a move as in effect 
--		 nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- Some test results to see what functions are producing 
--
run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
grid0 = generateGrid 3 2 4 []
slides0 = generateSlides grid0 3
jumps0 = generateLeaps grid0 3
board0 = sTrToBoard "WWW-WW-------BB-BBB"
newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
--heuristic0 = boardEvaluator W [] 3

--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of 
-- search tree, the size of the provide boards, and produces the 
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) p d n = map boardToStr (nextBoard:board:history)
	where
		board = sTrToBoard current -- converts to lists of pieces
		history = map sTrToBoard old -- returns boards that have been seen already
		player = getPlayer p -- return current turn's player
		grid = generateGrid n (n-1) (2*(n-1)) [] -- make the board
		slides = generateSlides grid n -- generate possible "slide" moves
		jumps = generateLeaps grid n -- generate possible "leap" moves
		nextBoard = stateSearch board history grid slides jumps player d n -- looks for next best board configuration

-- helper function to determine current turn's player
getPlayer :: Char -> Piece
getPlayer 'W'   = W
getPlayer 'B'   = B

--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
--

gameOver :: Board -> [Board] -> Int -> Bool
-- checks if current board has been seen before
-- checks if pieces of B is less than n therefore B loses
-- checks if pieces of W is less than n therefore W loses
gameOver board history n = elem board history || occurencesOf B board < n || occurencesOf W board < n
	where occurencesOf e lst = foldl (\ l r -> if r == e then l + 1 else l) 0 lst
-- occurencesOf is a helper function to determine amount of B or W on the board


--
-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
-- 	     [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
	where 
		check 'W' = W
		check 'B' = B
		check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B] 
-- 	     to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board 
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
	where 
		check W = 'W'
		check B = 'B'
		check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid 
--		   initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--		 [(0,0),(1,0),(2,0)
--		  (0,1),(1,1),(2,1),(3,1)
--		  (0,2),(1,2),(2,2),(3,2),(4,2)
--		  (0,3),(1,3),(2,3),(3,3)
--		  (0,4),(1,4),(2,4)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
	| n3 == -1		= acc
	| otherwise 	= generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
		where
			row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
			nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible slides is only generated once; and when 
-- 		 generating next moves, the program decides which slides out of 
--		 all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
--

generateSlides :: Grid -> Int -> [Slide]
generateSlides b n = filter (validSlide b) (generateAllSlides b n)

-- generates all slides including non valid slides on the grid
generateAllSlides :: Grid -> Int -> [Slide]
generateAllSlides [] _ = []
generateAllSlides ((x,y):xs) n = (generatePossibleSlides (x,y) n) ++ (generateAllSlides xs n)

-- generates all possible slides from the given point 
generatePossibleSlides :: Point -> Int -> [Slide]
generatePossibleSlides (x,y) n
	| y == n - 1 = [((x,y),(x-1,y+1)),((x,y),(x+1,y)),((x,y),(x,y+1)),((x,y),(x-1,y-1)),((x,y),(x-1,y)),((x,y),(x,y-1))]
	| y < n - 1 = [((x,y),(x+1,y+1)),((x,y),(x+1,y)),((x,y),(x,y+1)),((x,y),(x-1,y-1)),((x,y),(x-1,y)),((x,y),(x,y-1))]
	| y > n - 1 = [((x,y),(x+1,y-1)),((x,y),(x+1,y)),((x,y),(x,y+1)),((x,y),(x-1,y)),((x,y),(x-1,y+1)),((x,y),(x,y-1))]

-- checks whether the given slide from the given grid is valid
validSlide :: Grid -> Slide -> Bool
validSlide grid (_,(x,y)) = elem (x,y) grid

--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible leaps is only generated once; and when 
-- 		 generating next moves, the program decides which leaps out of 
--		 all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n = filter (validLeap b) (generateAllLeaps b n)

-- generates all leaps including non valid leaps on the grid
generateAllLeaps :: Grid -> Int -> [Jump]
generateAllLeaps [] _ = []
generateAllLeaps ((x,y):xs) n = (generatePossibleLeaps (x,y) n) ++ (generateAllLeaps xs n)

-- generates all possible leaps from the given point
generatePossibleLeaps :: Point -> Int -> [Jump]
generatePossibleLeaps (x,y) n
  | y == n - 1 = [((x,y),(x-1,y),(x-2,y)),((x,y),(x+1,y),(x+2,y)),((x,y),(x,y+1),(x,y+2)),((x,y),(x-1,y+1),(x-2,y+2)),((x,y),(x,y-1),(x,y-2)),((x,y),(x-1,y-1),(x-2,y-2))]
  | y == n - 2 = [((x,y),(x-1,y),(x-2,y)),((x,y),(x+1,y),(x+2,y)),((x,y),(x+1,y+1),(x+1,y+2)),((x,y),(x,y+1),(x-1,y+2)),((x,y),(x,y-1),(x,y-2)),((x,y),(x-1,y-1),(x-2,y-2))]
  | y == n = [((x,y),(x-1,y),(x-2,y)),((x,y),(x+1,y),(x+2,y)),((x,y),(x,y+1),(x,y+2)),((x,y),(x-1,y+1),(x-2,y+2)),((x,y),(x+1,y-1),(x+1,y-2)),((x,y),(x,y-1),(x-1,y-2))]
  | y > n = [((x,y),(x-1,y),(x-2,y)),((x,y),(x+1,y),(x+2,y)),((x,y),(x,y+1),(x,y+2)),((x,y),(x-1,y+1),(x-2,y+2)),((x,y),(x+1,y-1),(x+2,y-2)),((x,y),(x,y-1),(x,y-2))]
  | y < n - 2 = [((x,y),(x-1,y),(x-2,y)),((x,y),(x+1,y),(x+2,y)),((x,y),(x+1,y+1),(x+2,y+2)),((x,y),(x,y+1),(x,y+2)),((x,y),(x,y-1),(x,y-2)),((x,y),(x-1,y-1),(x-2,y-2))]

-- checks whether the given leap from the given grid is valid 
validLeap :: Grid -> Jump -> Bool
validLeap grid (_,_,(x,y)) = elem (x,y) grid

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the 
-- board, else generate a search tree till the specified depth and apply 
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over, 
--          otherwise produces the next best board
--

stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth num = minimax boardTree heuristic
    where
        boardTree = generateTree board history grid slides jumps player depth num
        heuristic b _ = boardEvaluator player num b
		
--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n = generateTree_h board history grid slides jumps player depth 0 n

-- helper function that recurses down tree
-- if gameOver, returns a Node
-- if reached depth of tree, returns a Node 
-- otherwise returns Node for current board and children
generateTree_h :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Int -> BoardTree
generateTree_h board history grid slides jumps player depth curr n  
    | curr == depth             = Node curr board []
    | gameOver board history n  = Node curr board []
    | otherwise                 = Node curr board restTree
    where
        nextBoards = generateNewStates board history grid slides jumps player
        generate x = generateTree_h x (x:history) grid slides jumps (nextPlayer player) depth (curr+1) n
        restTree = map generate nextBoards

-- helper function for finding next player
nextPlayer :: Piece -> Piece
nextPlayer player
    | W == player   = B
    | B == player   = W
	
--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player = newBoards
    where
        state = zip board grid
        moves = moveGenerator state slides jumps player
        newBoards = generateNewStates_h state history player moves

generateNewStates_h :: State -> [Board] -> Piece -> [Move] -> [Board]

generateNewStates_h _ _ _ [] = []       -- no more moves left
generateNewStates_h state history player ((p1,p2):rest)
    | elem newBoard history = generateNewStates_h state history player rest
    | otherwise             = newBoard : 
                            generateNewStates_h state history player rest
    where
        repl x = tileReplace p1 p2 x player
        newState = map repl state       -- reflect move in new state
        newBoard = map fst newState
		
-- helper function to update new states
tileReplace :: Point -> Point -> Tile -> Piece -> Tile
tileReplace p1 p2 (piece, point) player
    | p1 == point   = (D, point)
    | p2 == point   = (player, point)
    | otherwise     = (piece, point)
	
--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps, 
-- a list of possible slides and a player from whose perspective 
-- to generate moves, to check which of these jumps and slides 
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--		 type State, for our purposes it is zipping the board and the
--		 grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--

moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player = moveGenerator_h state state slides jumps player								 

moveGenerator_h :: State -> State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator_h _ [] _ _ _ = []
moveGenerator_h state ((piece,point):rest) slides jumps player
    | piece == player   = vS ++ vJ ++ moveGenerator_h state rest slides jumps player
    | otherwise         = moveGenerator_h state rest slides jumps player
    where
        relevSlides = filter ((==point).fst) slides
        vS = validSlideMoves state relevSlides
        relevJumps = filter (\(x,_,_) -> x == point ) jumps
        vJ = validJumpMoves state relevJumps player


-- takes current board state, and possible slide moves for a given piece
-- returns possible slides
validSlideMoves :: State -> [Slide] -> [Move]
validSlideMoves _ [] = []
validSlideMoves state ((first,point):rest)
    | piece == D    = (first,point) : validSlideMoves state rest
    | otherwise     = validSlideMoves state rest
    where
        ((piece,_):_) = filter ((==point).snd) state
		
-- takes current board state, and possible jumps moves for a given piece
-- returns possible jumps	
validJumpMoves :: State -> [Jump] -> Piece -> [Move]
validJumpMoves _ [] _ = []
validJumpMoves state ((first,center,end):rest) player
    | valid     = (first,end) : validJumpMoves state rest player
    | otherwise = validJumpMoves state rest player
    where
        ((c_piece,_):_) = filter ((==center).snd) state
        ((e_piece,_):_) = filter ((==end).snd) state
        valid = c_piece == player && e_piece /= player 
		
--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

boardEvaluator :: Piece -> Int -> Board -> Int
boardEvaluator player n board
  | (gameWonByPawnCount player n board)               = 10
  | (gameWonByPawnCount (otherPlayer player) n board) = -10
  | otherwise                                         = (calculateGoodnessValue player board)

-- returns the character of the opposite team
otherPlayer :: Piece -> Piece
otherPlayer player
  | player == W = B
  | otherwise     = W

-- returns true if opposing player's pawns are less than n
gameWonByPawnCount :: Piece -> Int -> Board -> Bool
gameWonByPawnCount player n board = n > (getPawnCount board (otherPlayer player) ) 

-- returns number of pawns on the board for the given player
getPawnCount :: Board -> Piece -> Int
getPawnCount board player 
  = (length (filter (== player) board))

-- goodness value of provided board calculated by 
-- taking the given player's number of pawns and 
-- subtracting the enemy player's number of pawns
-- simple heuristic, may need more work
calculateGoodnessValue :: Piece -> Board -> Int
calculateGoodnessValue player board =
  ((getPawnCount board player) - (getPawnCount board (otherPlayer player)))
  
--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree, 
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--

minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node _ b children) heuristic = getMaxBoard scoredBoards
    where
        mini x = minimax' x heuristic True
        scores = map mini children
        getBoards (Node _ board _) = board
        potentialBoards =  map getBoards children
        scoredBoards = zip potentialBoards scores
		
-- takes list of (Board, Int), Int is corresponding score
-- returns best possible Board for player
getMaxBoard :: [(Board, Int)] -> Board
getMaxBoard []  = []
getMaxBoard ((board, score):rest) = getMaxBoard' ((board, score):rest) board score

getMaxBoard' :: [(Board,Int)] -> Board -> Int -> Board
getMaxBoard' [] board _ = board     -- Iterated through all potential boards
getMaxBoard' ((board, score):rest) currBoard currScore
    | score > currScore = getMaxBoard' rest board score
    | otherwise         = getMaxBoard' rest currBoard currScore

--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes 
-- a search tree, an appropriate heuristic to apply to the leaf nodes of 
-- the tree, and based on whether it would have been the maximizing 
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
-- 				 or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--

-- true when maximizing
-- false when minimizing
minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
minimax' (Node _ board []) heuristic maxPlayer = (heuristic board maxPlayer)
minimax' (Node _ _ children) heuristic maxPlayer
    | maxPlayer == True = maximum scores
    | otherwise         = minimum scores
        where
            mini x = minimax' x heuristic (changemax maxPlayer)
            scores = map mini children

-- helper function that changes maxPlayer for different players while traversing BoardTree
changemax :: Bool -> Bool
changemax val
    | val == True   = False
    | val == False  = True



