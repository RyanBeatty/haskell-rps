
import System.Random

-- data type enumerating the possible values
-- of a move in rock-paper-scissors
data Move = Rock
          | Paper 
          | Scissors
    deriving(Show, Eq, Enum)

-- instance declration for Move to implement
-- the Random typeclass in order to be able to
-- generate random moves. Uses the enumeration mapping
-- of the Moves (0 -> Rock), (1 -> Paper), (2 -> Scissors)
instance Random Move where
    random g = case randomR (0, 2) g of
                (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

-- Enumerates the possible outcomes
-- of a game of rock-paper-scissors
data Result = Win
            | Lose
            | Tie
    deriving(Show)

-- a Player simple has a String which is their tag
data Player = Player String
    deriving(Show)

-- Compares 2 Moves. Returns Win if m1 beats m2,
-- returns Lose if m2 beats m1, and returns Tie if m1 == m2
compareMoves :: Move -> Move -> Result
compareMoves Rock Paper     = Lose
compareMoves Rock Scissors  = Win
compareMoves Paper Rock     = Win
compareMoves Paper Scissors = Lose
compareMoves Scissors Rock  = Lose
compareMoves Scissors Paper = Win
compareMoves _ _            = Tie

--pickRandomMove :: (RandomGen g) -














