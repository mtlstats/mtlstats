{- |

mtlstats
Copyright (C) 2019 Rhéal Lamothe
<rheal.lamothe@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

-}

{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}

module Mtlstats.Types (
  -- * Types
  Controller (..),
  Action,
  ProgState (..),
  ProgMode (..),
  GameState (..),
  GameType (..),
  CreatePlayerState (..),
  Database (..),
  Player (..),
  PlayerStats (..),
  Goalie (..),
  GoalieStats (..),
  GameStats (..),
  Prompt (..),
  -- * Lenses
  -- ** ProgState Lenses
  database,
  progMode,
  inputBuffer,
  -- ** ProgMode Lenses
  gameStateL,
  createPlayerStateL,
  -- ** GameState Lenses
  gameYear,
  gameMonth,
  gameDay,
  gameType,
  otherTeam,
  homeScore,
  awayScore,
  overtimeFlag,
  dataVerified,
  pointsAccounted,
  -- ** CreatePlayerState Lenses
  cpsNumber,
  cpsName,
  cpsPosition,
  cpsSuccessCallback,
  cpsFailureCallback,
  -- ** Database Lenses
  dbPlayers,
  dbGoalies,
  dbGames,
  dbHomeGameStats,
  dbAwayGameStats,
  -- ** Player Lenses
  pNumber,
  pName,
  pPosition,
  pYtd,
  pLifetime,
  -- ** PlayerStats Lenses
  psGoals,
  psAssists,
  psPMin,
  -- ** Goalie Lenses
  gNumber,
  gName,
  gYtd,
  gLifetime,
  -- ** GoalieStats Lenses
  gsGames,
  gsMinsPlayed,
  gsGoalsAllowed,
  gsGoalsAgainst,
  gsWins,
  gsLosses,
  gsTies,
  -- ** GameStats Lenses
  gmsWins,
  gmsLosses,
  gmsOvertime,
  -- * Constructors
  newProgState,
  newGameState,
  newCreatePlayerState,
  newDatabase,
  newPlayer,
  newPlayerStats,
  newGoalie,
  newGoalieStats,
  newGameStats,
  -- * Helper Functions
  -- ** GameState Helpers
  teamScore,
  otherScore,
  homeTeam,
  awayTeam,
  gameWon,
  gameLost,
  gameTied,
  unaccountedPoints,
  -- ** GameStats Helpers
  gmsGames,
  gmsPoints,
  addGameStats,
  -- ** Player Helpers
  pPoints,
  playerSearch
) where

import Control.Monad.Trans.State (StateT)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , object
  , pairs
  , parseJSON
  , toEncoding
  , toJSON
  , withObject
  , (.:)
  , (.=)
  )
import Data.List (isInfixOf)
import Lens.Micro (Lens', lens, (&), (^.), (.~))
import Lens.Micro.TH (makeLenses)
import qualified UI.NCurses as C

import Mtlstats.Config

-- | Controls the program flow
data Controller = Controller
  { drawController   :: ProgState -> C.Update C.CursorMode
  -- ^ The drawing phase
  , handleController :: C.Event -> Action Bool
  -- ^ The event handler
  }

-- | Action which maintains program state
type Action a = StateT ProgState C.Curses a

-- | Represents the program state
data ProgState = ProgState
  { _database    :: Database
  -- ^ The data to be saved
  , _progMode    :: ProgMode
  -- ^ The program's mode
  , _inputBuffer :: String
  -- ^ Buffer for user input
  }

-- | The program mode
data ProgMode
  = MainMenu
  | NewSeason
  | NewGame GameState
  | CreatePlayer CreatePlayerState

instance Show ProgMode where
  show MainMenu         = "MainMenu"
  show NewSeason        = "NewSeason"
  show (NewGame _)      = "NewGame"
  show (CreatePlayer _) = "CreatePlayer"

-- | The game state
data GameState = GameState
  { _gameYear        :: Maybe Int
  -- ^ The year the game took place
  , _gameMonth       :: Maybe Int
  -- ^ The month the game took place
  , _gameDay         :: Maybe Int
  -- ^ The day of the month the game took place
  , _gameType        :: Maybe GameType
  -- ^ The type of game (home/away)
  , _otherTeam       :: String
  -- ^ The name of the other team
  , _homeScore       :: Maybe Int
  -- ^ The home team's score
  , _awayScore       :: Maybe Int
  -- ^ The away team's score
  , _overtimeFlag    :: Maybe Bool
  -- ^ Indicates whether or not the game went into overtime
  , _dataVerified    :: Bool
  -- ^ Set to 'True' when the user confirms the entered data
  , _pointsAccounted :: Int
  } deriving (Eq, Show)

-- | The type of game
data GameType
  = HomeGame
  | AwayGame
  deriving (Eq, Show)

-- | Player creation status
data CreatePlayerState = CreatePlayerState
  { _cpsNumber          :: Maybe Int
  -- ^ The player's number
  , _cpsName            :: String
  -- ^ The player's name
  , _cpsPosition        :: String
  -- ^ The player's position
  , _cpsSuccessCallback :: ProgState -> ProgState
  -- ^ The function to call on success
  , _cpsFailureCallback :: ProgState -> ProgState
  -- ^ The function to call on failure
  }

-- | Represents the database
data Database = Database
  { _dbPlayers       :: [Player]
  -- ^ The list of players
  , _dbGoalies       :: [Goalie]
  -- ^ The list of goalies
  , _dbGames         :: Int
  -- ^ The number of games recorded
  , _dbHomeGameStats :: GameStats
  -- ^ Statistics for home games
  , _dbAwayGameStats :: GameStats
  -- ^ Statistics for away games
  } deriving (Eq, Show)

instance FromJSON Database where
  parseJSON = withObject "Database" $ \v -> Database
    <$> v .: "players"
    <*> v .: "goalies"
    <*> v .: "games"
    <*> v .: "home_game_stats"
    <*> v .: "away_game_stats"

instance ToJSON Database where
  toJSON (Database players goalies games hgs ags) = object
    [ "players"         .= players
    , "goalies"         .= goalies
    , "games"           .= games
    , "home_game_stats" .= hgs
    , "away_game_stats" .= ags
    ]
  toEncoding (Database players goalies games hgs ags) = pairs $
    "players"         .= players <>
    "goalies"         .= goalies <>
    "games"           .= games   <>
    "home_game_stats" .= hgs     <>
    "away_game_stats" .= ags

-- | Represents a (non-goalie) player
data Player = Player
  { _pNumber   :: Int
  -- ^ The player's number
  , _pName     :: String
  -- ^ The player's name
  , _pPosition :: String
  -- ^ The player's position
  , _pYtd      :: PlayerStats
  -- ^ The Player's year-to-date stats
  , _pLifetime :: PlayerStats
  -- ^ The player's lifetime stats
  } deriving (Eq, Show)

instance FromJSON Player where
  parseJSON = withObject "Player" $ \v -> Player
    <$> v .: "number"
    <*> v .: "name"
    <*> v .: "position"
    <*> v .: "ytd"
    <*> v .: "lifetime"

instance ToJSON Player where
  toJSON (Player num name pos ytd lt) = object
    [ "number"   .= num
    , "name"     .= name
    , "position" .= pos
    , "ytd"      .= ytd
    , "lifetime" .= lt
    ]
  toEncoding (Player num name pos ytd lt) = pairs $
    "number"   .= num  <>
    "name"     .= name <>
    "position" .= pos  <>
    "ytd"      .= ytd  <>
    "lifetime" .= lt

-- | Represents a (non-goalie) player's stats
data PlayerStats = PlayerStats
  { _psGoals   :: Int
  -- ^ The number of goals
  , _psAssists :: Int
  -- ^ The number of assists
  , _psPMin    :: Int
  -- ^ The number of penalty minutes
  } deriving (Eq, Show)

instance FromJSON PlayerStats where
  parseJSON = withObject "PlayerStats" $ \v -> PlayerStats
    <$> v .: "goals"
    <*> v .: "assists"
    <*> v .: "penalty_mins"

instance ToJSON PlayerStats where
  toJSON (PlayerStats g a pm) = object
    [ "goals"        .= g
    , "assists"      .= a
    , "penalty_mins" .= pm
    ]
  toEncoding (PlayerStats g a pm) = pairs $
    "goals"        .= g  <>
    "assists"      .= a  <>
    "penalty_mins" .= pm

-- | Represents a goalie
data Goalie = Goalie
  { _gNumber   :: Int
  -- ^ The goalie's number
  , _gName     :: String
  -- ^ The goalie's name
  , _gYtd      :: GoalieStats
  -- ^ The goalie's year-to-date stats
  , _gLifetime :: GoalieStats
  -- ^ The goalie's lifetime stats
  } deriving (Eq, Show)

instance FromJSON Goalie where
  parseJSON = withObject "Goalie" $ \v -> Goalie
    <$> v .: "number"
    <*> v .: "name"
    <*> v .: "ytd"
    <*> v .: "lifetime"

instance ToJSON Goalie where
  toJSON (Goalie num name ytd lt) = object
    [ "number"   .= num
    , "name"     .= name
    , "ytd"      .= ytd
    , "lifetime" .= lt
    ]
  toEncoding (Goalie num name ytd lt) = pairs $
    "number"   .= num  <>
    "name"     .= name <>
    "ytd"      .= ytd  <>
    "lifetime" .= lt

-- | Represents a goalie's stats
data GoalieStats = GoalieStats
  { _gsGames        :: Int
  -- ^ The number of games played
  , _gsMinsPlayed   :: Int
  -- ^ The number of minutes played
  , _gsGoalsAllowed :: Int
  -- ^ The number of goals allowed
  , _gsGoalsAgainst :: Int
  -- ^ The number of goals against
  , _gsWins         :: Int
  -- ^ The number of wins
  , _gsLosses       :: Int
  -- ^ The number of losses
  , _gsTies         :: Int
  -- ^ The number of ties
  } deriving (Eq, Show)

instance FromJSON GoalieStats where
  parseJSON = withObject "GoalieStats" $ \v -> GoalieStats
    <$> v .: "games"
    <*> v .: "mins_played"
    <*> v .: "goals_allowed"
    <*> v .: "goals_against"
    <*> v .: "wins"
    <*> v .: "losses"
    <*> v .: "ties"

instance ToJSON GoalieStats where
  toJSON (GoalieStats g m al ag w l t) = object
    [ "games"         .= g
    , "mins_played"   .= m
    , "goals_allowed" .= al
    , "goals_against" .= ag
    , "wins"          .= w
    , "losses"        .= l
    , "ties"          .= t
    ]
  toEncoding (GoalieStats g m al ag w l t) = pairs $
      "games"         .= g  <>
      "mins_played"   .= m  <>
      "goals_allowed" .= al <>
      "goals_against" .= ag <>
      "wins"          .= w  <>
      "losses"        .= l  <>
      "ties"          .= t

-- | Game statistics
data GameStats = GameStats
  { _gmsWins     :: Int
  -- ^ Games won
  , _gmsLosses   :: Int
  -- ^ Games lost
  , _gmsOvertime :: Int
  -- ^ Games lost in overtime
  } deriving (Eq, Show)

instance FromJSON GameStats where
  parseJSON = withObject "GameStats" $ \v -> GameStats
    <$> v .: "wins"
    <*> v .: "losses"
    <*> v .: "overtime"

instance ToJSON GameStats where
  toJSON (GameStats w l ot) = object
    [ "wins"     .= w
    , "losses"   .= l
    , "overtime" .= ot
    ]
  toEncoding (GameStats w l ot) = pairs $
    "wins"     .= w  <>
    "losses"   .= l  <>
    "overtime" .= ot

-- | Defines a user prompt
data Prompt = Prompt
  { promptDrawer      :: ProgState -> C.Update ()
  -- ^ Draws the prompt to thr screen
  , promptCharCheck   :: Char -> Bool
  -- ^ Determines whether or not the character is valid
  , promptAction      :: String -> Action ()
  -- ^ Action to perform when the value is entered
  , promptFunctionKey :: Integer -> Action ()
  -- ^ Action to perform when a function key is pressed
  }

makeLenses ''ProgState
makeLenses ''GameState
makeLenses ''CreatePlayerState
makeLenses ''Database
makeLenses ''Player
makeLenses ''PlayerStats
makeLenses ''Goalie
makeLenses ''GoalieStats
makeLenses ''GameStats

gameStateL :: Lens' ProgMode GameState
gameStateL = lens
  (\case
    NewGame gs -> gs
    _          -> newGameState)
  (\_ gs -> NewGame gs)

createPlayerStateL :: Lens' ProgMode CreatePlayerState
createPlayerStateL = lens
  (\case
    CreatePlayer cps -> cps
    _                -> newCreatePlayerState)
  (\_ cps -> CreatePlayer cps)

-- | Constructor for a 'ProgState'
newProgState :: ProgState
newProgState = ProgState
  { _database    = newDatabase
  , _progMode    = MainMenu
  , _inputBuffer = ""
  }

-- | Constructor for a 'GameState'
newGameState :: GameState
newGameState = GameState
  { _gameYear        = Nothing
  , _gameMonth       = Nothing
  , _gameDay         = Nothing
  , _gameType        = Nothing
  , _otherTeam       = ""
  , _homeScore       = Nothing
  , _awayScore       = Nothing
  , _overtimeFlag    = Nothing
  , _dataVerified    = False
  , _pointsAccounted = 0
  }

-- | Constructor for a 'CreatePlayerState'
newCreatePlayerState :: CreatePlayerState
newCreatePlayerState = CreatePlayerState
  { _cpsNumber          = Nothing
  , _cpsName            = ""
  , _cpsPosition        = ""
  , _cpsSuccessCallback = id
  , _cpsFailureCallback = id
  }

-- | Constructor for a 'Database'
newDatabase :: Database
newDatabase = Database
  { _dbPlayers       = []
  , _dbGoalies       = []
  , _dbGames         = 0
  , _dbHomeGameStats = newGameStats
  , _dbAwayGameStats = newGameStats
  }

-- | Constructor for a 'Player'
newPlayer
  :: Int
  -- ^ The player's number
  -> String
  -- ^ The player's name
  -> String
  -- ^ The player's position
  -> Player
newPlayer num name pos = Player
  { _pNumber   = num
  , _pName     = name
  , _pPosition = pos
  , _pYtd      = newPlayerStats
  , _pLifetime = newPlayerStats
  }

-- | Constructor for a 'PlayerStats' value
newPlayerStats :: PlayerStats
newPlayerStats = PlayerStats
  { _psGoals   = 0
  , _psAssists = 0
  , _psPMin    = 0
  }

-- | Constructor for a 'Goalie'
newGoalie
  :: Int
  -- ^ The goalie's number
  -> String
  -- ^ The goalie's name
  -> Goalie
newGoalie num name = Goalie
  { _gNumber   = num
  , _gName     = name
  , _gYtd      = newGoalieStats
  , _gLifetime = newGoalieStats
  }

-- | Constructor for a 'GoalieStats' value
newGoalieStats :: GoalieStats
newGoalieStats = GoalieStats
  { _gsGames        = 0
  , _gsMinsPlayed   = 0
  , _gsGoalsAllowed = 0
  , _gsGoalsAgainst = 0
  , _gsWins         = 0
  , _gsLosses       = 0
  , _gsTies         = 0
  }

-- | Constructor for a 'GameStats' value
newGameStats :: GameStats
newGameStats = GameStats
  { _gmsWins     = 0
  , _gmsLosses   = 0
  , _gmsOvertime = 0
  }

-- | Determines the team's score
teamScore :: GameState -> Maybe Int
teamScore s = case s ^. gameType of
  Just HomeGame -> s ^. homeScore
  Just AwayGame -> s ^. awayScore
  Nothing       -> Nothing

-- | Determines the other team's score
otherScore :: GameState -> Maybe Int
otherScore s = case s ^. gameType of
  Just HomeGame -> s ^. awayScore
  Just AwayGame -> s ^. homeScore
  Nothing       -> Nothing

-- | Returns the name of the home team (or an empty string if
-- unavailable)
homeTeam :: GameState -> String
homeTeam gs = case gs^.gameType of
  Just HomeGame -> myTeam
  Just AwayGame -> gs^.otherTeam
  Nothing       -> ""

-- | Returns the name of the visiting team (or an empty string if
-- unavailable)
awayTeam :: GameState -> String
awayTeam gs = case gs^.gameType of
  Just HomeGame -> gs^.otherTeam
  Just AwayGame -> myTeam
  Nothing       -> ""

-- | Checks if the game was won
gameWon :: GameState -> Maybe Bool
gameWon gs = (>) <$> teamScore gs <*> otherScore gs

-- | Checks if the game was lost
gameLost :: GameState -> Maybe Bool
gameLost gs = do
  ot    <- gs^.overtimeFlag
  team  <- teamScore gs
  other <- otherScore gs
  Just $ not ot && other > team

-- | Checks if the game has tied
gameTied :: GameState -> Maybe Bool
gameTied gs = (==) <$> gs^.homeScore <*> gs^.awayScore

-- | Checks for unaccounted points
unaccountedPoints :: GameState -> Maybe Bool
unaccountedPoints gs = do
  scored <- teamScore gs
  let accounted = gs^.pointsAccounted
  Just $ scored > accounted

-- | Calculates the number of games played
gmsGames :: GameStats -> Int
gmsGames gs = gs^.gmsWins + gs^.gmsLosses + gs^.gmsOvertime

-- | Calculates the number of points
gmsPoints :: GameStats -> Int
gmsPoints gs = 2 * gs^.gmsWins + gs^. gmsOvertime

-- | Adds two 'GameStats' values together
addGameStats :: GameStats -> GameStats -> GameStats
addGameStats s1 s2 = GameStats
  { _gmsWins     = s1^.gmsWins + s2^.gmsWins
  , _gmsLosses   = s1^.gmsLosses + s2^.gmsLosses
  , _gmsOvertime = s1^.gmsOvertime + s2^.gmsOvertime
  }

-- | Calculates a player's points
pPoints :: PlayerStats -> Int
pPoints s = s^.psGoals + s^.psAssists

-- | Searches through a list of players
playerSearch
  :: String
  -- ^ The search string
  -> [Player]
  -- ^ The list of players to search
  -> [(Int, Player)]
  -- ^ The matching players with their index numbers
playerSearch sStr =
  filter (match sStr) .
  zip [0..]
  where match sStr (_, p) = sStr `isInfixOf` (p^.pName)
