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
  Action,
  ProgState (..),
  GameState (..),
  ProgMode (..),
  GameType (..),
  Database (..),
  Player (..),
  PlayerStats (..),
  Goalie (..),
  GoalieStats (..),
  -- * Lenses
  -- ** ProgState Lenses
  database,
  progMode,
  -- ** GameState Lenses
  gameType,
  homeScore,
  awayScore,
  -- ** ProgMode Lenses
  gameTypeL,
  homeScoreL,
  awayScoreL,
  -- ** Database Lenses
  dbPlayers,
  dbGoalies,
  dbGames,
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
  -- * Constructors
  newProgState,
  newGameState,
  newDatabase,
  newPlayer,
  newPlayerStats,
  newGoalie,
  newGoalieStats,
  -- * Helper Functions
  -- ** ProgState Helpers
  teamScore,
  -- ** Player Helpers
  pPoints
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
import Lens.Micro (Lens', lens, (&), (^.), (.~))
import Lens.Micro.TH (makeLenses)
import UI.NCurses (Curses)

-- | Action which maintains program state
type Action a = StateT ProgState Curses a

-- | Represents the program state
data ProgState = ProgState
  { _database :: Database
  -- ^ The data to be saved
  , _progMode :: ProgMode
  -- ^ The program's mode
  } deriving (Eq, Show)

-- | The game state
data GameState = GameState
  { _gameType  :: Maybe GameType
  -- ^ The type of game (home/away)
  , _homeScore :: Maybe Int
  -- ^ The home team's score
  , _awayScore :: Maybe Int
  -- ^ The away team's score
  } deriving (Eq, Show)

-- | The program mode
data ProgMode
  = MainMenu
  | NewSeason
  | NewGame GameState
  deriving (Eq, Show)

-- | The type of game
data GameType
  = HomeGame
  | AwayGame
  deriving (Eq, Show)

-- | Represents the database
data Database = Database
  { _dbPlayers :: [Player]
  -- ^ The list of players
  , _dbGoalies :: [Goalie]
  -- ^ The list of goalies
  , _dbGames   :: Int
  -- ^ The number of games recorded
  } deriving (Eq, Show)

instance FromJSON Database where
  parseJSON = withObject "Database" $ \v -> Database
    <$> v .: "players"
    <*> v .: "goalies"
    <*> v .: "games"

instance ToJSON Database where
  toJSON (Database players goalies games) = object
    [ "players" .= players
    , "goalies" .= goalies
    , "games"   .= games
    ]
  toEncoding (Database players goalies games) = pairs $
    "players" .= players <>
    "goalies" .= goalies <>
    "games"   .= games

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

makeLenses ''ProgState
makeLenses ''GameState
makeLenses ''Database
makeLenses ''Player
makeLenses ''PlayerStats
makeLenses ''Goalie
makeLenses ''GoalieStats

gameTypeL :: Lens' ProgMode (Maybe GameType)
gameTypeL = lens
  (\case
    NewGame gs -> gs ^. gameType
    _          -> Nothing)
  (\m gt -> case m of
    NewGame gs -> NewGame $ gs & gameType .~ gt
    _          -> NewGame $ newGameState & gameType .~ gt)

homeScoreL :: Lens' ProgMode (Maybe Int)
homeScoreL = lens
  (\case
    NewGame gs -> gs ^. homeScore
    _          -> Nothing)
  (\m hs -> case m of
    NewGame gs -> NewGame $ gs & homeScore .~ hs
    _          -> NewGame $ newGameState & homeScore .~ hs)

awayScoreL :: Lens' ProgMode (Maybe Int)
awayScoreL = lens
  (\case
    NewGame gs -> gs ^. awayScore
    _          -> Nothing)
  (\m as -> case m of
    NewGame gs -> NewGame $ gs & awayScore .~ as
    _          -> NewGame $ newGameState & awayScore .~ as)

-- | Constructor for a 'ProgState'
newProgState :: ProgState
newProgState = ProgState
  { _database = newDatabase
  , _progMode = MainMenu
  }

-- | Constructor for a 'GameState'
newGameState :: GameState
newGameState = GameState
  { _gameType  = Nothing
  , _homeScore = Nothing
  , _awayScore = Nothing
  }

-- | Constructor for a 'Database'
newDatabase :: Database
newDatabase = Database
  { _dbPlayers = []
  , _dbGoalies = []
  , _dbGames   = 0
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

-- | Determines the team's points
teamScore :: ProgState -> Maybe Int
teamScore s = case s ^. progMode . gameTypeL of
  Just HomeGame -> s ^. progMode . homeScoreL
  Just AwayGame -> s ^. progMode . awayScoreL
  Nothing       -> Nothing

-- | Calculates a player's points
pPoints :: PlayerStats -> Int
pPoints s = s^.psGoals + s^.psAssists
