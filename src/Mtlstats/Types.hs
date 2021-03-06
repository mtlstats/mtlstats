{- |

mtlstats
Copyright (C) 1984, 1985, 2019, 2020, 2021 Rhéal Lamothe
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
  CreateGoalieState (..),
  EditPlayerState (..),
  EditPlayerMode (..),
  EditGoalieState (..),
  EditGoalieMode (..),
  EditStandingsMode (..),
  ESMSubMode (..),
  Database (..),
  Player (..),
  PlayerStats (..),
  Goalie (..),
  GoalieStats (..),
  GameStats (..),
  Prompt (..),
  SelectParams (..),
  TableCell (..),
  -- * Lenses
  -- ** ProgState Lenses
  database,
  progMode,
  dbName,
  inputBuffer,
  scrollOffset,
  -- ** ProgMode Lenses
  gameStateL,
  createPlayerStateL,
  createGoalieStateL,
  editPlayerStateL,
  editGoalieStateL,
  editStandingsModeL,
  -- ** EditStandingsMode Lenses
  esmSubModeL,
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
  goalBy,
  assistsBy,
  gamePlayerStats,
  confirmGoalDataFlag,
  gameSelectedPlayer,
  gamePMinsRecorded,
  gameGoalieStats,
  gameSelectedGoalie,
  gameGoalieMinsPlayed,
  gameGoalsAllowed,
  gameGoaliesRecorded,
  gameGoalieAssigned,
  -- ** CreatePlayerState Lenses
  cpsNumber,
  cpsName,
  cpsPosition,
  cpsRookieFlag,
  cpsActiveFlag,
  cpsSuccessCallback,
  cpsFailureCallback,
  -- ** CreateGoalieState Lenses
  cgsNumber,
  cgsName,
  cgsRookieFlag,
  cgsActiveFlag,
  cgsSuccessCallback,
  cgsFailureCallback,
  -- ** EditPlayerState Lenses
  epsSelectedPlayer,
  epsMode,
  epsCallback,
  -- ** EditGoalieState Lenses
  egsSelectedGoalie,
  egsMode,
  egsCallback,
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
  pRookie,
  pActive,
  pYtd,
  pLifetime,
  -- ** PlayerStats Lenses
  psGoals,
  psAssists,
  psPMin,
  -- ** Goalie Lenses
  gNumber,
  gName,
  gRookie,
  gActive,
  gYtd,
  gLifetime,
  -- ** GoalieStats Lenses
  gsGames,
  gsMinsPlayed,
  gsGoalsAllowed,
  gsShutouts,
  gsWins,
  gsLosses,
  gsTies,
  -- ** GameStats Lenses
  gmsWins,
  gmsLosses,
  gmsOvertime,
  gmsGoalsFor,
  gmsGoalsAgainst,
  -- * Constructors
  newProgState,
  newGameState,
  newCreatePlayerState,
  newCreateGoalieState,
  newEditPlayerState,
  newEditGoalieState,
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
  playerSearch,
  activePlayerSearch,
  playerSearchExact,
  modifyPlayer,
  playerSummary,
  playerIsActive,
  -- ** PlayerStats Helpers
  psPoints,
  addPlayerStats,
  -- ** Goalie Helpers
  goalieSearch,
  activeGoalieSearch,
  goalieSearchExact,
  goalieSummary,
  goalieIsActive,
  -- ** GoalieStats Helpers
  addGoalieStats,
  gsAverage
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
  , (.:?)
  , (.!=)
  , (.=)
  )
import Data.Char (toUpper)
import Data.List (find, isInfixOf)
import qualified Data.Map as M
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
  { _database     :: Database
  -- ^ The data to be saved
  , _progMode     :: ProgMode
  -- ^ The program's mode
  , _dbName       :: String
  -- ^ The name of the database file
  , _inputBuffer  :: String
  -- ^ Buffer for user input
  , _scrollOffset :: Int
  -- ^ The scrolling offset for the display
  }

-- | The program mode
data ProgMode
  = TitleScreen
  | MainMenu
  | NewSeason Bool
  | NewGame GameState
  | EditMenu
  | CreatePlayer CreatePlayerState
  | CreateGoalie CreateGoalieState
  | EditPlayer EditPlayerState
  | EditGoalie EditGoalieState
  | EditStandings EditStandingsMode

instance Show ProgMode where
  show TitleScreen       = "TitleScreen"
  show MainMenu          = "MainMenu"
  show (NewSeason _)     = "NewSeason"
  show (NewGame _)       = "NewGame"
  show EditMenu          = "EditMenu"
  show (CreatePlayer _)  = "CreatePlayer"
  show (CreateGoalie _)  = "CreateGoalie"
  show (EditPlayer _)    = "EditPlayer"
  show (EditGoalie _)    = "EditGoalie"
  show (EditStandings _) = "EditStandings"

-- | The game state
data GameState = GameState
  { _gameYear            :: Maybe Int
  -- ^ The year the game took place
  , _gameMonth           :: Maybe Int
  -- ^ The month the game took place
  , _gameDay             :: Maybe Int
  -- ^ The day of the month the game took place
  , _gameType            :: Maybe GameType
  -- ^ The type of game (home/away)
  , _otherTeam           :: String
  -- ^ The name of the other team
  , _homeScore           :: Maybe Int
  -- ^ The home team's score
  , _awayScore           :: Maybe Int
  -- ^ The away team's score
  , _overtimeFlag        :: Maybe Bool
  -- ^ Indicates whether or not the game went into overtime
  , _dataVerified        :: Bool
  -- ^ Set to 'True' when the user confirms the entered data
  , _pointsAccounted     :: Int
  -- ^ The number of game points accounted for
  , _goalBy              :: Maybe Int
  -- ^ The index number of the player who scored the most recently
  -- entered goal
  , _assistsBy            :: [Int]
  -- ^ The index numbers of the players who have assisted the most
  -- recently entered goal
  , _gamePlayerStats      :: M.Map Int PlayerStats
  -- ^ The player stats accumulated over the game
  , _confirmGoalDataFlag  :: Bool
  -- ^ Set when the user confirms the goal data
  , _gameSelectedPlayer   :: Maybe Int
  -- ^ Index number of the selected 'Player'
  , _gamePMinsRecorded    :: Bool
  -- ^ Set when the penalty mintes have been recorded
  , _gameGoalieStats      :: M.Map Int GoalieStats
  -- ^ The goalie stats accumulated over the game
  , _gameSelectedGoalie   :: Maybe Int
  -- ^ Index number of the selected 'Goalie'
  , _gameGoalieMinsPlayed :: Maybe Int
  -- ^ The number of minutes the currently selected goalie played in
  -- the game
  , _gameGoalsAllowed     :: Maybe Int
  -- ^ The number of goals the currently selected goalie allowed in
  -- the game
  , _gameGoaliesRecorded  :: Bool
  -- ^ Set when the user confirms that all goalie info has been entered
  , _gameGoalieAssigned   :: Bool
  -- ^ Set to 'True' when the goalie has been selected who will be
  -- given the win/loss/tie
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
  , _cpsRookieFlag      :: Maybe Bool
  -- ^ Indicates whether or not the player is a rookie
  , _cpsActiveFlag      :: Maybe Bool
  -- ^ Indicates whether or not the plauer is active
  , _cpsSuccessCallback :: Action ()
  -- ^ The function to call on success
  , _cpsFailureCallback :: Action ()
  -- ^ The function to call on failure
  }

-- | Goalie creation status
data CreateGoalieState = CreateGoalieState
  { _cgsNumber          :: Maybe Int
  -- ^ The goalie's number
  , _cgsName            :: String
  -- ^ The goalie's name
  , _cgsRookieFlag      :: Maybe Bool
  -- ^ Indicates whether or not the goalie is a rookie
  , _cgsActiveFlag      :: Maybe Bool
  -- ^ Indicates whether or not the goalie is active
  , _cgsSuccessCallback :: Action ()
  -- ^ The function to call on success
  , _cgsFailureCallback :: Action ()
  -- ^ The function to call on failure
  }

-- | Player edit status
data EditPlayerState = EditPlayerState
  { _epsSelectedPlayer :: Maybe Int
  -- ^ The index number of the player being edited
  , _epsMode           :: EditPlayerMode
  -- ^ The editing mode
  , _epsCallback       :: Action ()
  -- ^ The action to perform when the edit is complete
  }

-- | Player editing mode
data EditPlayerMode
  = EPMenu
  | EPNumber
  | EPName
  | EPPosition
  | EPYtd
  | EPLifetime
  | EPDelete
  | EPYtdGoals Bool
  | EPYtdAssists Bool
  | EPYtdPMin
  | EPLtGoals Bool
  | EPLtAssists Bool
  | EPLtPMin
  deriving (Eq, Show)

-- | 'Goalie' edit status
data EditGoalieState = EditGoalieState
  { _egsSelectedGoalie :: Maybe Int
  -- ^ The index number of the 'Goalie' being edited
  , _egsMode           :: EditGoalieMode
  -- ^ The editing mode
  , _egsCallback       :: Action ()
  -- ^ The action to perform when the edit is complete
  }

-- | 'Goalie' editing mode
data EditGoalieMode
  = EGMenu
  | EGNumber
  | EGName
  | EGYtd
  | EGLifetime
  | EGDelete
  | EGYtdGames    Bool
  | EGYtdMins     Bool
  | EGYtdGoals    Bool
  | EGYtdShutouts Bool
  | EGYtdWins     Bool
  | EGYtdLosses   Bool
  | EGYtdTies
  | EGLtGames     Bool
  | EGLtMins      Bool
  | EGLtGoals     Bool
  | EGLtShutouts  Bool
  | EGLtWins      Bool
  | EGLtLosses    Bool
  | EGLtTies
  deriving (Eq, Show)

-- | Represents the standings edit mode
data EditStandingsMode
  = ESMMenu
  | ESMHome ESMSubMode
  | ESMAway ESMSubMode
  deriving (Eq, Show)

-- | Represents the standings edit sub-mode
data ESMSubMode
  = ESMSubMenu
  | ESMEditWins
  | ESMEditLosses
  | ESMEditOvertime
  | ESMEditGoalsFor
  | ESMEditGoalsAgainst
  deriving (Eq, Show)

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

-- | Represents a (non-goalie) player
data Player = Player
  { _pNumber   :: Int
  -- ^ The player's number
  , _pName     :: String
  -- ^ The player's name
  , _pPosition :: String
  -- ^ The player's position
  , _pRookie   :: Bool
  -- ^ Indicates that the player is a rookie
  , _pActive   :: Bool
  -- ^ Indicates that the player is active
  , _pYtd      :: PlayerStats
  -- ^ The Player's year-to-date stats
  , _pLifetime :: PlayerStats
  -- ^ The player's lifetime stats
  } deriving (Eq, Show)

-- | Represents a (non-goalie) player's stats
data PlayerStats = PlayerStats
  { _psGoals   :: Int
  -- ^ The number of goals
  , _psAssists :: Int
  -- ^ The number of assists
  , _psPMin    :: Int
  -- ^ The number of penalty minutes
  } deriving (Eq, Show)

-- | Represents a goalie
data Goalie = Goalie
  { _gNumber   :: Int
  -- ^ The goalie's number
  , _gName     :: String
  -- ^ The goalie's name
  , _gRookie   :: Bool
  -- ^ Indicates that the goalie is a rookie
  , _gActive   :: Bool
  -- ^ Indicates that the goalie is active
  , _gYtd      :: GoalieStats
  -- ^ The goalie's year-to-date stats
  , _gLifetime :: GoalieStats
  -- ^ The goalie's lifetime stats
  } deriving (Eq, Show)

-- | Represents a goalie's stats
data GoalieStats = GoalieStats
  { _gsGames        :: Int
  -- ^ The number of games played
  , _gsMinsPlayed   :: Int
  -- ^ The number of minutes played
  , _gsGoalsAllowed :: Int
  -- ^ The number of goals allowed
  , _gsShutouts     :: Int
  -- ^ The number of shutouts the goalie has accumulated
  , _gsWins         :: Int
  -- ^ The number of wins
  , _gsLosses       :: Int
  -- ^ The number of losses
  , _gsTies         :: Int
  -- ^ The number of ties
  } deriving (Eq, Show)

-- | Game statistics
data GameStats = GameStats
  { _gmsWins         :: Int
  -- ^ Games won
  , _gmsLosses       :: Int
  -- ^ Games lost
  , _gmsOvertime     :: Int
  -- ^ Games lost in overtime
  , _gmsGoalsFor     :: Int
  -- ^ Goals for the team
  , _gmsGoalsAgainst :: Int
  -- ^ Goals against the team
  } deriving (Eq, Show)

-- | Defines a user prompt
data Prompt = Prompt
  { promptDrawer      :: ProgState -> C.Update ()
  -- ^ Draws the prompt to the screen
  , promptProcessChar :: Char -> String -> String
  -- ^ Modifies the string based on the character entered
  , promptAction      :: String -> Action ()
  -- ^ Action to perform when the value is entered
  , promptSpecialKey  :: C.Key -> Action ()
  -- ^ Action to perform when a special key is pressed
  }

-- | Parameters for a search prompt
data SelectParams a = SelectParams
  { spPrompt       :: String
  -- ^ The search prompt
  , spSearchHeader :: String
  -- ^ The header to display at the top of the search list
  , spSearch       :: String -> Database -> [(Int, a)]
  -- ^ The search function
  , spSearchExact  :: String -> Database -> Maybe Int
  -- ^ Search function looking for an exact match
  , spElemDesc     :: a -> String
  -- ^ Provides a string description of an element
  , spProcessChar  :: Char -> String -> String
  -- ^ Processes a character entered by the user
  , spCallback     :: Maybe Int -> Action ()
  -- ^ The function when the selection is made
  , spNotFound     :: String -> Action ()
  -- ^ The function to call when the selection doesn't exist
  }

-- | Describes a table cell
data TableCell
  = CellText String
  -- ^ A cell with text
  | CellFill Char
  -- ^ A cell filled with the given character
  deriving (Eq, Show)

makeLenses ''ProgState
makeLenses ''GameState
makeLenses ''CreatePlayerState
makeLenses ''CreateGoalieState
makeLenses ''EditPlayerState
makeLenses ''EditGoalieState
makeLenses ''Database
makeLenses ''Player
makeLenses ''PlayerStats
makeLenses ''Goalie
makeLenses ''GoalieStats
makeLenses ''GameStats

instance FromJSON Database where
  parseJSON = withObject "Database" $ \v -> Database
    <$> v .:? "players"         .!= []
    <*> v .:? "goalies"         .!= []
    <*> v .:? "games"           .!= 0
    <*> v .:? "home_game_stats" .!= newGameStats
    <*> v .:? "away_game_stats" .!= newGameStats

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

instance FromJSON Player where
  parseJSON = withObject "Player" $ \v -> Player
    <$> v .:  "number"
    <*> v .:  "name"
    <*> v .:  "position"
    <*> v .:? "rookie"   .!= False
    <*> v .:? "active"   .!= True
    <*> v .:? "ytd"      .!= newPlayerStats
    <*> v .:? "lifetime" .!= newPlayerStats

instance ToJSON Player where
  toJSON (Player num name pos rk act ytd lt) = object
    [ "number"   .= num
    , "name"     .= name
    , "position" .= pos
    , "rookie"   .= rk
    , "active"   .= act
    , "ytd"      .= ytd
    , "lifetime" .= lt
    ]
  toEncoding (Player num name pos rk act ytd lt) = pairs $
    "number"   .= num  <>
    "name"     .= name <>
    "position" .= pos  <>
    "rookie"   .= rk   <>
    "active"   .= act  <>
    "ytd"      .= ytd  <>
    "lifetime" .= lt

instance FromJSON PlayerStats where
  parseJSON = withObject "PlayerStats" $ \v -> PlayerStats
    <$> v .:? "goals"        .!= 0
    <*> v .:? "assists"      .!= 0
    <*> v .:? "penalty_mins" .!= 0

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

instance FromJSON Goalie where
  parseJSON = withObject "Goalie" $ \v -> Goalie
    <$> v .:  "number"
    <*> v .:  "name"
    <*> v .:? "rookie"   .!= False
    <*> v .:? "active"   .!= True
    <*> v .:? "ytd"      .!= newGoalieStats
    <*> v .:? "lifetime" .!= newGoalieStats

instance ToJSON Goalie where
  toJSON (Goalie num name rk act ytd lt) = object
    [ "number"   .= num
    , "name"     .= name
    , "ytd"      .= ytd
    , "rookie"   .= rk
    , "active"   .= act
    , "lifetime" .= lt
    ]
  toEncoding (Goalie num name rk act ytd lt) = pairs $
    "number"   .= num  <>
    "name"     .= name <>
    "rookie"   .= rk   <>
    "active"   .= act  <>
    "ytd"      .= ytd  <>
    "lifetime" .= lt

instance FromJSON GoalieStats where
  parseJSON = withObject "GoalieStats" $ \v -> GoalieStats
    <$> v .:? "games"         .!= 0
    <*> v .:? "mins_played"   .!= 0
    <*> v .:? "goals_allowed" .!= 0
    <*> v .:? "shutouts"      .!= 0
    <*> v .:? "wins"          .!= 0
    <*> v .:? "losses"        .!= 0
    <*> v .:? "ties"          .!= 0

instance ToJSON GoalieStats where
  toJSON (GoalieStats g m a s w l t) = object
    [ "games"         .= g
    , "mins_played"   .= m
    , "goals_allowed" .= a
    , "shutouts"      .= s
    , "wins"          .= w
    , "losses"        .= l
    , "ties"          .= t
    ]
  toEncoding (GoalieStats g m a s w l t) = pairs $
      "games"         .= g <>
      "mins_played"   .= m <>
      "goals_allowed" .= a <>
      "shutouts"      .= s <>
      "wins"          .= w <>
      "losses"        .= l <>
      "ties"          .= t

instance FromJSON GameStats where
  parseJSON = withObject "GameStats" $ \v -> GameStats
    <$> v .: "wins"
    <*> v .: "losses"
    <*> v .: "overtime"
    <*> v .: "goals_for"
    <*> v .: "goals_against"

instance ToJSON GameStats where
  toJSON (GameStats w l ot gf ga) = object
    [ "wins"          .= w
    , "losses"        .= l
    , "overtime"      .= ot
    , "goals_for"     .= gf
    , "goals_against" .= ga
    ]
  toEncoding (GameStats w l ot gf ga) = pairs $
    "wins"          .= w  <>
    "losses"        .= l  <>
    "overtime"      .= ot <>
    "goals_for"     .= gf <>
    "goals_against" .= ga

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

createGoalieStateL :: Lens' ProgMode CreateGoalieState
createGoalieStateL = lens
  (\case
    CreateGoalie cgs -> cgs
    _                -> newCreateGoalieState)
  (\_ cgs -> CreateGoalie cgs)

editPlayerStateL :: Lens' ProgMode EditPlayerState
editPlayerStateL = lens
  (\case
    EditPlayer eps -> eps
    _              -> newEditPlayerState)
  (\_ eps -> EditPlayer eps)

editGoalieStateL :: Lens' ProgMode EditGoalieState
editGoalieStateL = lens
  (\case
    EditGoalie egs -> egs
    _              -> newEditGoalieState)
  (\_ egs -> EditGoalie egs)

editStandingsModeL :: Lens' ProgMode EditStandingsMode
editStandingsModeL = lens
  (\case
    EditStandings esm -> esm
    _                 -> ESMMenu)
  (\_ esm -> EditStandings esm)

esmSubModeL :: Lens' EditStandingsMode ESMSubMode
esmSubModeL = lens
  (\case
    ESMMenu   -> ESMSubMenu
    ESMHome m -> m
    ESMAway m -> m)
  (\mode subMode -> case mode of
    ESMMenu   -> ESMMenu
    ESMHome _ -> ESMHome subMode
    ESMAway _ -> ESMAway subMode)

-- | Constructor for a 'ProgState'
newProgState :: ProgState
newProgState = ProgState
  { _database     = newDatabase
  , _progMode     = TitleScreen
  , _dbName       = ""
  , _inputBuffer  = ""
  , _scrollOffset = 0
  }

-- | Constructor for a 'GameState'
newGameState :: GameState
newGameState = GameState
  { _gameYear             = Nothing
  , _gameMonth            = Nothing
  , _gameDay              = Nothing
  , _gameType             = Nothing
  , _otherTeam            = ""
  , _homeScore            = Nothing
  , _awayScore            = Nothing
  , _overtimeFlag         = Nothing
  , _dataVerified         = False
  , _pointsAccounted      = 0
  , _goalBy               = Nothing
  , _assistsBy            = []
  , _gamePlayerStats      = M.empty
  , _confirmGoalDataFlag  = False
  , _gameSelectedPlayer   = Nothing
  , _gamePMinsRecorded    = False
  , _gameGoalieStats      = M.empty
  , _gameSelectedGoalie   = Nothing
  , _gameGoalieMinsPlayed = Nothing
  , _gameGoalsAllowed     = Nothing
  , _gameGoaliesRecorded  = False
  , _gameGoalieAssigned   = False
  }

-- | Constructor for a 'CreatePlayerState'
newCreatePlayerState :: CreatePlayerState
newCreatePlayerState = CreatePlayerState
  { _cpsNumber          = Nothing
  , _cpsName            = ""
  , _cpsPosition        = ""
  , _cpsRookieFlag      = Nothing
  , _cpsActiveFlag      = Nothing
  , _cpsSuccessCallback = return ()
  , _cpsFailureCallback = return ()
  }

-- | Constructor for a 'CreateGoalieState'
newCreateGoalieState :: CreateGoalieState
newCreateGoalieState = CreateGoalieState
  { _cgsNumber          = Nothing
  , _cgsName            = ""
  , _cgsRookieFlag      = Nothing
  , _cgsActiveFlag      = Nothing
  , _cgsSuccessCallback = return ()
  , _cgsFailureCallback = return ()
  }

-- | Constructor for an 'EditPlayerState'
newEditPlayerState :: EditPlayerState
newEditPlayerState = EditPlayerState
  { _epsSelectedPlayer = Nothing
  , _epsMode           = EPMenu
  , _epsCallback       = return ()
  }

-- | Constructor for an 'EditGoalieState' value
newEditGoalieState :: EditGoalieState
newEditGoalieState = EditGoalieState
  { _egsSelectedGoalie = Nothing
  , _egsMode           = EGMenu
  , _egsCallback       = return ()
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
  , _pRookie   = True
  , _pActive   = True
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
  , _gRookie   = True
  , _gActive   = True
  , _gYtd      = newGoalieStats
  , _gLifetime = newGoalieStats
  }

-- | Constructor for a 'GoalieStats' value
newGoalieStats :: GoalieStats
newGoalieStats = GoalieStats
  { _gsGames        = 0
  , _gsMinsPlayed   = 0
  , _gsGoalsAllowed = 0
  , _gsShutouts     = 0
  , _gsWins         = 0
  , _gsLosses       = 0
  , _gsTies         = 0
  }

-- | Constructor for a 'GameStats' value
newGameStats :: GameStats
newGameStats = GameStats
  { _gmsWins         = 0
  , _gmsLosses       = 0
  , _gmsOvertime     = 0
  , _gmsGoalsFor     = 0
  , _gmsGoalsAgainst = 0
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
  { _gmsWins         = s1^.gmsWins + s2^.gmsWins
  , _gmsLosses       = s1^.gmsLosses + s2^.gmsLosses
  , _gmsOvertime     = s1^.gmsOvertime + s2^.gmsOvertime
  , _gmsGoalsFor     = s1^.gmsGoalsFor + s2^.gmsGoalsFor
  , _gmsGoalsAgainst = s1^.gmsGoalsAgainst + s2^.gmsGoalsAgainst
  }

-- | Searches through a list of players with a specified criteria
playerSearchWith
  :: (Player -> Bool)
  -- ^ The search criteria
  -> String
  -- ^ The search string
  -> [Player]
  -- ^ The list of players to search
  -> [(Int, Player)]
  -- ^ The matching players with their index numbers
playerSearchWith criteria sStr =
  filter match . zip [0..]
  where
    match (_, p)
      =  map toUpper sStr `isInfixOf` map toUpper (p^.pName)
      && criteria p

-- | Searches through a list of players
playerSearch
  :: String
  -- ^ The search string
  -> [Player]
  -- ^ The list of players to search
  -> [(Int, Player)]
  -- ^ The matching players with their index numbers
playerSearch = playerSearchWith $ const True

-- | Searches through a list of players for an active player
activePlayerSearch
  :: String
  -- ^ The search string
  -> [Player]
  -- ^ The list of players to search
  -> [(Int, Player)]
  -- ^ The matching players with their index numbers
activePlayerSearch = playerSearchWith (^.pActive)

-- | Searches for a player by exact match on name
playerSearchExact
  :: String
  -- ^ The player's name
  -> [Player]
  -- ^ The list of players to search
  -> Maybe (Int, Player)
  -- ^ The player's index and value
playerSearchExact sStr =
  find match . zip [0..]
  where match (_, p) = p^.pName == sStr

-- | Modifies a player with a given name
modifyPlayer
  :: (Player -> Player)
  -- ^ The modification function
  -> String
  -- ^ The player's name
  -> [Player]
  -- ^ The list of players to modify
  -> [Player]
  -- ^ The modified list
modifyPlayer f n = map
  (\p -> if p^.pName == n
    then f p
    else p)

-- | Provides a short summary string for a player
playerSummary :: Player -> String
playerSummary p =
  p^.pName ++ " (" ++ show (p^.pNumber) ++ ") " ++ p^.pPosition

-- | Determines whether or not a player has been active in the current
-- season/year
playerIsActive :: Player -> Bool
playerIsActive = do
  stats <- (^.pYtd)
  return
    $  stats^.psGoals   /= 0
    || stats^.psAssists /= 0
    || stats^.psPMin    /= 0

-- | Calculates a player's points
psPoints :: PlayerStats -> Int
psPoints s = s^.psGoals + s^.psAssists

-- | Adds two 'PlayerStats' together
addPlayerStats :: PlayerStats -> PlayerStats -> PlayerStats
addPlayerStats s1 s2 = newPlayerStats
  & psGoals   .~ s1^.psGoals + s2^.psGoals
  & psAssists .~ s1^.psAssists + s2^.psAssists
  & psPMin    .~ s1^.psPMin + s2^.psPMin

-- | Searches a list of goalies with a search criteria
goalieSearchWith
  :: (Goalie -> Bool)
  -- ^ The search criteria
  -> String
  -- ^ The search string
  -> [Goalie]
  -- ^ The list to search
  -> [(Int, Goalie)]
  -- ^ The search results with their corresponding index numbers
goalieSearchWith criteria sStr =
  filter match . zip [0..]
  where
    match (_, g)
      =  map toUpper sStr `isInfixOf` map toUpper (g^.gName)
      && criteria g

-- | Searches a list of goalies
goalieSearch
  :: String
  -- ^ The search string
  -> [Goalie]
  -- ^ The list to search
  -> [(Int, Goalie)]
  -- ^ The search results with their corresponding index numbers
goalieSearch = goalieSearchWith $ const True

-- | Searches a list of goalies for an active goalie
activeGoalieSearch
  :: String
  -- ^ The search string
  -> [Goalie]
  -- ^ The list to search
  -> [(Int, Goalie)]
  -- ^ The search results with their corresponding index numbers
activeGoalieSearch = goalieSearchWith (^.gActive)

-- | Searches a list of goalies for an exact match
goalieSearchExact
  :: String
  -- ^ The search string
  -> [Goalie]
  -- ^ The list to search
  -> Maybe (Int, Goalie)
  -- ^ The result with its index number
goalieSearchExact sStr goalies = let
  results = filter (\(_, goalie) -> sStr == goalie^.gName) $
    zip [0..] goalies
  in case results of
    []       -> Nothing
    result:_ -> Just result

-- | Provides a description string for a 'Goalie'
goalieSummary :: Goalie -> String
goalieSummary g = g^.gName ++ " (" ++ show (g^.gNumber) ++ ")"

-- | Determines whether or not a goalie has been active in the current
-- season
goalieIsActive :: Goalie -> Bool
goalieIsActive g = g^.gYtd.gsMinsPlayed /= 0

-- | Adds two sets of 'GoalieStats'
addGoalieStats :: GoalieStats -> GoalieStats -> GoalieStats
addGoalieStats g1 g2 = GoalieStats
  { _gsGames        = g1^.gsGames        + g2^.gsGames
  , _gsMinsPlayed   = g1^.gsMinsPlayed   + g2^.gsMinsPlayed
  , _gsGoalsAllowed = g1^.gsGoalsAllowed + g2^.gsGoalsAllowed
  , _gsShutouts     = g1^.gsShutouts     + g2^.gsShutouts
  , _gsWins         = g1^.gsWins         + g2^.gsWins
  , _gsLosses       = g1^.gsLosses       + g2^.gsLosses
  , _gsTies         = g1^.gsTies         + g2^.gsTies
  }

-- | Determines a goalie's average goals allowed per game.
gsAverage :: GoalieStats -> Rational
gsAverage gs = let
  allowed = fromIntegral $ gs^.gsGoalsAllowed
  mins    = fromIntegral $ gs^.gsMinsPlayed
  gLen    = fromIntegral gameLength
  in if mins == 0
    then 0
    else allowed / mins * gLen
