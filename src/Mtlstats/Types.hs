{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Mtlstats.Types (
  -- * Types
  Player (..),
  PlayerStats (..),
  -- * Lenses
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
  -- * Constructors
  newPlayer,
  newPlayerStats,
  -- * Helper functions
  pPoints
) where

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
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

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

makeLenses ''Player
makeLenses ''PlayerStats

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

-- | Calculates a player's points
pPoints :: PlayerStats -> Int
pPoints s = s^.psGoals + s^.psAssists
