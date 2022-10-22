{-# LANGUAGE DeriveGeneric, ExistentialQuantification #-}
{- This Module houses the Models used in this app.  -}

module Models where

import           GHC.Generics
import Data.Binary (Binary)
import Control.Exception


{- Base Exception -}
data PokemanException = forall e . Exception e => PokemanException e

instance Show PokemanException where
    show (PokemanException e) = show e

instance Exception PokemanException


pokemanExceptionToException :: Exception e => e -> SomeException
pokemanExceptionToException = toException . PokemanException


pokemanExceptionFromException :: Exception e => SomeException -> Maybe e
pokemanExceptionFromException = fromException


{- Pokemon Ability datatype -}
data Ability = Ability
    { abilityName :: String
    , abilityText :: String }

instance Show Ability where
    show = abilityName


{- Data Type for holding pokemon movesets -}
data MoveSet = MoveSet
    { firstMove  :: Maybe Move
    , secondMove :: Maybe Move
    , thirdMove  :: Maybe Move
    , fourthMove :: Maybe Move
    }
    deriving (Show)


{- The various types of stats that a pokemon has in battle -}
data Stat = HP
          | Attack
          | Defense
          | SpecialAttack
          | SpecialDefense
          | Speed
          | Accuracy
          | Evasion
          deriving (Show, Generic)


{- Pokemon Stats -}
data PokemonStats = PokemonStats
    { hp             :: Int
    , attack         :: Int
    , defense        :: Int
    , specialAttack  :: Int
    , specialDefense :: Int
    , speed          :: Int
    } deriving (Generic, Show)
    
{- Autogenerate binary methods -}
instance Binary PokemonStats where


{- Pokemon Types -}
data Type = Normal
          | Fire
          | Water
          | Grass
          | Electric
          | Ice
          | Fighting
          | Poison
          | Ground
          | Flying
          | Psychic
          | Bug
          | Rock
          | Ghost
          | Dark
          | Dragon
          | Steel
          | Fairy
          deriving (Generic, Show)

instance Binary Type where


{- Pokemon Type Container -}
data PokemonTypes = SingleType Type
                  | DoubleType (Type, Type)
                  deriving (Generic, Show)

instance Binary PokemonTypes where


{- Data model for the response of the move endpoint for PokeApi -}
data Move = Move
     { moveName         :: String
     , moveAccuracy     :: Maybe Int
     , moveDamageClass  :: DamageClass
     , movePower        :: Maybe Int
     , movePP           :: Int
     , movePriority     :: Int
     , moveTarget       :: Target
     , moveType         :: Type
     , moveEffectText   :: String
     , moveEffectChance :: Maybe Int
     , moveMeta         :: MoveMeta
     , moveStatChanges  :: [StatChange]
     }

instance Show Move where
    show = moveName

{- Types of Damage -}
data DamageClass = Physical
                 | Status
                 | Special
                 deriving (Show, Generic)


{- Meta Data for Pokemon Moves -}
data MoveMeta = MoveMeta
    { moveMetaCategory     :: MoveCategory
    , moveMetaCritRate     :: Int
    , moveMetaDrain        :: Int
    , moveMetaFlinchChance :: Int
    , moveMetaHealing      :: Int
    , moveMetaMaxHits      :: Maybe Int
    , moveMetaMaxTurns     :: Maybe Int
    , moveMetaMinHits      :: Maybe Int
    , moveMetaMinTurns     :: Maybe Int
    , moveMetaStatChance   :: Maybe Int
    }
    deriving (Show, Generic)


{- Types of Targets for moves -}
data Target = SpecificMove
            | SelectedPokemonMeFirst
            | Ally
            | UsersField
            | UserOrAlly
            | OpponentsField
            | User
            | RandomOpponent
            | AllOtherPokemon
            | SelectedPokemon
            | AllOpponents
            | EntireField
            | UserAndAllies
            | AllPokemon
            | AllAllies
            deriving (Show, Generic)


{-
    All possible move categories which can be used to dynamically build moves
    for any value other than "Unique"
-}
data MoveCategory = Damage
                  | Ailment
                  | NetGoodStats
                  | Heal
                  | DamageAilment
                  | Swagger
                  | DamageLower
                  | DamageRaise
                  | DamageHeal
                  | OHKO
                  | WholeFieldEffect
                  | FieldEffect
                  | ForceSwitch
                  | Unique
                  deriving (Show, Generic)


{- Stat change data structure -}
data StatChange = StatChange
    { statChangeStages :: Int
    , statChangeStat   :: Stat
    }
    deriving (Show, Generic)


{- Generic Pokemon object to be stored in database -}
data DatabasePokemon = DatabasePokemon
    { dbPokemonName :: String
    , dbPokemonIndex :: Int
    }
