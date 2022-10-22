module LoadPokemon where

-- Hackage
import           System.Random
import Control.Monad.Trans.Except (ExceptT (..), except)
import Safe
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (nonEmpty)
import Prelude hiding (log)

-- Local Modules
import qualified CallPokemon     as CP
import qualified Models          as M
import           PokemonDatabase (loadPokemon, PokemanDatabaseException)
import CallPokemon (DatabasePokemon(dbPokemonMoves), PokemansCallException)
import PokemonLogger


-- Constants
hiddenAbilityChance :: Float
hiddenAbilityChance = 0.01


-- Datatypes
{- Base Pokemon datatype -}
data Pokemon = Pokemon
    { pokemonName     :: String
    , pokemonNickname :: Maybe String
    , pokemonAbility  :: M.Ability
    , pokemonTypes    :: M.PokemonTypes
    , pokemonStats    :: M.PokemonStats
    , pokemonMoveSet  :: M.MoveSet
    , pokemonLevel    :: Level
    }
    deriving (Show)


{- Container for level to help protect against negative values -}
newtype Level = Level { unLevel :: Int }

instance Show Level where
    show = show . unLevel

type Id = Int


-- Functions
{- Converts Response to Caught Pokemon -}
loadPokemonAtLevel :: Level -> Id -> ExceptT M.PokemanException IO Pokemon
loadPokemonAtLevel level id = do
    lift $ logString INFO "Loading pokemon from DB..."
    dbPokemon <- loadPokemon id
    lift $ logString INFO "Loaded pokemon from DB."
    lift $ logString INFO "Calling ability..."
    ability <- chooseAbility (CP.dbPokemonAbilities dbPokemon)
    lift $ logString INFO "Ability Received."
    lift $ logString INFO "Calling moves..."
    moves <- getMovesAtLevel level dbPokemon
    lift $ logString INFO "Moves received."
    return Pokemon
        { pokemonName=CP.dbPokemonName dbPokemon
        , pokemonLevel=level
        , pokemonNickname=Nothing
        , pokemonAbility=ability
        , pokemonTypes=CP.dbPokemonTypes dbPokemon
        , pokemonStats=CP.dbPokemonStats dbPokemon
        , pokemonMoveSet=moves
        }


-- Helpers
{- Converts the array of response abilities to a single ability. -}
chooseAbility :: [CP.ResponsePokemonAbility] -> ExceptT M.PokemanException IO M.Ability
chooseAbility abilities = do
        lift $ log INFO abilities
        lift $ logString INFO "Generating random ... "
        random <- getStdRandom (randomR (0.0, 1.0))
        lift $ logString INFO "Random generated."
        lift $ logString INFO "Choosing abilitiy..."
        let hiddenAbilities = filter CP.responsePokemonAbilityIsHidden abilities
        let nonHiddenAbilities = filter (not . CP.responsePokemonAbilityIsHidden) abilities
        chosenAbility <- if random <= hiddenAbilityChance
               then do
                   lift $ logString INFO "Picking hidden ability if possible."
                   return $ head $ hiddenAbilities ++ nonHiddenAbilities
               else do
                   lift $ logString INFO "Picking non hidden ability..."
                   lift $ logString INFO "Picking index..."
                   index <- getStdRandom $ randomR (0, length nonHiddenAbilities - 1)
                   lift $ log INFO index
                   lift $ logString INFO "Picking non hidden ability"
                   lift $ log INFO $ length nonHiddenAbilities
                   let hiddenAbility = (nonHiddenAbilities ++ nonHiddenAbilities) !! max index 0
                   return hiddenAbility
        lift $ logString INFO "Ability chosen."
        ability <- CP.callAbility $ CP.responsePokemonAbilityUrl chosenAbility
        return M.Ability { M.abilityName=CP.responseAbilityName ability
                         , M.abilityText=CP.responseAbilityEffect ability
                         }


{- Retrieves up to last 4 pokemon moves for pokemon that are learned at or before given level. -}
getMovesAtLevel :: Level -> CP.DatabasePokemon -> ExceptT M.PokemanException IO M.MoveSet
getMovesAtLevel level dbPokemon = do
    -- Filter moves to moves learned at level or lower, and reverse order to get latest moves first
    let leveledMoves = reverse $ filter (_moveLearnedAtOrBeforeLevel level) (CP.dbPokemonMoves dbPokemon)
    moveResponses <- mapM (CP.callMove . CP.responseMoveUrl) $ take 4 leveledMoves
    return $ M.MoveSet (moveResponses `atMay` 0)
                       (moveResponses `atMay` 1)
                       (moveResponses `atMay` 2)
                       (moveResponses `atMay` 3)


{- Helper function: Checks that response move can be learned at level or earlier. -}
_moveLearnedAtOrBeforeLevel :: Level -> CP.ResponseMove -> Bool
_moveLearnedAtOrBeforeLevel (Level lvl1) move =
    case CP.responseMoveLearnMethod move of
      (CP.LevelUp lvl2) -> lvl2 <= lvl1
      _ -> False


{- Converts a list of possible moves from a pokemon to a moveset. -}
caughtMoves :: [CP.ResponseMove] -> M.MoveSet
caughtMoves = undefined
