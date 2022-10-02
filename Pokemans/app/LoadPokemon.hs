module LoadPokemon where

-- Hackage
import           System.Random

-- Local Modules
import Control.Monad.Trans.Except (ExceptT (..), except)
import qualified CallPokemon     as CP
import qualified Models          as M
import           PokemonDatabase (loadPokemon, PokemanDatabaseException)
import CallPokemon (DatabasePokemon(dbPokemonMoves), PokemansCallException)
import Safe


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


{- Container for level to help protect against negative values -}
newtype Level = Level { unLevel :: Int }

type Id = Int


-- Functions
{- Converts Response to Caught Pokemon -}
loadPokemonAtLevel :: Level -> Id -> ExceptT PokemanDatabaseException IO Pokemon
loadPokemonAtLevel level id = do
    dbPokemon <- loadPokemon id
    let ability = chooseAbility (CP.dbPokemonAbilities dbPokemon)
    moves <- getMovesAtLevel level dbPokemon
    case loaded of
      Left err -> return $ Left err
      Right dbPokemon -> do
        ability <- chooseAbility (CP.dbPokemonAbilities dbPokemon)
        eitherMoves <- getMovesAtLevel level dbPokemon
        return $ flip fmap eitherMoves $ \moves -> Pokemon
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
chooseAbility :: [CP.ResponsePokemonAbility] -> IO M.Ability
chooseAbility abilities = do
        -- random <- getStdRandom (randomR (0.0, 1.0))
        -- let chosenAbility = if random <= hiddenAbilityChance
        --        then head $ filter CP.responsePokemonAbilityIsHidden abilities
        --        else undefined
        -- let ability = M.Ability { M.abilityName=CP.responsePokemonAbilityName chosenAbility
        --                         , M.abilityText=CP.responseAbilityEffect chosenAbility
        --                         }
        undefined


{- Retrieves up to last 4 pokemon moves for pokemon that are learned at given level. -}
getMovesAtLevel :: Level -> CP.DatabasePokemon -> ExceptT PokemansCallException IO M.MoveSet
getMovesAtLevel level dbPokemon = do
    let leveledMoves = reverse $ filter ((== CP.LevelUp (unLevel level)) . CP.responseMoveLearnMethod) (CP.dbPokemonMoves dbPokemon)
    moveResponses <- mapM (CP.callMove . CP.responseMoveUrl) leveledMoves
    let eitherMoveList = sequence moveResponses
    return $ flip fmap eitherMoveList $ \moveList ->
        M.MoveSet (moveList `atMay` 0)
                  (moveList `atMay` 1)
                  (moveList `atMay` 2)
                  (moveList `atMay` 3)


{- Converts a list of possible moves from a pokemon to a moveset. -}
caughtMoves :: [CP.ResponseMove] -> M.MoveSet
caughtMoves = undefined
