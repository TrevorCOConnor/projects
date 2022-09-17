{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CallPokemon where

import           Data.Aeson
import qualified Data.Aeson.KeyMap    as KM
import           Data.Aeson.Types
import           Data.Binary          (Binary)
import qualified Data.ByteString      as BS
import           Data.Char
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Data.Tuple.Sequence  (sequenceT)
import qualified Data.Vector          as V
import           GHC.Generics
import           Network.HTTP.Request
import qualified Text.Casing          as C

-- Local Modules
import qualified Models               as M


-- Helpers
modifyText :: (String -> String) -> T.Text -> T.Text
modifyText mod = T.pack . mod . T.unpack


-- Response Datatypes
{- Data model for retrieving pokemon from the PokeApi -}
data DatabasePokemon = DatabasePokemon
    { dbPokemonName      :: String
    , dbPokemonId        :: Int
    , dbPokemonAbilities :: [ResponsePokemonAbility]
    , dbPokemonStats     :: M.PokemonStats
    , dbPokemonTypes     :: M.PokemonTypes
    , dbPokemonMoves     :: [ResponseMove]
    , dbPokemonWeight    :: Int
    } deriving (Show, Generic)

{- Method for creating PokemonResponse from PokeApi. -}
instance FromJSON DatabasePokemon where
    parseJSON = withObject "DatabasePokemon" $ \v -> DatabasePokemon
        -- responsePokemonName
        <$> v .: "name"
        -- responsePokemonId
        <*> v .: "id"
        -- responsePokemonAbilities
        <*> v .: "abilities"
        -- responsePokemonStats
        <*> v .: "stats"
        -- responsePokemonTypes
        <*> v .: "types"
        -- responsePokemonMoves
        <*> v .: "moves"
        -- responsePokemonWeight
        <*> v .: "weight"

{- Autogenerate decode and encode values -}
instance Binary DatabasePokemon where


{- Data model for retrieving abilities from the PokeApi -}
data ResponsePokemonAbility = ResponsePokemonAbility
    { responsePokemonAbilityName     :: String
    , responsePokemonAbilityIsHidden :: Bool
    , responsePokemonAbilityUrl      :: String
    } deriving (Show, Generic)


{- Method for creating ResponsePokemonAbility from the PokeAPi response. -}
instance FromJSON ResponsePokemonAbility where
    parseJSON = withObject "ResponsePokemonAbility" $ \v -> ResponsePokemonAbility
        -- responsePokemonAbilityName
        <$> ((v .: "ability") >>= (.: "name"))
        -- responsePokemonAbilityIsHidden
        <*> v .: "is_hidden"
        -- responsePokemonAbilityUrl
        <*> ((v .: "ability") >>= (.: "url"))

{- Autogenerate decode and encode values -}
instance Binary ResponsePokemonAbility where


{- Data model for retrieving abilities from the ability endpoint. -}
data ResponseAbility = ResponseAbility
    { responseAbilityName   :: String
    , responseAbilityEffect :: String
    } deriving (Show, Generic)

{- Method for creating ResponseAbility from the PokeAPi response. -}
instance FromJSON ResponseAbility where
    parseJSON = withObject "ResponseAbility" $ \v -> ResponseAbility
        -- responseAbilityName
        <$> v .: "name"
        -- responseAbilityEffect
        <*> ((v .: "effect_entries")
                >>= withArray "effect_entries" (parseJSON . V.head . V.filter effectFilter))
                    where effectFilter :: Value -> Bool
                          effectFilter v = fromMaybe False $ do
                              let parser = withObject "effect_entry" $
                                      \v -> (v .: "language") >>= (.: "name") :: Parser String
                              name <- parseMaybe parser v
                              return $ name == "en"

{- Autogenerate decode and encode values -}
instance Binary ResponseAbility where


-- FromJSON for Models
instance FromJSON M.PokemonStats where
    parseJSON = withArray "PokemonBaseStats" $ \v -> M.PokemonStats
        <$> withObject "hp" (.: "base_stat") (v V.! 0)
        <*> withObject "attack" (.: "base_stat") (v V.! 1)
        <*> withObject "defense" (.: "base_stat") (v V.! 2)
        <*> withObject "special attack" (.: "base_stat") (v V.! 3)
        <*> withObject "special defense" (.: "base_stat") (v V.! 4)
        <*> withObject "speed" (.: "base_stat") (v V.! 5)


instance FromJSON M.PokemonTypes where
    parseJSON = withArray "PokemonTypes" $ \v ->
        if V.length v == 1
           then M.SingleType <$> extractType ( v V.! 0)
           else M.DoubleType <$> sequenceT ( extractType $ v V.! 0
                                         , extractType $ v V.! 1
                                         )
        where extractType = withObject "TypeObject" $
                \v -> (v .: "type")
                      >>= (.: "name")
                      >>= withText "TypeName"
                              (parseJSON . String . modifyText C.pascal)


{- Autogenerated from deriving Generics -}
instance FromJSON M.Type where


data ResponseMove = ResponseMove
    { responseMoveName        :: String
    , responseMoveUrl         :: String
    , responseMoveLearnMethod :: MoveLearnMethod
    }
    deriving (Show, Generic)

instance FromJSON ResponseMove where
    parseJSON = withObject "ResponseMove" $ \v -> ResponseMove
        <$> ((v .: "move") >>= (.: "name"))
        <*> ((v .: "move") >>= (.: "url"))
        <*> ((v .: "version_group_details") >>=
            withArray "version_group_details" (parseJSON . V.last))

instance Binary ResponseMove where


data MoveLearnMethod = LevelUp {level :: Int}
                     | Machine
                     | Tutor
                     | Egg
                     deriving (Show, Generic, Eq)

instance FromJSON MoveLearnMethod where
    parseJSON = withObject "MoveLearnMethod" parseObj
        where parseObj obj = do
                respMethodName <- (obj .: "move_learn_method") >>= (.: "name")
                let methodName = respMethodName :: String
                level <- obj .: "level_learned_at"
                let result
                      | methodName == "level-up" = LevelUp {level=level}
                      | methodName == "machine" = Machine
                      | methodName == "tutor" = Tutor
                      | methodName == "egg" = Egg
                      | otherwise = Tutor
                return result

instance Binary MoveLearnMethod where


{- Set method for converting moves to Haskell objects. -}
instance FromJSON M.Move where
    parseJSON = withObject "Move" $ \v -> M.Move
        -- moveName
        <$> v .: "name"
        -- moveAccuracy
        <*> v .: "accuracy"
        -- moveDamageClass
        <*> ((v .: "damage_class") >>= (.: "name") >>= (parseJSON . String . modifyText C.pascal))
        -- movePower
        <*> v .: "power"
        -- movePP
        <*> v .: "pp"
        -- movePriority
        <*> v .: "priority"
        -- moveTarget
        <*> ((v .: "target") >>= (.: "name")
                             >>= (parseJSON . String . modifyText C.pascal))
        -- moveType
        <*> ((v .: "type") >>= (.: "name")
                           >>= (parseJSON . String . modifyText C.pascal))
        -- moveEffectText
        -- Need to filter by language and grab the last entry
        <*> ((v .: "flavor_text_entries") >>= (parseJSON . V.head)
                                          >>= (.: "flavor_text"))
        -- moveEffectChance
        <*> v .: "effect_chance"
        -- moveMeta
        <*> v .: "meta"
        -- moveStatChanges
        <*> v .: "stat_changes"


{- Use auto FromJSON for DamageClass -}
instance FromJSON M.DamageClass where


instance FromJSON M.MoveMeta where
    parseJSON = withObject "MoveMeta" $ \v -> M.MoveMeta
        <$> ((v .: "category") >>= (.: "name")
            >>= withText "category.name" (parseJSON . String . modifyText C.pascal))
        <*> (v .: "crit_rate")
        <*> (v .: "drain")
        <*> (v .: "flinch_chance")
        <*> (v .: "healing")
        <*> (v .: "max_hits")
        <*> (v .: "max_turns")
        <*> (v .: "min_hits")
        <*> (v .: "min_turns")
        <*> (v .: "stat_chance")


instance FromJSON M.StatChange where
    parseJSON = withObject "StatChange" $ \v -> M.StatChange
        <$> v .: "stages"
        <*> v .: "stat"


instance FromJSON M.MoveCategory where


instance FromJSON M.Target where


instance FromJSON M.Stat where


-- Functions
{- Sets the URL for calling the pokemon api -}
pokemonBaseUrl :: String
pokemonBaseUrl = "https://pokeapi.co/api/v2/pokemon"


{-
   Attempts to retreive pokemon data by name. Returns Nothing if something goes
   wrong.
   E.g. callPokemon "pikachu" -> ResponsePokemon
-}
callPokemon :: String -> IO (Either String DatabasePokemon)
callPokemon str = callPokemonFromUrl $ pokemonBaseUrl ++ '/':str


callPokemonFromUrl :: String -> IO (Either String DatabasePokemon)
callPokemonFromUrl url = do
    resp <- get url
    if responseStatus resp == 200
       then return $ eitherDecodeStrict $ responseBody resp
       else return $ Left $ show $ responseBody resp


data PokemonQueryResult = PokemonQueryResult
    { name :: String , url :: String }
    deriving (Show, Generic)

instance FromJSON PokemonQueryResult where


queryPokemon :: IO (Either String [PokemonQueryResult])
queryPokemon = do
    resp <- get $ pokemonBaseUrl ++ "?limit=10000"
    if responseStatus resp == 200
       then return $ do 
           body <- eitherDecodeStrict $ responseBody resp :: Either String Object
           parseEither (.: "results") body
       else return $ Left $ show $ responseBody resp


{- Attempts to get move information by using move url -}
callMove :: String -> IO (Either String M.Move)
callMove url = do
    resp <- get url
    if responseStatus resp == 200
       then return $ eitherDecodeStrict $ responseBody resp
       else return $ Left $ show $ responseBody resp


{- TODO -}
callAbility :: String -> ResponseAbility
callAbility = undefined
