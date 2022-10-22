module Main where

import Data.Char
import System.Environment
import Control.Monad
import Control.Monad.Trans.Except

import CallPokemon (callPokemon, callMove, DatabasePokemon (dbPokemonAbilities))
import LoadPokemon (loadPokemonAtLevel, Level (..))
import PokemonDatabase
import PokemonLogger (clearLogs)

-- main :: IO ()
-- main = do
--     args <- getArgs         -- IO[String]
--     do if null args
--           then putStrLn "Missing argument: pokemon-name"
--           else do
--               pokemon <- callPokemon (map toLower . head $ args)
--               case pokemon of
--                 Left str -> print str
--                 Right p -> print $ dbPokemonAbilities p

-- main :: IO ()
-- main = populateDatabase

main :: IO ()
main = do
    args <- getArgs
    clearLogs
    when (null args) $ putStrLn "Missing arguments: pokemon id, level"
    when (length args == 1) $ putStrLn "Missing argument: level"
    when (length args > 2) $ putStrLn "Too many arguments. Expected two: pokemon_id, level"
    when (length args == 2) $ do 
        let response = loadPokemonAtLevel (Level $ read $ args !! 1) (read $ head args)
        result <- runExceptT response
        print result
