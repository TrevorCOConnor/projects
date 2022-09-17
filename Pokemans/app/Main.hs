module Main where

import Data.Char
import System.Environment

import CallPokemon (callPokemon, callMove, DatabasePokemon (dbPokemonAbilities))
import PokemonDatabase

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

main :: IO ()
main = populateDatabase
