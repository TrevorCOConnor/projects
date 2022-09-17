module PokemonDatabase where

-- Standard Haskell
import           Control.Monad
import qualified Control.Monad.Parallel as MP
import           Data.Binary
import           Data.Binary.Get  (ByteOffset)
import           System.Directory
import           System.FilePath  ((-<.>), (</>))
import           System.IO

-- Local Modules
import qualified CallPokemon      as CP


-- Constants
dbDirectory :: FilePath
dbDirectory = "PokemonDB"


-- Data
data ProcessStatus = Complete String
                   | Incomplete
                       { name   :: String
                       , fails  :: Int
                       , errors :: [String]
                       }
                   | Fatal String String
                   deriving (Show)



storePokemon :: CP.DatabasePokemon -> IO ()
storePokemon dbPokemon = do
    let fileName = dbDirectory </> show (CP.dbPokemonId dbPokemon) -<.> "txt"
    putStrLn $ "Creating " ++ fileName
    encodeFile fileName dbPokemon


loadPokemon :: Int -> IO (Either String CP.DatabasePokemon)
loadPokemon pokemonId = do
    let fileName = dbDirectory </> show pokemonId -<.> "txt"
    result <- decodeFileOrFail fileName
    case result of
      Left (_, err) -> return $ Left err
      Right p -> return $ Right p


downloadPokemon :: ProcessStatus -> String -> IO ProcessStatus
downloadPokemon (Incomplete name fails errs) url = do
    resp <- CP.callPokemonFromUrl url
    putStrLn "Pokemon response received"
    case resp of
      Left err -> do
          putStrLn err
          return $ Incomplete name (fails + 1) (err:errs)
      Right dbPokemon -> do
        putStrLn "Success"
        storePokemon dbPokemon
        loadable <- loadPokemon $ CP.dbPokemonId dbPokemon
        case loadable of
          Left err -> return $ Fatal name err
          Right _  -> return $ Complete name
downloadPokemon status _ = return status


getAllPokemon :: IO [(String, String)]
getAllPokemon = undefined


setPokemonToInProgress :: CP.PokemonQueryResult -> (ProcessStatus, String)
setPokemonToInProgress result = (Incomplete (CP.name result) 0 [], CP.url result)


populateDatabase :: IO ()
populateDatabase = do
    dirExists <- doesDirectoryExist dbDirectory
    unless dirExists $ createDirectory dbDirectory
    pokemonUrls <- CP.queryPokemon
    putStrLn "Urls retrieved"
    case pokemonUrls of
      Left err -> putStrLn err
      Right urls -> do
        let todos = map setPokemonToInProgress urls
        putStrLn "Processing urls"
        results <- MP.mapM (uncurry downloadPokemon) todos
        let (complete, incomplete, fatal) = foldl (\(complete, incomplete, fatal) status ->
                case status of
                  Complete {}   -> (status:complete, incomplete, fatal)
                  Incomplete {} -> (complete, status:incomplete, fatal)
                  Fatal {}      -> (complete, incomplete, status:fatal)
                )
                ([], [], []) results
        if null incomplete && null fatal
           then putStrLn "Success!"
           else do
               withFile "etl_errors.txt" WriteMode $ \h -> do
                   hPutStrLn h $ "Completed: " ++ show (length complete)
                   hPutStrLn h $ "Incomplete: " ++ show (length incomplete)
                   hPutStrLn h $ "Fatal Fails: " ++ show (length fatal)
                   mapM_ (hPrint h) complete
                   mapM_ (hPrint h) incomplete
                   mapM_ (hPrint h) fatal
                   putStrLn $ "Errors: " ++ show (length incomplete + length fatal)
