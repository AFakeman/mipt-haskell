import System.Environment
import Control.Exception
import System.Exit
import System.IO.Error
import Data.List
import Data.Ord
import Data.Either


data Args = Args {
    argInputFile :: String,
    argOutputFile :: String,
    argSortOrder :: Bool
}

main :: IO ()
main = getArgs >>= processArgs >>= mainWithArgs

processArgs :: [String] -> IO Args
processArgs xs = case parseArgs xs of
                     Right a -> return a
                     Left s -> putStrLn s >>
                               usage "task42" >>
                               exitWith ExitSuccess

parseArgs :: [String] -> Either String Args
parseArgs xs =
    if length xs == 3
        then let args = Args (xs !! 0) (xs !! 1) in
                 case parseSortOrder $ xs !! 2 of
                     Right order -> Right $ args order
                     Left err -> Left err
        else Left "Incorrect number of arguments"

parseSortOrder :: String -> Either String Bool
parseSortOrder "asc" = Right True
parseSortOrder "desc" = Right False
parseSortOrder _ = Left "Invalid ordering parameter"

usage :: String -> IO ()
usage name = putStrLn $ "Usage: " ++ name ++ "<infile> <outfile> <asc/desc>"

printArgs :: Args -> IO ()
printArgs a = putStrLn (argInputFile a) >>
              putStrLn (argOutputFile a) >>
              putStrLn (show $ argSortOrder a)

sortAscDesc :: Bool -> [String] -> [String]
sortAscDesc True = sort
sortAscDesc False = sortBy (flip compare)

handleIOError :: (IOErrorType -> String) -> IOError -> IO (a)
handleIOError f e = die msg
    where code = ioeGetErrorType e
          msg  = f code

handleInputErrorMessage :: IOErrorType -> String
handleInputErrorMessage e
    |  isPermissionErrorType e = "Can't access input file due to permissions"
    |  isDoesNotExistErrorType e = "Input file does not exist"
    |  isIllegalOperationErrorType e = "Wait. That's illegal."
    |  otherwise = show e

handleOutputErrorMessage :: IOErrorType -> String
handleOutputErrorMessage e
    |  isPermissionErrorType e = "Can't access output file due to permissions"
    |  isIllegalOperationErrorType e = "Wait. That's illegal."
    |  isFullErrorType e = "Can't write output due to the lack of space on device."
    |  otherwise = show e

readInputFile :: Args -> IO String
readInputFile args = catch (readFile $ argInputFile args)
                           (handleIOError handleInputErrorMessage)

writeOutputFile :: Args -> String -> IO ()
writeOutputFile args x = catch (writeFile outFile x)
                          (handleIOError handleOutputErrorMessage)
    where outFile = argOutputFile args

mainWithArgs :: Args -> IO ()
mainWithArgs a = (readInputFile a) >>=
                 (return . lines) >>=
                 (return . (sortAscDesc (argSortOrder a))) >>=
                 (return . unlines) >>=
                 (writeOutputFile a)
