{-# LANGUAGE ViewPatterns #-}
module Args (parseArgs, OptionMonad) where

import qualified Ether.State as I
import qualified Ether.Except as I
import qualified Ether.Reader as I
import System.Linux.Kvm.Components.Init
import System.Linux.Kvm.Components.Console
import System.Linux.Kvm.Components.Ram
import Data.List
import Text.Read
import Control.Monad
import System.Environment

type OptionMonad = RamT (ConsoleT (InitT IO))

data Options = Options
    { file :: String
    , root :: (Maybe String)
    , initrd :: (Maybe String)
    , console :: (Maybe String)
    , ram :: (Maybe Int) }

data Help = Help


type OptionState = I.StateT' Options (I.ExceptT' String (I.Except' Help))
contParse :: [String] -> OptionState ()
contParse [] = return ()
contParse (("-h"):_) = I.throw' Help
contParse (("-m"):ram:xs) = maybe
                                (I.throw' "cannot parse ram amount")
                                (\r -> I.modify' (\x -> x{ram=Just r}))
                                (readMaybe ram)
contParse (("--initrd"):initrd:xs) = I.modify' (\x -> x{ initrd=Just initrd }) >> contParse xs

contParse ((stripPrefix "root=" -> Just rootVal):xs) = I.modify' (\x -> x{ root=Just rootVal }) >> contParse xs
contParse ((stripPrefix "console=" -> Just console):xs) = I.modify' (\x -> x{ console=Just console }) >> contParse xs
contParse (x:xs) = do
                    curr <- I.gets' file
                    when (curr /= "") $ I.throw' $ "too many files provided : " ++ show [curr, x]
                    I.modify' (\opt -> opt{ file=x }) >> contParse xs

emptyOpts:: Options
emptyOpts = Options { file="", root=Nothing, initrd=Nothing, console=Nothing, ram=Nothing }

doParse:: [String] -> Either Help (Either String Options)
doParse args = I.runExcept' $ I.runExceptT' $ I.execStateT' (contParse args) emptyOpts

showHelp:: IO ()
showHelp = putStrLn "TODO : help"

showErr:: String -> IO ()
showErr err = putStrLn $ "arg parsing failed : " ++ err -- TODO : better error message

continueWithOpts:: Options -> OptionMonad () -> IO ()
continueWithOpts (Options kern root initrd console ram) m = runInit kern initrd $ runConsole console $ runRam ram m

parseArgs :: OptionMonad () -> IO ()
parseArgs continue = doParse <$> getArgs >>= \x -> (case x of
                                                     (Left Help) -> showHelp
                                                     (Right (Left err)) -> showErr err
                                                     (Right (Right opts)) | file opts == "" -> showErr "no file provided"
                                                     (Right (Right opts)) -> continueWithOpts opts continue)

