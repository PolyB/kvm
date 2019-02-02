{-# LANGUAGE ViewPatterns #-}
module Args (parseArgs, OptionMonad) where

import System.Linux.Kvm.Components.Init
import System.Linux.Kvm.Components.Ram
import Data.List
import Options.Applicative

type OptionMonad = ConfigRamT (InitT IO)

data Options = Options
    { file :: String
    , initrd :: (Maybe String)
    , ram :: (Maybe Int)
    , cmdline :: [String]
    }

opts::Parser Options
opts = Options
        <$> argument str (metavar "FILE")
        <*> optional (strOption (long "initrd"))
        <*> optional (option auto (long "ram" <> short 'm' <> metavar "BYTECOUNT"))
        <*> many (argument str (metavar "cmdline..."))

continueWithOpts:: Options -> OptionMonad () -> IO ()
continueWithOpts (Options kern initrd ram cmdline) m = runInit kern initrd (intercalate " " cmdline) $  runConfigRam ram m

parseArgs :: OptionMonad () -> IO ()
parseArgs continue = execParser options >>= \opt -> continueWithOpts opt continue
                    where options = info (opts <**> helper)
                                        (fullDesc
                                        <> progDesc "kvm"
                                        <> header "kvm")
