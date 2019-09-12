-----------------------------------------------------------------------------
--
-- Module      :  HReg.CLI
-- Copyright   :
-- License     :  BSD-3-Clause
--
-- Maintainer  :  omgbebebe@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE ApplicativeDo   #-}
module HReg.CLI (
    hregCli
) where

import Data.Version (Version, showVersion)
import Options.Applicative (Parser, ParserInfo, ParserPrefs, argument, command, customExecParser,
                            flag, flag', fullDesc, help, helpLongEquals, helper, info, infoFooter,
                            infoHeader, infoOption, long, maybeReader, metavar, option, optional,
                            prefs, progDesc, short, showHelpOnEmpty, strArgument, strOption,
                            subparser, subparserInline, switch, value)
import Options.Applicative.Help.Chunk (stringChunk)
import HReg.Types
import qualified HReg
import qualified HReg.Regfiles as HReg
import qualified Paths_hreg as Meta (version)

-- | Represents all command
data Command
    -- | @export@ command create .reg file with exported branch
    = Export ExportOpts
    -- | @import@ command import records from a .reg file and write it to a filesystem
    | Import ImportOpts
    -- | @apply@ command parse .pol files and modify registry
    | Apply ApplyOpts
    -- | @diff@ command compare two registries and creates resulting .pol file
    | Diff DiffOpts

data ExportOpts = ExportOpts
    { exportOptsPath    :: FilePath
    , exportOptsOutFile :: FilePath
    , exportOptsRoot    :: FilePath
    }

data ImportOpts = ImportOpts
    { importOptsInFile  :: FilePath
    , importOptsRoot    :: FilePath
    }

data ApplyOpts = ApplyOpts
    { applyOptsInFile :: FilePath
    , applyOptsScope  :: Scope
    , applyOptsRoot   :: FilePath
    }

data DiffOpts = DiffOpts
    { diffOptsPathA   :: FilePath
    , diffOptsPathB   :: FilePath
    , diffOptsOutFile :: FilePath
    }

hregCli :: IO ()
hregCli = hreg Meta.version runCliCommand

hreg :: Version -> (Command -> IO ()) -> IO ()
hreg version performCommand =
    customExecParser hregParserPrefs (cliParser version) >>= performCommand
    where
        hregParserPrefs :: ParserPrefs
        hregParserPrefs = prefs
            $ helpLongEquals
            <> showHelpOnEmpty
            <> subparserInline

runCliCommand :: Command -> IO ()
runCliCommand = \case
    Export opts -> runExport opts
    Import opts -> runImport opts
    Apply opts -> runApply opts
    Diff opts -> runDiff opts

runExport :: ExportOpts -> IO ()
runExport exportOpts@ExportOpts{..} = do
    HReg.exportReg exportOptsRoot exportOptsPath exportOptsOutFile

runImport :: ImportOpts -> IO ()
runImport importOpts@ImportOpts{..} = do
    let conf = Config importOptsRoot [] [importOptsInFile]
    HReg.runApp  conf HReg.hregImport

runApply :: ApplyOpts -> IO ()
runApply applyOpts@ApplyOpts{..} = do
    let conf = Config applyOptsRoot [(applyOptsInFile, applyOptsScope)] []
    HReg.runApp conf HReg.hregApply

runDiff :: DiffOpts -> IO ()
runDiff diffOpts@DiffOpts{..} = undefined

versionP :: Version -> Parser (a -> a)
versionP version = infoOption (hregVersion version)
     $ long "version"
    <> short 'v'
    <> help "Show HReg's version"

hregVersion :: Version -> String
hregVersion = showVersion

hregP :: Parser Command
hregP = subparser
     $ command "export" (info (helper <*> exportP) $ progDesc "Export registry branch to the .reg file")
    <> command "import" (info (helper <*> importP) $ progDesc "import .reg file to the registry")
    <> command "apply"  (info (helper <*> applyP)  $ progDesc "Apply .pol files to the registry")
    <> command "diff"   (info (helper <*> diffP)   $ progDesc "Compare two registry branches and generate .pol file")

exportP :: Parser Command
exportP = do
    root   <- strArgument (metavar "REG_ROOT")
    path   <- strArgument (metavar "BRANCH")
    output <- strArgument (metavar "OUTPUT")
    pure $ Export $ ExportOpts path output root

importP :: Parser Command
importP = do
    root  <- strArgument (metavar "REG_ROOT")
    input <- strArgument (metavar "INPUT")
    pure $ Import $ ImportOpts input root

applyP :: Parser Command
applyP = do
    root  <- strArgument (metavar "REG_PATH")
    path  <- strArgument (metavar "POL_FILE")
    scope <- scopeP
    pure $ Apply $ ApplyOpts path scope root

diffP :: Parser Command
diffP = do
    pathA <- strArgument (metavar "REG_A")
    pathB <- strArgument (metavar "REG_B")
    out <- outputP
    pure $ Diff $ DiffOpts pathA pathB out

rootP :: Parser FilePath
rootP = strOption
     $ long "path"
    <> short 'r'
    <> metavar "BRANCH_ROOT"
    <> help "Path to the root of branch to export"

outputP :: Parser FilePath
outputP = strOption
     $ long "output"
    <> short 'o'
    <> metavar "OUTPUT"
    <> help "write export to the file"

scopeP :: Parser Scope
scopeP = scopeMachineP <|> scopeUserP

scopeMachineP :: Parser Scope
scopeMachineP = flag' Machine
    (long "machine"
    <> short 'm'
    <> help "import to the HKLM branch"
    )

scopeUserP :: Parser Scope
scopeUserP = User <$> strOption
    -- FIX: SID MUST be validated
    (long "user"
    <> short 'u'
    <> metavar "SID"
    <> help "import to the HKCU/<SID> branch"
    )

cliParser :: Version -> ParserInfo Command
cliParser version = modifyHeader
    $ modifyFooter
    $ info (helper <*> versionP version <*> hregP)
        $ fullDesc
        <> progDesc "Registry manager"

-- to put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo a -> ParserInfo a
modifyHeader p = p {infoHeader = stringChunk $ toString artHeader}

-- to put custom footer which doesn't cut all spaces
modifyFooter :: ParserInfo a -> ParserInfo a
modifyFooter p = p {infoFooter = stringChunk $ toString artFooter}

artFooter :: Text
artFooter = ""

artHeader :: Text
artHeader = unlines [
    "  _   _    ____    U _____ u   ____   ",
    " |'| |'|U |  _\"\\ u \\| ___\"|/U /\"___|u ",
    "/| |_| |\\\\| |_) |/  |  _|\"  \\| |  _ / ",
    "U|  _  |u |  _ <    | |___   | |_| |  ",
    " |_| |_|  |_| \\_\\   |_____|   \\____|  ",
    " //   \\\\  //   \\\\_  <<   >>   _)(|_   ",
    "(_\") (\"_)(__)  (__)(__) (__) (__)__)  "
    ]
