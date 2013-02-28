{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Database.CDB.Write
import Database.CDB.Read
import Database.CDB.Packable
import Language.Haskell.Exts.Annotated
import Control.Monad.Trans (liftIO)
import Control.Monad (void, when)
import System.Process (readProcess)
import Data.List (foldl', intercalate, isInfixOf)
import Data.Maybe
import Data.Serialize
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import Control.Applicative ((<$>), (<*>))
import System.IO (hFlush, stdout)
import Data.Generics.Uniplate.Data (transformBiM)
import System.Directory (doesFileExist)
import Data.Data
import Language.Preprocessor.Cpphs (runCpphs, defaultCpphsOptions,  CpphsOptions(..)
            , defaultBoolOptions, BoolOptions(..))

data Flag = Build Bool | File FilePath | Line | Query String
                | CPPInclude String | OExtension String deriving (Show)

data Config = Config { cBuild :: Bool, cFile :: FilePath, cLine :: Bool
                        , cQuery :: Maybe String, cCPPIncludes :: [String]
                        , cExtensions :: [String] } deriving Show

data IType = Definition | Call deriving (Enum, Show, Eq)
data Info = Info IType String Int B.ByteString deriving Show
type Lines = V.Vector (Int, B.ByteString)

instance Serialize Info where
    put (Info ity file line bs) = do
        put $ fromEnum ity 
        put file
        put line
        put bs

    get = do
        ity <- fmap toEnum get
        Info ity <$> get <*> get <*> get

instance Unpackable Info where
    unpack = either error id . decode

options :: [OptDescr Flag]
options = [
            Option ['b'] [ "build" ] (NoArg $ Build True) "Build hscope database"
            , Option ['d'] [ "dont-update" ] (NoArg $ Build False)
                "Do not update the cross-reference"
            , Option ['l'] [ "line" ] (NoArg $ Line) "Line-oriented interface"
            , Option ['f'] [ "file" ] (ReqArg File "REFFILE") $
                "Use REFFILE as the cross-reference file name instead of the default"
                    ++ " \"hscope.out\""
            , Option ['1'] [ "definition" ] (ReqArg (Query . ('1':)) "SYMBOL")
                    $ "Find the definition of the SYMBOL"
            , Option ['3'] [ "callers" ] (ReqArg (Query . ('3':)) "SYMBOL")
                    $ "Find SYMBOLs calling this SYMBOL"
            , Option ['4'] [ "text" ] (ReqArg (Query . ('4':)) "TEXT") $ "Find TEXT in files"
            , Option ['7'] [ "file" ] (ReqArg (Query . ('7':)) "FILE")
                    $ "Find files matching FILE"
            , Option ['I'] [ "cpp-include" ] (ReqArg CPPInclude "DIRECTORY")
                    $ "Include path for CPP preprocessor"
            , Option ['X'] [ "extension" ] (ReqArg OExtension "EXTENSION") $ "Add GHC extension"
          ]

parseFlags :: [Flag] -> Config
parseFlags = foldl' go $ Config False "hscope.out" False Nothing [] [] where
    go c (Build b) = c { cBuild = b }
    go c (File f) = c { cFile = f }
    go c Line = c { cLine = True }
    go c (Query q) = c { cQuery = Just q }
    go c (CPPInclude i) = c { cCPPIncludes = i:(cCPPIncludes c) }
    go c (OExtension i) = c { cExtensions = i:(cExtensions c) }

addInfo :: Lines -> IType -> Name SrcSpanInfo -> CDBMake
addInfo vec ity n = cdbAdd f $ encode $ Info ity (fileName src) (fst lp) (snd lp)
    where l = startLine src
          lp = maybe (error $ "Bad line: " ++ show n ++ ", " ++ show vec) id $ vec V.!? (l - 2)
          (src, f) = case n of
            (Ident src' f') -> (src', f')
            (Symbol src' f') -> (src', f')

traverseAST :: (Data b, Monad m, Data a, Functor m) => (a -> m ()) -> b -> m ()
traverseAST cb = void . transformBiM go where go v = cb v >> return v

handleDefinitions :: Lines -> Decl SrcSpanInfo -> CDBMake
handleDefinitions vec = go where
    go (PatBind _ (PVar _ n) _ _ _) = addDef n
    go (FunBind _ ((Match _ n _ _ _):_)) = addDef n
    go _ = return ()
    addDef = addInfo vec Definition

handleCalls :: Lines -> QName SrcSpanInfo -> CDBMake
handleCalls vec (UnQual _ n) = addInfo vec Call n
handleCalls _ _ = return ()

handleConstructors :: Lines -> ConDecl SrcSpanInfo -> CDBMake
handleConstructors vec (ConDecl _ n _) = addInfo vec Definition n
handleConstructors vec (RecDecl _ n recs) = do
    addInfo vec Definition n
    mapM_ go recs
    where go (FieldDecl _ ns _) = mapM_ (addInfo vec Definition) ns
handleConstructors _ c = error $ "handleConstructors " ++ show c

handleDeclarations :: Lines -> DeclHead SrcSpanInfo -> CDBMake
handleDeclarations vec (DHead _ n _) = addInfo vec Definition n
handleDeclarations _ c = error $ "handleDeclarations " ++ show c

mapLines :: Show a => FilePath -> [a] -> [String] -> [a]
mapLines f to = reverse . snd . foldl' go ((to, True), []) where
    go t ('#':str) = let ((xs, _), res) = ret t in ((xs, isInfixOf f str), res)
    go t _ = ret t
    ret ((a@(x:xs), b), res) = ((if b then xs else a, b), x:res)
    ret t = t

preprocess :: [String] -> FilePath -> IO (String, Lines)
preprocess idirs f = do
    rf <- B.readFile f
    flns <- fmap lines $ runCpphs cpphsOpts f $ B.unpack rf
    return (fconts flns, V.fromList $ mapLines f (zip [1 ..] $ B.lines rf) flns)
    where fconts = intercalate "\n" . map cmnt
          cmnt ('#':_) = ""
          cmnt x = x
          cpphsOpts = defaultCpphsOptions { includes = idirs, boolopts = bools }
          bools = defaultBoolOptions { stripC89 = True }

parseCurrentFile :: FilePath -> String -> [Extension] -> Either String (Module SrcSpanInfo)
parseCurrentFile f fstr exts = case parseFileContentsWithMode (pmode exts) fstr of
        ParseOk m -> Right m
        ParseFailed src str -> let (ext, rest) = break (== ' ') str
                in if rest == " is not enabled"
                   then parseCurrentFile f fstr ((classifyExtension ext):exts)
                   else Left $ str ++ " for " ++ f ++ " at " ++ show src ++ " with " ++ show exts
    where pmode exs = defaultParseMode { fixities = Just []
                            , extensions = exs
                            , parseFilename = f }

buildOne :: Config -> FilePath -> CDBMake
buildOne cfg f = do
    cdbAdd "0_hs_files" f
    (fstr, lns) <- liftIO $ preprocess (cCPPIncludes cfg) f
    either (liftIO . putStrLn) (go lns) $ parseCurrentFile f fstr
        $ map classifyExtension $ cExtensions cfg
    where go lns modul = do
                    traverseAST (handleDefinitions lns) modul
                    traverseAST (handleCalls lns) modul
                    traverseAST (handleConstructors lns) modul
                    traverseAST (handleDeclarations lns) modul
                    -- liftIO $ putStrLn $ show modul

handleQuery :: CDB -> String -> IO ()
handleQuery cdb ('1':str) = findInfo Definition cdb str
handleQuery cdb ('3':str) = findInfo Call cdb str
handleQuery cdb ('4':str) = do
    let files = cdbGetAll cdb "0_hs_files"
    lns <- fmap lines $ readProcess "grep" ("-n":"-H":str:files) ""
    outputLines $ map go lns
    where go l = let (f, rest1) = break (':' ==) l
                     (n, rest2) = break (':' ==) $ drop 1 rest1
                  in f ++ " <unknown> " ++ n ++ " " ++ drop 1 rest2 
handleQuery cdb ('7':str) = do
    let files = cdbGetAll cdb "0_hs_files"
    outputLines $ map go $ filter (isInfixOf str) files
    where go f = f ++ " <unknown> 1 <unknown>"
handleQuery _ _ = return ()

runLines :: CDB -> IO ()
runLines cdb = do
    putStr ">> "
    hFlush stdout
    !l <- getLine
    handleQuery cdb l
    case l of
        ('q':_) -> return ()
        _ -> runLines cdb

outputLines :: [String] -> IO ()
outputLines infos = do
    putStrLn $ "cscope: " ++ (show $ length infos) ++ " lines"
    mapM_ putStrLn infos

findInfo :: IType -> CDB -> String -> IO ()
findInfo ity cdb str = outputLines $ catMaybes $ map go $ cdbGetAll cdb str
    where go (Info t file lno line) | t == ity = Just $ file ++ " " ++ str
                                        ++ " " ++ show lno ++ " " ++ B.unpack line
                                    | otherwise = Nothing

main :: IO ()
main = do
    (flags, rest, _) <- fmap (getOpt Permute options) getArgs
    let cfg = parseFlags flags
    when (cBuild cfg) $ do
        if null rest then putStrLn $ "Please provide files to build the hscope database"
                     else cdbMake (cFile cfg) $ mapM_ (buildOne cfg) rest
    if null flags then usage else go cfg
    where usage = do
            pn <- getProgName
            putStrLn $ usageInfo ("Usage: " ++ pn ++ " [OPTION...] files...") options
          go cfg = do
            b <- doesFileExist $ cFile cfg
            if b then do
                    cdb <- cdbInit $ cFile cfg
                    maybe (return ()) (handleQuery cdb) $ cQuery cfg
                    when (cLine cfg) $ runLines cdb
                 else putStrLn $ cFile cfg ++ " does not exist"
