{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Database.PureCDB
import Language.Haskell.Exts.Annotated
import Control.Exception
import Control.DeepSeq (($!!))
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad (void, when)
import System.Process (readProcess)
import Data.List (foldl', intercalate, isInfixOf)
import Data.Maybe
import Data.Serialize
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import Control.Applicative ((<$>), (<*>))
import System.IO (BufferMode (..), hSetBuffering, hFlush, hPutStrLn, stdout, stderr)
import Data.Generics.Uniplate.Data (transformBiM)
import System.Directory (doesFileExist)
import Data.Data
import Data.Either (rights)
import Language.Preprocessor.Cpphs (runCpphs, defaultCpphsOptions,  CpphsOptions(..)
            , defaultBoolOptions, BoolOptions(..))

data Flag = Build Bool | File FilePath | Line | Query String
                | CPPInclude String | OExtension String | Verbose
          deriving (Show)

data Config = Config { cBuild :: Bool, cFile :: FilePath, cLine :: Bool
                        , cQuery :: Maybe String, cCPPIncludes :: [String]
                        , cExtensions :: [String]
                        , verbose :: Bool } deriving Show

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
            , Option ['v'] [ "verbose" ] (NoArg $ Verbose) "Verbose output"
          ]

parseFlags :: [Flag] -> Config
parseFlags = foldl' go $ Config False "hscope.out" False Nothing [] [] False where
    go c (Build b) = c { cBuild = b }
    go c (File f) = c { cFile = f }
    go c Line = c { cLine = True }
    go c (Query q) = c { cQuery = Just q }
    go c (CPPInclude i) = c { cCPPIncludes = i:(cCPPIncludes c) }
    go c (OExtension i) = c { cExtensions = i:(cExtensions c) }
    go c Verbose = c { verbose = True }

addInfo :: Lines -> IType -> Name SrcSpanInfo -> WriteCDB IO ()
addInfo vec ity n = case vec V.!? (l - 2) of
    Nothing -> warning $ "Bad line: " ++ show n ++ ", " ++ show vec
    Just lp -> addBS (B.pack f) $ encode $ Info ity (fileName src) (fst lp) (snd lp)
    where l = startLine src
          (src, f) = case n of
            (Ident src' f') -> (src', f')
            (Symbol src' f') -> (src', f')

traverseAST :: (Data b, Monad m, Data a, Functor m) => (a -> m ()) -> b -> m ()
traverseAST cb = void . transformBiM go where go v = cb v >> return v

handleDefinitions :: Lines -> Decl SrcSpanInfo -> WriteCDB IO ()
handleDefinitions vec = go where
    go (PatBind _ (PVar _ n) _ _ _) = addDef n
    go (FunBind _ ((Match _ n _ _ _):_)) = addDef n
    go _ = return ()
    addDef = addInfo vec Definition

handleCalls :: Lines -> QName SrcSpanInfo -> WriteCDB IO ()
handleCalls vec (UnQual _ n) = addInfo vec Call n
handleCalls _ _ = return ()

handleConstructors :: Lines -> ConDecl SrcSpanInfo -> WriteCDB IO ()
handleConstructors vec (ConDecl _ n _) = addInfo vec Definition n
handleConstructors vec (RecDecl _ n recs) = do
    addInfo vec Definition n
    mapM_ go recs
    where go (FieldDecl _ ns _) = mapM_ (addInfo vec Definition) ns
handleConstructors vec (InfixConDecl _ _ n _) = addInfo vec Definition n

handleDeclarations :: Lines -> DeclHead SrcSpanInfo -> WriteCDB IO ()
handleDeclarations vec (DHead _ n _) = addInfo vec Definition n
handleDeclarations vec (DHInfix _ _ n _) = addInfo vec Definition n
handleDeclarations vec (DHParen _ declHead) = handleDeclarations vec declHead

mapLines :: Show a => FilePath -> [a] -> [String] -> [a]
mapLines f to = reverse . snd . foldl' go ((to, True), []) where
    go t ('#':str) = let ((xs, _), res) = ret t in ((xs, isInfixOf f str), res)
    go t _ = ret t
    ret ((a@(x:xs), b), res) = ((if b then xs else a, b), x:res)
    ret (([], _), res) = (([], False), (head res):res) -- TODO explain why head is safe here?

-- `runCpphs` calls `error` when an #error directive is encountered.
-- This would terminate our program; this function catches that.
safeCpphs :: CpphsOptions -> FilePath -> String -> IO (Either String String)
safeCpphs opts filename input = do
    res <- try $ (evaluate $!!) =<< runCpphs opts filename input
    return $ case res of
        Left (ErrorCall e) -> Left e
        Right cppOutput    -> Right cppOutput

preprocess :: [String] -> FilePath -> IO (Either String (String, Lines))
preprocess idirs f = do
    rf <- B.readFile f
    e'cppOutput <- safeCpphs cpphsOpts f $ B.unpack rf
    let processCppOutput flns = ( fconts flns
                                , V.fromList $ mapLines f (zip [1 ..] $ B.lines rf) flns)
    return $ (processCppOutput . lines) `fmap` e'cppOutput
    where fconts = intercalate "\n" . map cmnt
          cmnt ('#':_) = ""
          cmnt x = x
          cpphsOpts = defaultCpphsOptions { includes = idirs, boolopts = bools }
          bools = defaultBoolOptions { stripC89 = True, stripEol = True }


parseCurrentFile :: FilePath -> String -> [Extension] -> Either String (Module SrcSpanInfo)
parseCurrentFile f fstr exts = case parseFileContentsWithMode (pmode exts) fstr of
        ParseOk m -> Right m
        ParseFailed src str -> let (extStr, rest) = break (== ' ') str
                                   ext            = classifyExtension extStr
                -- Make sure to check if the extension is already enabled because
                -- a file can use a No... LANGUAGE pragma in which case we would do this
                -- infinitely often.
                in if rest == " is not enabled" && ext `notElem` exts
                   then parseCurrentFile f fstr (ext:exts)
                   else Left $ str ++ " for " ++ f ++ " at " ++ show src ++ " with " ++ show exts
    where pmode exs = defaultParseMode { fixities = Just []
                            , extensions = exs
                            , parseFilename = f }

buildOne :: Config -> FilePath -> WriteCDB IO ()
buildOne cfg f = do
    when (verbose cfg) $
        liftIO $ putStrLn $ "Processing " ++ f
    addBS (B.pack "0_hs_files") (B.pack f)
    e'preprocessed <- liftIO $ preprocess (cCPPIncludes cfg) f
    case e'preprocessed of
        Left cppErr       -> warning $ "CPP error: " ++ cppErr
        Right (fstr, lns) -> either (liftIO . putStrLn) (go lns) $
                               parseCurrentFile f fstr (map classifyExtension $ cExtensions cfg)
    where go lns modul = do
                    traverseAST (handleDefinitions lns) modul
                    traverseAST (handleCalls lns) modul
                    traverseAST (handleConstructors lns) modul
                    traverseAST (handleDeclarations lns) modul
                    -- liftIO $ putStrLn $ show modul

handleQuery :: ReadCDB -> String -> IO ()
handleQuery cdb ('1':str) = findInfo Definition cdb str
handleQuery cdb ('3':str) = findInfo Call cdb str
handleQuery cdb ('4':str) = do
    files <- fmap (map B.unpack) $ getBS cdb (B.pack "0_hs_files")
    lns <- fmap lines $ readProcess "grep" ("-n":"-H":str:files) ""
    outputLines $ map go lns
    where go l = let (f, rest1) = break (':' ==) l
                     (n, rest2) = break (':' ==) $ drop 1 rest1
                  in f ++ " <unknown> " ++ n ++ " " ++ drop 1 rest2 
handleQuery cdb ('7':str) = do
    files <- fmap (map B.unpack) $ getBS cdb $ B.pack "0_hs_files"
    outputLines $ map go $ filter (isInfixOf str) files
    where go f = f ++ " <unknown> 1 <unknown>"
handleQuery _ _ = return ()

runLines :: ReadCDB -> IO ()
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

findInfo :: IType -> ReadCDB -> String -> IO ()
findInfo ity cdb str = do
    bses <- getBS cdb $ B.pack str
    outputLines $ catMaybes $ map go $ rights $ map decode bses
    where go (Info t file lno line) | t == ity = Just $ file ++ " " ++ str
                                        ++ " " ++ show lno ++ " " ++ B.unpack line
                                    | otherwise = Nothing

warning :: (MonadIO m) => String -> m ()
warning msg = liftIO . hPutStrLn stderr $ "WARNING: " ++ msg

main :: IO ()
main = do
    -- Always set to line buffering
    hSetBuffering stdout LineBuffering

    (flags, rest, _) <- fmap (getOpt Permute options) getArgs
    let cfg = parseFlags flags
    when (cBuild cfg) $ do
        if null rest then putStrLn $ "Please provide files to build the hscope database"
                     else makeCDB (mapM_ (buildOne cfg) rest) (cFile cfg) 
    if null flags then usage else go cfg
    where usage = do
            pn <- getProgName
            putStrLn $ usageInfo ("Usage: " ++ pn ++ " [OPTION...] files...") options
          go cfg = do
            b <- doesFileExist $ cFile cfg
            if b then do
                    cdb <- openCDB $ cFile cfg
                    maybe (return ()) (handleQuery cdb) $ cQuery cfg
                    when (cLine cfg) $ runLines cdb
                 else putStrLn $ cFile cfg ++ " does not exist"
