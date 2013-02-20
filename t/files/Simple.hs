{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Console.GetOpt
import System.Environment (getArgs)
import Database.CDB.Write
import Database.CDB.Read
import Database.CDB.Packable
import Language.Haskell.Exts (parseFile)
import Language.Haskell.Exts.Parser (ParseResult(ParseOk))
import Control.Monad.Trans (liftIO)
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe
import Data.IORef
import Data.Generics.Zipper.Bubble (bubble, Callback(..))
import Language.Haskell.Exts.Syntax (Decl(PatBind, FunBind), Name(Ident)
                        , Pat(PVar), SrcLoc(..), Match(Match), Exp(..), QName(UnQual))
import Data.Serialize
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import Control.Applicative ((<$>), (<*>))
import System.IO (hFlush, stdout)

type Unused = Int

testSymbol :: Eq a => [a] -> [a]
testSymbol = groupBy (==)

data Flag = Build Bool | File FilePath | Line deriving (Show)

data Config =
    Config { cBuild :: Bool, cFile :: FilePath, cLine :: Bool } deriving Show

data IType = Definition | Call deriving (Enum, Show, Eq)
data Info = Info IType String Int B.ByteString deriving Show

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
            , Option ['d'] [ "dont-update" ] (NoArg $ Build False) "Do not update the cross-reference"
            , Option ['l'] [ "line" ] (NoArg $ Line) "Line-oriented interface"
            , Option ['f'] [ "file" ] (ReqArg File "REFFILE") $
                "Use REFFILE as the cross-reference file name instead of the default \"hscope.out\""
          ]

parseFlags :: [Flag] -> Config
parseFlags = foldl' go $ Config False "hscope.out" False where
    go c (Build b) = c { cBuild = b }
    go c (File f) = c { cFile = f }
    go c Line = c { cLine = True }

addInfo :: V.Vector B.ByteString -> SrcLoc -> IType -> String -> CDBMake
addInfo vec (SrcLoc file line _) ity f = cdbAdd f $ encode $ Info ity file line (vec V.! (line - 1))

handleDefinitions :: V.Vector B.ByteString -> Decl -> CDBMake
handleDefinitions vec = go where
    go (PatBind src (PVar (Ident f)) _ _ _) = addDef f src
    go (FunBind ((Match src (Ident f) _ _ _ _):_)) = addDef f src
    go _ = return ()
    addDef f src = addInfo vec src Definition f

handleSrcLoc :: IORef SrcLoc -> SrcLoc -> CDBMake
handleSrcLoc ior = liftIO . writeIORef ior

handleCalls :: IORef SrcLoc -> V.Vector B.ByteString -> Exp -> CDBMake
handleCalls ior vec (Var (UnQual (Ident f))) = do
    src <- liftIO $ readIORef ior
    addInfo vec src Call f
handleCalls _ _ _ = return ()

buildOne :: FilePath -> CDBMake
buildOne f = do
    ParseOk modul <- liftIO $ parseFile f
    lns <- V.fromList <$> B.lines <$> (liftIO $ B.readFile f)
    sior <- liftIO $ newIORef $ SrcLoc f 1 1
    bubble [Callback $ handleSrcLoc sior, Callback $ handleDefinitions lns, Callback $ handleCalls sior lns] [] modul

runLines :: CDB -> IO ()
runLines cdb = do
    putStr ">> "
    hFlush stdout
    !l <- getLine
    case l of
        ('q':_) -> return ()
        ('1':str) -> findInfo Definition cdb str
        ('3':str) -> findInfo Call cdb str
        _ -> runLines cdb

findInfo :: IType -> CDB -> String -> IO ()
findInfo ity cdb str = do
    let infos = catMaybes $ map go $ cdbGetAll cdb str
    putStrLn $ "cscope: " ++ (show $ length infos) ++ " lines"
    mapM_ putStrLn infos
    runLines cdb
    where go (Info t file lno line) | t == ity = Just $ file ++ " " ++ str
                                        ++ " " ++ show lno ++ " " ++ B.unpack line
                                    | otherwise = Nothing

main :: IO ()
main = do
    (flags, rest, _) <- fmap (getOpt Permute options) getArgs
    let cfg = parseFlags flags
    when (cBuild cfg) $ cdbMake "hscope.out" $ mapM_ buildOne rest
    when (cLine cfg) $ (cdbInit $ cFile cfg) >>= runLines
