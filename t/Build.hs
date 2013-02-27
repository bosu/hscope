{-# OPTIONS -fno-warn-unused-do-bind -XBangPatterns #-}
module Main (main) where

import Test.Simple
import System.Unix.Directory (withTemporaryDirectory)
import System.Directory (canonicalizePath, doesFileExist)
import System.Process (system, readProcess, runInteractiveProcess, waitForProcess)
import System.Exit (ExitCode(ExitSuccess))
import Control.Monad.Trans (liftIO)
import System.IO (hGetContents, hPutStrLn, hFlush, hSetBinaryMode)

hscopeInteract :: FilePath -> FilePath -> [String] -> IO (ExitCode, String)
hscopeInteract hpath td cmds = do
    (inp, out, _, ph1) <- runInteractiveProcess hpath [ "-dl", "-f", td ++ "/hscope.out" ]
                                Nothing Nothing
    hSetBinaryMode inp False
    mapM_ (go inp) $ cmds ++ [ "q" ]

    !l1 <- hGetContents out
    ec2 <- waitForProcess ph1
    return (ec2, l1)
    where go inp cmd = hPutStrLn inp cmd >> hFlush inp

main :: IO ()
main = withTemporaryDirectory "/tmp/hscope_test_XXXXXX" $ \td -> testSimpleMain $ do
    plan 47
    hpath <- liftIO $ canonicalizePath "./dist/build/hscope/hscope"
    tfile <- liftIO $ canonicalizePath "./t/files/Simple.hs"
    ec1 <- liftIO $ system $ "cd " ++ td ++ " && " ++ hpath ++ " -b " ++ tfile
    res1 <- liftIO $ readProcess "cdb" [ "-l", td ++ "/hscope.out" ] ""
    is ec1 ExitSuccess
    like res1 "main"

    res2 <- liftIO $ readProcess "cdb" [ "-q", td ++ "/hscope.out", "main" ] ""
    like res2 "main = "

    res3 <- liftIO $ readProcess "cdb" [ "-q", td ++ "/hscope.out", "findInfo" ] ""
    like res3 "-> findInfo "

    res4 <- liftIO $ readProcess "cdb" [ "-q", td ++ "/hscope.out", "options" ] ""
    like res4 "Permute options"

    res5 <- liftIO $ readProcess "cdb" [ "-q", td ++ "/hscope.out", "runLines" ] ""
    like res5 ">>= runLines"

    (ec2, l1) <- liftIO $ hscopeInteract hpath td [ "1main", "1findInfo" ]
    is ec2 ExitSuccess
    like l1 ">> "
    like l1 tfile
    like l1 "main ="
    like l1 "findInfo ity cdb str ="

    (ec3, l2) <- liftIO $ hscopeInteract hpath td [ "3findInfo", "1Build", "1Config" ]
    is ec3 ExitSuccess
    like l2 "-> findInfo "
    like l2 "2 lines"
    like l2 "Build Bool"
    like l2 "    Config {"

    (ec4, l3) <- liftIO $ hscopeInteract hpath td [ "1Flag", "1Unused", "1cBuild" ]
    is ec4 ExitSuccess
    like l3 "data Flag"
    like l3 "type Unused"
    like l3 "cBuild :: Bool"

    (ec5, l4) <- liftIO $ hscopeInteract hpath td [ "3cBuild", "3Flag", "3==" ]
    is ec5 ExitSuccess
    like l4 "cBuild cfg"
    like l4 "parseFlags ::"
    like l4 "groupBy (==)"

    res6 <- liftIO $ readProcess hpath [ "-b", "-f", td ++ "/foo.out", "t/files/a.c" ] ""
    like res6 "Parse error: 0"
    like res6 "a.c"

    dfoo <- liftIO $ doesFileExist $ td ++ "/foo.out"
    is dfoo True

    res7 <- liftIO $ readProcess hpath [ "-b", "-f", td ++ "/foo.out"
                                            , "t/files/Arrow.hs", "t/files/CPP.hs" ] ""
    is res7 ""

    res8 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/foo.out"
                                            , "-1", "doSth" ] ""
    like res8 "doSth ="

    res9 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/foo.out"
                                            , "-1", "foo" ] ""
    like res9 "foo 6 bar = id"
    unlike res9 "warning"

    res9_1 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/foo.out"
                                            , "-7", "Ar" ] ""
    is res9_1 "cscope: 1 lines\nt/files/Arrow.hs <unknown> 1 <unknown>\n"

    res10 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/hscope.out"
                                            , "-3", "groupBy" ] ""
    like res10 "groupBy (==)"

    res10_1 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/hscope.out"
                                            , "-3", "Build" ] ""
    like res10_1 "NoArg $ Build False"
    like res10_1 "Build b) ="

    res11 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/hscope.out"
                                            , "-4", "= do" ] ""
    like res11 "files/Simple.hs <unknown> 115 main = do"

    res12 <- liftIO $ readProcess hpath [ "-b", "-f", td ++ "/foo.out"
                                    , "-I", "t/files/a", "-I", "t/files/b"
                                    , "t/files/CPP2.hs" ] ""
    is res12 ""

    res13 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/foo.out"
                                            , "-1", "bar" ] ""
    like res13 "bar 7 bar = id"

    res14 <- liftIO $ readProcess hpath [ "-b", "-f", td ++ "/foo.out"
                                    , "t/files/garrs.hs" ] ""
    is res14 ""

    res15 <- liftIO $ readProcess hpath [ "--help" ] ""
    like res15 $ "Usage: hscope [OPTION...] files..."
    like res15 "-b"

    res16 <- liftIO $ readProcess hpath [] ""
    is res16 res15

    res17 <- liftIO $ readProcess hpath [ "--build" ] ""
    like res17 $ "Please provide files to build the hscope database"

    res18 <- liftIO $ readProcess hpath [ "--build", "-f", td ++ "/foo.out"
                            , "t/files/Fixity.hs" ] ""
    is res18 $ ""

    res19 <- liftIO $ readProcess hpath [ "-d", "-f", td ++ "/foo.out"
                                            , "-3", "==>" ] ""
    like res19 "0 ==>"

    res20 <- liftIO $ readProcess hpath [ "--build", "-f", td ++ "/foo.out"
                            , "-X", "MagicHash", "t/files/Unbox.hs" ] ""
    is res20 $ ""

    res21 <- liftIO $ readProcess hpath [ "--build", "-f", td ++ "/foo.out"
                            , "t/files/Setup.lhs" ] ""
    is res21 $ ""

    return ()
