
import Control.Monad
import Data.Bits
import Data.List
import System.Environment
import System.Exit
import System.Directory
import System.IO
import System.Process
import Text.Printf


data Opts =
  Opts {
    oVerbosity :: !Int
  , oFailFast :: !Bool
  } deriving Show
oVerbose :: Opts -> Bool
oVerbose = (>0) . oVerbosity
dft_opts :: Opts
dft_opts = Opts 0 True

cls64_exe :: FilePath
cls64_exe = "builds/vs2017-64/Debug/cls64.exe"

parseOpts :: [String] -> Opts -> IO Opts
parseOpts []     os = return os
parseOpts (a:as) os =
    case a of
      "-v" -> continue $ os{oVerbosity = 1}
      "-v2" -> continue $ os{oVerbosity = 2}
      ('-':_) -> badOpt "unrecongized option"
      _ -> badOpt "unrecongized argument"
  where continue :: Opts -> IO Opts
        continue os = parseOpts as os

        badOpt :: String -> IO a
        badOpt msg = die (a ++ ": " ++ msg)


main :: IO ()
main = getArgs >>= run
run :: [String] -> IO ()
run as = do
  z <- doesFileExist cls64_exe
  unless z $
    die $ cls64_exe ++ ": file not found"
  os <- parseOpts as dft_opts

  let intTest = runTestIntImm os
  intTest "44"               44
  intTest "( 44 )"           44
  intTest "(40+ -4)"         36
  intTest "2*(40+ 4)/2 - 1"  43
  intTest "(33<<4)>>2"       ((33`shiftL`4)`shiftR`2)
  intTest "1&(7%4)"          (7.&.(7`mod`4))
  intTest "gcd(12,8)"        (gcd 12 8)
  intTest "lcm(12,8)"        (lcm 12 8)
  intTest "abs(-12)"         (abs (-12))
  intTest "int(23.2)"        23
  intTest "0xFFFF"           (0xFFFF`xor`0xAAAA)
  intTest "~0xAAAA&0xFFFF"   (complement 0xAAAA `xor` 0xFFFF)


runTestIntImm :: Opts -> String -> Int -> IO ()
runTestIntImm os arg_expr expect_value = do
  runTest
    os
    ("imm. arg: " ++ arg_expr ++ " ")
    "int"
    arg_expr
    (show expect_value)


runTest :: Opts -> String -> String -> String -> String -> IO ()
runTest os arg_desc arg_type arg_expr expect_value = do
  putStr $ printf "%-32s" arg_desc
  let script :: String
      script =
        "let R=0:w\n" ++
        "#0`tests/args.cl[-DT=int -DEXPECT=" ++ expect_value ++ "]`test<1>(R," ++ arg_expr ++ ")\n" ++
        "diff(0,R)\n"
  (ec,out,err) <- readProcessWithExitCode cls64_exe ["-e",script] ""
  case ec of
    ExitSuccess -> do
      putStrLn $ "  PASSED"
      when (oVerbose os) $
        putStrLn $ out ++ err
      return ()
    ExitFailure ec -> do
      putStrLn $ "  FAILED"
      putStrLn $ out ++ err
      hFlush stdout
      writeFile "failed.cls" script
      when (oFailFast os) $
        die "test failed"
