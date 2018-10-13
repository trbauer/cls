
import Control.Monad
import Data.Bits
import Data.List
import System.Environment
import System.Exit
import System.Directory
import System.IO
import System.Process
import Text.Printf

cls64_exe :: FilePath
cls64_exe = "builds/vs2017-64/Debug/cls64.exe"

main :: IO ()
main = getArgs >>= run
run :: [String] -> IO ()
run as = do
  z <- doesFileExist cls64_exe
  unless z $
    die $ cls64_exe ++ ": file not found"
  verbose <-
        case as of
          ["-v"] -> return True
          [] -> return False
          _ -> die "invalid arguments"
  let intTest = runTestIntImm verbose
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


runTestIntImm :: Bool -> String -> Int -> IO ()
runTestIntImm verbose arg_expr expect_value = do
  runTest
    verbose
    ("imm. arg: " ++ arg_expr ++ " ")
    "int"
    arg_expr
    (show expect_value)

runTest :: Bool -> String -> String -> String -> String -> IO ()
runTest verbose arg_desc arg_type arg_expr expect_value = do
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
      when verbose $
        putStrLn $ out ++ err
      return ()
    ExitFailure ec -> do
      putStrLn $ "  FAILED"
      putStrLn $ out ++ err
      hFlush stdout
      writeFile "failed.cls" script
      die "test failed"
