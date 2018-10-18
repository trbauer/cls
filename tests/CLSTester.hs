
import Control.Monad
import Data.Bits
import Data.List
import Data.Int
import Data.Word
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
  --
  runImmTests os
  --
  runAddTest os "int" ["1","3","-2"] "2"

-- tests evaluators (kernel arg evaluates)
runImmTests :: Opts -> IO ()
runImmTests os = do
  let sintImmTest arg_expr val_expr = runArgTestImm os "int" arg_expr (val_expr :: Int64)
  let uintImmTest arg_expr val_expr = runArgTestImm os "uint" arg_expr (val_expr :: Word64)
  let floatImmTest arg_expr val_expr = runArgTestImm os "float" arg_expr (val_expr :: Float)
  --
  -- floatImmTest "1.0/0.0"             (1.0/0.0)
  floatImmTest "23.1"                 23.1
  floatImmTest "23.0 + 0.1"           23.1
  floatImmTest "23   + 0.1"           23.1
  floatImmTest "abs(-12.1)"          (abs (-12.1))
  floatImmTest "abs( 12.1)"          (abs   12.1)
  floatImmTest "float(23)"           (23.0)
  --
  sintImmTest "44"                   44
  sintImmTest "( 44 )"               44
  sintImmTest "(40+ -4)"             36
  sintImmTest "2*(40+ 4)/2 - 1"      43
  sintImmTest "(33<<4)>>2"           ((33`shiftL`4)`shiftR`2)
  sintImmTest "1&(7%4)"              (1.&.(7`mod`4))
  sintImmTest "gcd(12,8)"            (gcd 12 8)
  sintImmTest "lcm(12,8)"            (lcm 12 8)
  sintImmTest "abs(-12)"             (abs (-12))
  sintImmTest "abs(12)"              (abs 12)
  sintImmTest "int(23.2)"            23
  sintImmTest "0xF1|0x8F"            (0xF1.|.0x8F) -- 0xFF
  sintImmTest "0xFFFF^0xAAAA"        (0xFFFF`xor`0xAAAA)
  sintImmTest "~0xAAAA&0xFFFF"       (complement 0xAAAA .&. 0xFFFF) -- 0x5555



class ShowNum n where
  showNum :: n -> String
instance ShowNum Int64 where
  showNum = show
instance ShowNum Word64 where
  -- showNum = printf "0x%016X"
  showNum = printf "0x%X"
instance ShowNum Int32 where
  showNum = show
instance ShowNum Word32 where
  showNum = printf "0x%X"
  -- showNum = printf "0x%08X"
instance ShowNum Float where
  showNum = show

runArgTestImm :: ShowNum a => Opts -> String -> String -> a -> IO ()
runArgTestImm os ty arg_expr expect_value = do
  runArgTest
    os
    ("imm. arg: " ++ arg_expr ++ " ")
    ty
    arg_expr
    (showNum expect_value)

runAddTest :: Opts -> String -> [String] -> String -> IO ()
runAddTest os arg_type args result = do
  let script :: String
      script =
        "let B=0:w\n" ++
        concatMap (\a -> "#0`tests/add.cl[-DT=" ++ arg_type ++ "]`add<1>(B," ++ a ++ ")\n") args ++
        "diff<" ++ arg_type ++ ">(" ++ result ++ ",B)\n"
  runScript os script

runArgTest :: Opts -> String -> String -> String -> String -> IO ()
runArgTest os arg_desc arg_type arg_expr show_expect_value = do
  putStr $ printf "%-32s" arg_desc
  let script :: String
      script =
        "let R=0:w\n" ++
        "let V=0:w\n" ++
        "#0`tests/args.cl[-DT=" ++ arg_type ++ " -DEXPECT=" ++ show_expect_value ++ "]`test<1>(R," ++ arg_expr ++ ",V)\n" ++
        "diff<" ++ arg_type ++ ">(" ++ show_expect_value ++ ",V)\n"
        -- "diff<" ++ arg_type ++ ">(0,R)\n"
  runScript os script

runScript :: Opts -> String -> IO ()
runScript os script = do
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
