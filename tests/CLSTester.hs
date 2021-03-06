import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Int
import Data.IORef
import Data.List
import Data.Word
import System.Environment
import System.Exit
import System.Directory
import System.Info
import System.IO
import System.Process
import Text.Printf
import qualified Data.ByteString as BS

import qualified System.Console.ANSI as SCA -- cabal install ansi-terminal

data Opts =
  Opts {
    oVerbosity :: !Int
  , oFailFast :: !Bool
  , oDeviceIndex :: !Int
  , oResults :: !(IORef (Int,Int,Int))
  , oClsExe :: !FilePath
  , oPrecheck :: !Bool
  }
oVerbose :: Opts -> Bool
oVerbose = (>0) . oVerbosity
oDebug :: Opts -> Bool
oDebug = (>1) . oVerbosity

mkDftOpts :: IO Opts
mkDftOpts = mkOptsDev 0
mkDftOpts1 :: IO Opts
mkDftOpts1 = mkOptsDev 1
mkOptsDev :: Int -> IO Opts
mkOptsDev d = do
  ior <- newIORef (0,0,0)
  return $
    Opts {
      oVerbosity = 0
    , oFailFast = True
    , oDeviceIndex = d
    , oResults = ior
    , oPrecheck = True
    , oClsExe = cls64_exe
    }

cls64_msvc_exe :: FilePath
cls64_msvc_exe = "builds/vs2019-64/Debug/cls64.exe"
cls32_msvc_exe :: FilePath
cls32_msvc_exe = "builds/vs2019-32/Debug/cls32.exe"
cls64_gnu_exe :: FilePath
cls64_gnu_exe = "builds/gnu-64/Debug/cls"
cls64_exe :: FilePath
cls64_exe = if os == "mingw32" then cls64_msvc_exe else cls64_gnu_exe

help :: IO ()
help = do
  putStr $
    "main - runs tests\n" ++
    "run [...] - runs with args (try -h)\n" ++
    "---------------------------\n" ++
    "mkDftOpts >>= ...\n" ++
    "  runSyntaxErrorTestCL\n" ++
    "  runSyntaxErrorTestCLS\n" ++
    "  runInitAtomTests\n" ++
    "  runDimensionTests\n" ++
    "  runInputVariableTests\n" ++
    "     runInputVariableTestPos\n" ++
    "     runInputVariableTestNeg1\n" ++
    "  runSequentialAddTest os ARG-TYPE [ARGS] EXPECT-VAL - \n" ++
    "  runPrintCommandTests\n" ++
    "  runPrintAttributeTests\n" ++
    "  runSaveTest\n" ++
    "  runSaveTestNegBadSurf\n" ++
    "  runDiffUniformTestMatch\n" ++
    "  runDiffUniformTestMismatch\n" ++
    "  runDiffSurfaceTestMatchVar\n" ++
    "  runDiffSurfaceTestMatchImm\n" ++
    "  runDiffSurfaceTestMismatchVar\n" ++
    "  runDiffSurfaceTestMismatchImm\n" ++
    "  runContextIdentifierTest\n" ++
    "  runContextIdentifierNegativeTest\n" ++
    "  runInitConstWithDim\n" ++
    "  runInitRandomTests\n" ++
    "  runInitSequenceTests\n" ++
    "  runInitFileBinTest\n" ++
    "  runSlmDynamicTest\n" ++
    "  runSlmStaticTest\n" ++
    "  runImageTests\n" ++
    "  ========== INTEL ONLY ========\n" ++
    "  runIntelBinaryTests\n" ++
    "  runIntelSpirTests\n" ++
    ""

preCheck :: IO ()
preCheck = mkDftOpts >>= preCheckForOpts

default_arguments :: [String]
default_arguments = []

parseOpts :: [String] -> Opts -> IO Opts
parseOpts []     os = return os
parseOpts (a:as) os =
    case a of
      _ | "-exe="`isPrefixOf`a -> continue os{oClsExe = val_str}
      _ | "-d="`isPrefixOf`a ->
          case reads val_str of
            [(val,"")] -> continue $ os{oDeviceIndex = val}
            _ -> badOpt "malformed device index"
      _ | "-d" `isPrefixOf` a && all isDigit (drop 2 a) -> parseOpts (("-d="++drop 2 a):as) os
      "-F" -> continue os{oFailFast = False}
      "-q" -> continue os{oVerbosity = -1}
      "-v" -> continue os{oVerbosity = 1}
      "-v2" -> continue os{oVerbosity = 2}
      "--no-precheck" -> continue os{oPrecheck = False}
      "-h" -> do
        putStr $
          "  -d=INT         tests using a given device index\n" ++
          "  -exe=PATH      overrides path to cls*.exe\n" ++
          "  --no-precheck  skip prechecks\n" ++
          "  -F             disable fast failing\n" ++
          "  -q/-v/v2       sets verbosity\n" ++
          ""
        exitSuccess
      ('-':_) -> badOpt "unrecongized option"
      _ -> badOpt "unrecongized argument"
  where val_str :: String
        val_str = drop 1 (dropWhile (/='=') a)

        continue :: Opts -> IO Opts
        continue os = parseOpts as os

        badOpt :: String -> IO a
        badOpt msg = die (a ++ ": " ++ msg)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run as = do
  dft_opts <- mkDftOpts
  os <- parseOpts as dft_opts
  unless (oVerbosity os < 0) $
    putStrLn $ "testing with EXE: " ++ oClsExe os
  when (oPrecheck os) $
    preCheckForOpts os
  runWithOpts os

putStrLnWarning :: String -> IO ()
putStrLnWarning s = putStrX SCA.Yellow (s ++ "\n")
--
putStrRed :: String -> IO ()
putStrRed = putStrX SCA.Red
putStrGreen :: String -> IO ()
putStrGreen = putStrX SCA.Green
putStrYellow :: String -> IO ()
putStrYellow = putStrX SCA.Yellow
putStrX c s = do
  SCA.setSGR [SCA.SetColor SCA.Foreground SCA.Vivid c]
  putStr s
  SCA.setSGR [SCA.Reset]

preCheckForOpts :: Opts -> IO ()
preCheckForOpts os = do
  z <- doesFileExist (oClsExe os)
  unless z $
    die $ oClsExe os ++ ": file not found"
  -- ensure the executable is newer than the source files
  mt_exe <- getModificationTime (oClsExe os)

  let checkAgainstFile :: FilePath -> IO Bool
      checkAgainstFile fp
        | any (`isSuffixOf`fp) [".cpp",".hpp",".flex"] = do
          fstr <- readFile fp
          length fstr `seq` return ()
          let mAX_COLS = 120
          let lns = zip [1..] (lines fstr) :: [(Int,String)]
          let isLineTooLong ln =
                -- allow long lines for special cases
                length ln >= mAX_COLS &&
                all (not . (`isInfixOf`ln)) ["http://","https://"]
              bad_lines_too_long = filter (isLineTooLong . snd) lns
              bad_lines_tabs = filter (('\t'`elem`) . snd) lns
              bad_lines_trailing_ws = filter ((" "`isSuffixOf`) . snd) lns
          forM_ bad_lines_too_long $ \(lno,ln) -> do
            putStrLnWarning $ fp ++ ":" ++ show lno ++ ": line exceeds max cols"
          forM_ bad_lines_tabs $ \(lno,ln) -> do
            putStrLnWarning $ fp ++ ":" ++ show lno ++ ": line has tab space"
          forM_ bad_lines_trailing_ws $ \(lno,ln) -> do
            putStrLnWarning $ fp ++ ":" ++ show lno ++ ": line has trailing whitespace"
          mt_fp <- getModificationTime fp
          when (mt_exe < mt_fp) $
            putStrLnWarning $ fp ++ ": source file is newer than executable (recompile or touch exe)"
          return (all null [bad_lines_too_long,bad_lines_tabs,bad_lines_trailing_ws] && mt_exe >= mt_fp)
        | otherwise = return True -- ignore file

      checkAgainstDir dir = do
        fs <- listDirectory dir
        let checkPath p = do
              let fp = dir ++ "/" ++ p
              z <- doesDirectoryExist fp
              if z then checkAgainstDir fp
                else checkAgainstFile fp
        and <$> mapM checkPath fs
  z1 <- checkAgainstFile "CMakeLists.txt"
  z2 <- checkAgainstDir "src"
  when (not z1 || not z2) $
    dieOrError os "precheck failed (use --no-precheck to skip)"

dieOrError :: Opts -> String -> IO ()
dieOrError os msg = do
  putStrRed (msg ++ "\n")
  when (oFailFast os) $ exitFailure

oNormalLn :: Opts -> String -> IO ()
oNormalLn os
  | oVerbosity os < 0 = const (return ())
  | otherwise = putStrLn

runWithOpts :: Opts -> IO ()
runWithOpts os = run_tests >> print_summary >> exit
  where run_tests = do
          oNormalLn os $ "running with " ++ oClsExe os ++ " on device " ++ show (oDeviceIndex os)
          oup <- readProcess (oClsExe os) ["-l"] ""
          let filterDevice =
                filter (("DEVICE[" ++ show (oDeviceIndex os) ++ "]")`isInfixOf`)
          case filterDevice (lines oup) of
            [x] -> oNormalLn os x
            _ ->
              -- this indicates cls did not produce a valid device index for the chosen device
              -- or the tester was given an invalid index
              dieOrError os $
                "selected test device not found on machine or cls output is malformed:\n" ++
                oup
          putStrLn "==============================================="

          -- MISC
          runSyntaxErrorTestCL os
          runSyntaxErrorTestCLS os
          ---------------------
          -- not a portable test
          --   runProgramBinariesTest os
          --
          -- ATOM ARG SETTERS
          runInitAtomTests os
          --
          -- DIMENSIONS
          runDimensionTests os
          --
          -- MUTABILITY
          runSequentialAddTest os "int" ["1","3","-2"] "2"
          --
          -- INPUT VARS ($1)
          runInputVariableTests os
          --
          -- PRINT
          runPrintCommandTests os
          runPrintAttributeTests os
          --
          -- SAVE
          runSaveTest os
          runSaveTestNegBadSurf os
          --
          -- DIFF(U)
          runDiffUniformTestMatch os
          runDiffUniformTestMatchFuzzy os
          runDiffUniformTestMismatch os
          runDiffUniformTestMismatchFuzzy os
          -- DIFF(S)
          runDiffSurfaceTestMatchVar os
          runDiffSurfaceTestMatchImm os
          runDiffSurfaceTestMismatchVar os
          runDiffSurfaceTestMismatchImm os
          --
          -- MULTIPLE CONTEXTS
          runContextIdentifierTest os
          runContextIdentifierNegativeTest os
          --
          -- MEM INITIALIZERS
          --   EXPLICIT DIMENSION
          runInitConstWithDim os
          --   RANDOM VARIABLES
          runInitRandomTests os
          --   SEQUENCE VARIABLES
          runInitSequenceTests os
          --
          --   BINARY FILE MEM INITIALIZERS
          runInitFileBinTest os
          -- runInitFileTextTest os
          -- runInitFileTextColTests os
          --
          -- SLM
          runSlmDynamicTest os
          runSlmStaticTest os
          --
          -- IMAGE
          runImageTests os

          -- binary and SPIR support
          runIntelBinaryTests os
          runIntelSpirTests os

          --
          -- TODO: random<> init
          -- TODO:   IMAGE FILE INITIALIZER

          -- TODO:
          -- tests that use:
          --   mem init random
          --   mem init file
          --   mem init file (with padding / dimension slicer)
          --   mem init resize expression
          --   more print() tests (int2 formatter)
          return ()

        print_summary :: IO ()
        print_summary = do
          putStrLn "==============================================="
          (total,passed,skipped) <- readIORef (oResults os)
          if total == passed then
              putStrGreen ("passed all " ++ show passed ++ " tests\n")
            else if passed + skipped == total then
              putStrYellow ("passed " ++ show passed ++ " (skipped " ++ show skipped ++ ")\n")
            else
              putStrRed ("passed " ++ show passed ++ " of " ++ show total ++ "\n")

        exit :: IO ()
        exit = do
          (total,passed,skipped) <- readIORef (oResults os)
          if total == passed then exitSuccess
            else exitFailure

runDimensionTests :: Opts -> IO ()
runDimensionTests os = do
  let runNegative :: String -> IO ()
      runNegative dims = do
        let script :: String
            script =
              "let G=0:w\n" ++
              "let L=0:w\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/dims.cl`dims" ++ dims ++ "(G,L)\n"
        runScript os ("dimension syntax (negative): " ++ dims) (mShouldExit 1) script
  runNegative "<1x>"
  runNegative "<1024x16xDSF>"
  runNegative "<-4>"
  runNegative "<1024,-4>"
  --
  let runOne :: String -> [Int] -> [Int] -> IO ()
      runOne dims expect_gs expect_ls = do
        let mkDiffStatement sym ds0
              | null ds0 = ""
              | otherwise = "diff((int4)(" ++ intercalate "," (map show ds_padded) ++ ")," ++ sym ++ ")\n"
              where ds_padded = ds0 ++ replicate (3 - length ds0) 1 ++ [0] -- e.g. [256] -> [256,1,1,0]
        let script :: String
            script =
              "let G=0:w\n" ++
              "let L=0:w\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/dims.cl`dims" ++ dims ++ "(G,L)\n" ++
              mkDiffStatement "G" expect_gs ++
              mkDiffStatement "L" expect_ls ++
              ""
        runScript os ("testing dimension syntax: " ++ dims) (mShouldExit 0) script
  --
  runOne "<1024>"                       [1024]      []
  runOne "<2*512>"                      [1024]      []
  runOne "<1024,nullptr>"               [1024]      []
  runOne "<1024,NULL>"                  [1024]      []
  runOne "<1024,32>"                    [1024]      [32]
  runOne "<1024,2*16>"                  [1024]      [32]
  runOne "<1024,min(g.x/16,128)>"       [1024]      [64]
  --
  runOne "<1024x64>"                    [1024,64]   []
  runOne "<1024 x 64>"                  [1024,64]   []
  runOne "<1024 x64>"                   [1024,64]   []
  runOne "<1024x 64>"                   [1024,64]   []
  runOne "<1024x(g.x/256)>"             [1024, 4]   []
  runOne "<(1024)x (2*32)>"             [1024,64]   []
  runOne "<1024x64,32x1>"               [1024,64]   [32,1]
  runOne "<1024x64,(g.x/256)x(g.y/4)>"  [1024,64]   [4,16]
  --
  runOne "<1024x16x4>"                  [1024,16,4] []
  runOne "<1024x16 x4>"                 [1024,16,4] []
  runOne "<1024x16x 4>"                 [1024,16,4] []
  runOne "<1024x16 x 4>"                [1024,16,4] []
  runOne "<1024x16x(4)>"                [1024,16,4] []
  runOne "<1024x16 x(4)>"               [1024,16,4] []
  runOne "<1024x16x4,16x2x2>"           [1024,16,4] [16,2,2]
  runOne "<1024x16x4,16x(2*2)x1>"       [1024,16,4] [16,4,1]


-- tests evaluators (kernel arg evaluates)
runInitAtomTests :: Opts -> IO ()
runInitAtomTests os = do
  let runArgTestImm :: ShowNum a => Opts -> String -> String -> a -> IO ()
      runArgTestImm os ty arg_expr expect_value = do
        runArgTest
          os
          ("imm. arg: " ++ arg_expr)
          ty
          arg_expr
          (showNum expect_value)

  let sintImmTest arg_expr val_expr = runArgTestImm os "int" arg_expr (val_expr :: Int64)
  let uintImmTest arg_expr val_expr = runArgTestImm os "uint" arg_expr (val_expr :: Word64)
  let floatImmTest arg_expr val_expr = runArgTestImm os "float" arg_expr (val_expr :: Float)
      vectorImmTest :: String -> String -> String -> IO ()
      -- -D EXPECT=(int2)(1,2) fails, so to expand in CPP correctly
      -- so use runArgTestF
      vectorImmTest ty arg_expr val_expr = runArgTestVec os ("imm. arg: " ++ arg_expr) ty arg_expr val_expr
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
  --
  uintImmTest "~0xAAAA&0xFFFF"       (complement 0xAAAA .&. 0xFFFF) -- 0x5555
  --
  vectorImmTest "int2"    "(int2)(1+2,2)"          "(int2)(3,2)"
  vectorImmTest "int2"    "(int2)(1+2,1)"          "(int2)(3,1)"
  vectorImmTest "float2"  "(float2)M_PI"           "(float2)(M_PI,M_PI+0.0f)" -- ensure LPAREN IDENTIFIER RPAREN works
  vectorImmTest "float2"  "(float2)(1.0f+2,4.0f)"  "(float2)(3, 4.0f)" -- conversion


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


runArgTest :: Opts -> String -> String -> String -> String -> IO ()
runArgTest os arg_desc arg_type arg_expr show_expect_value = do
  let script :: String
      script =
        "let R=0:w\n" ++
        "let V=0:w\n" ++
        "#" ++ show (oDeviceIndex os) ++
          "`tests/args.cl[-DT=" ++ arg_type ++ " -DEXPECT=" ++ show_expect_value ++ "]" ++
          "`test<1>(R," ++ arg_expr ++ ",V)\n" ++
        "diff<" ++ arg_type ++ ">(" ++ show_expect_value ++ ",V)\n"
        -- "diff<" ++ arg_type ++ ">(0,R)\n"
  runScript os arg_desc (mShouldExit 0) script

-- runArgTest with "-D EXPECT=(int2)(1,2)" fails, so to expand in CPP correctly
-- so use runArgTestF
runArgTestVec :: Opts -> String -> String -> String -> String -> IO ()
runArgTestVec os arg_desc arg_type arg_expr show_expect_value = do
  let source :: String
      source =
        "kernel void test(\n"++
        "  global int *error,\n"++
        "  " ++ arg_type ++ " arg,\n"++
        "  global " ++ arg_type ++ " *val)\n"++
        "{\n"++
        "  val[0] = arg;\n"++
        "  if (any(arg != " ++ show_expect_value ++ ")) {\n"++
        "    error[0] = 1;\n"++
        "  }\n"++
        "}\n"

      temp_cl_file = "last.cl"

      script :: String
      script =
        "let R=0:w\n" ++
        "let V=0:w\n" ++
        "#" ++ show (oDeviceIndex os) ++"`" ++ temp_cl_file ++ "`test<1>(R," ++ arg_expr ++ ",V)\n" ++
        "diff<" ++ arg_type ++ ">(" ++ show_expect_value ++ ",V)\n"
        -- "diff<" ++ arg_type ++ ">(0,R)\n"
  writeFile "last.cl" source
  --
  runScript os arg_desc (mShouldExit 0) script
  --
  removeFile temp_cl_file


-------------------------------------------------------------------------------
--
runDiffUniformTestMatch :: Opts -> IO ()
runDiffUniformTestMatch os = do
  runScript os "diff uniform match" (mShouldExit 0) $
    "let B=44:rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add<8>(B,16)\n" ++
    "diff(44+16,B)\n" ++
    ""
  runScript os "diff uniform match (NaN)" (mShouldExit 0) $
    "let A=(0.0/0.0):rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initClean<8>(A,0.0/0.0)\n" ++
    "diff<float>(0.0/0.0,A)\n" ++
    ""

runDiffUniformTestMatchFuzzy :: Opts -> IO ()
runDiffUniformTestMatchFuzzy os = do
  runScript os "diff uniform fuzzy match" (mShouldExit 0) $
    "let A=0:rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initDirty<8>(A,0.0000,0.0009)\n" ++
    "diff<float,0.0010>(0.0000,A)\n" ++
    ""

runDiffUniformTestMismatch :: Opts -> IO ()
runDiffUniformTestMismatch os = do
  runScript os "diff uniform mismatch (negative)" (mShouldExit 1 .&&. mStderrContains "element 7") $
    "let B=44:rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/add_buggy.cl[-DT=int]`add<8>(B,16)\n" ++
    "diff(44+16,B)\n" ++
    ""

runDiffUniformTestMismatchFuzzy :: Opts -> IO ()
runDiffUniformTestMismatchFuzzy os = do
  runScript os "diff uniform mismatch fuzzy (negative 1)" (mShouldExit 1 .&&. mStderrContains "element 3") $
    "let A=0:rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initDirty<8>(A,1.000,0.0010)\n" ++
    "diff<float,0.0009>(1.000,A)\n" ++
    ""
  --
  runScript os "diff uniform mismatch fuzzy (negative NaN)" (mShouldExit 1 .&&. mStderrContains "element 3") $
    "let A=0:rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initDirty<8>(A,0.000,0.0/0.0)\n" ++
    "diff<float,0.000>(0.000,A)\n" ++
    ""

runDiffSurfaceTestMatchImm :: Opts -> IO ()
runDiffSurfaceTestMatchImm os = do
  let script :: String
      script =
        "let A=seq(120):rw\n" ++ -- 120-127
        "let B=seq(122):rw\n" ++ -- 122-129
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add<8>(A,1)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add<8>(B,-1)\n" ++
        "diff(seq(121):rw,A)\n" ++ -- immediate reference value
        ""
  runScript os "diff surface matching imm" (mShouldExit 0) script

runDiffSurfaceTestMatchVar :: Opts -> IO ()
runDiffSurfaceTestMatchVar os = do
  let script :: String
      script =
        "let A=seq(120):rw\n" ++ -- 120-127
        "let B=seq(122):rw\n" ++ -- 122-129
        "let C=seq(121):r\n" ++ -- 121-128
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add<8>(A,1)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add<8>(B,-1)\n" ++
        "diff(A,A)\n" ++ -- identity compare (ILLEGAL because it double maps A, which is non-deterministic)
        "diff(A,B)\n" ++ -- transitive compare
        "diff(C,A)\n" ++ -- immediate reference value (indirect)
        ""
  runScript os "diff surface matching var" (mShouldExit 0) script

runDiffSurfaceTestMismatchVar :: Opts -> IO ()
runDiffSurfaceTestMismatchVar os = do
  let script :: String
      script =
        "let A=seq(0):rw\n" ++
        "let B=seq(0):rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add<8>(A,1)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add_buggy.cl[-DT=int]`add<8>(B,1)\n" ++
        "diff(A,B)\n" ++
        ""
  runScript
    os "diff surface mismatching var" (mShouldExit 1 .&&. mStderrContains "element 7") script
  runScriptExtra ["-Xno-exit-on-diff-fail"]
    os "diff surface mismatch (no exit)" (mShouldExit 0 .&&. mStderrContains "element 7") script

runDiffSurfaceTestMismatchImm :: Opts -> IO ()
runDiffSurfaceTestMismatchImm os = do
  let script :: String
      script =
        "let A=seq(0):rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add_buggy.cl[-DT=int]`add<8>(A,1)\n" ++
        "diff(seq(1):r,A)\n" ++
        ""
  runScript os "diff surface mismatching imm" (mShouldExit 1 .&&. mStderrContains "element 7") script

-------------------------------------------------------------------------------
--
runPrintCommandTests :: Opts -> IO ()
runPrintCommandTests os = do
  let script_s :: String
      script_s =
        "let B=seq():rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uint]`add<8>(B,16)\n" ++
        "print(B)\n" ++
        "print<int>(B)\n" ++
        "print<int,4>(B)\n" ++
        ""
  let has_lines_s = mHasAllLines $
        "0000:  0x00000010  0x00000011  0x00000012  0x00000013  0x00000014  0x00000015  0x00000016  0x00000017\n" ++
        "0000:            16            17            18            19            20            21            22            23\n" ++
        "0000:            16            17            18            19\n" ++
        "0010:            20            21            22            23\n" ++
        ""
  runScript os "print command test (int)" (mShouldExit 0 .&&. has_lines_s) script_s
  -----------------------------------------------------------------------------
  let script_v :: String
      script_v =
        "let B=(uint2)(1,2):rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uint2]`add<8>(B,(uint2)(1,2))\n" ++
        "print(B)\n" ++
        "print<int2,4>(B)\n" ++
        ""
  let has_lines_v = mHasAllLines $
        "0000:  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)\n" ++
        "0000:  (           2,           4)  (           2,           4)  (           2,           4)  (           2,           4)\n" ++
        "0020:  (           2,           4)  (           2,           4)  (           2,           4)  (           2,           4)\n" ++
        ""
  runScript os "print command test (int2)" (mShouldExit 0 .&&. has_lines_v) script_v


runPrintAttributeTests :: Opts -> IO ()
runPrintAttributeTests os = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uint]`add<8>(seq(2,2):rwPp4,16)\n"
  let has_lines = mHasAllLines $
        "0000:  0x00000002  0x00000004  0x00000006  0x00000008  0x0000000A  0x0000000C  0x0000000E  0x00000010\n" ++
        "0000:  0x00000012  0x00000014  0x00000016  0x00000018\n" ++
        "0010:  0x0000001A  0x0000001C  0x0000001E  0x00000020\n" ++
        ""
  runScript os "print attribute test" (mShouldExit 0 .&&. has_lines) script


runInputVariableTests :: Opts -> IO ()
runInputVariableTests os = do
  runInputVariableTestPos os
  runInputVariableTestNeg1 os

runInputVariableTestPos :: Opts -> IO ()
runInputVariableTestPos os = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/${A}dd.cl[-DT=int]`$ADD<1>(0:w,1)"
  runScriptExtra ["-DA=a","-DADD=add"] os "input variable expansion" (mShouldExit 0) script
runInputVariableTestNeg1 :: Opts -> IO ()
runInputVariableTestNeg1 os = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/${A}dd.cl[-DT=int]`$ADD<1>(0:w,1)"
  runScriptExtra ["-DA=a","-DA=a","-DADD=add"] os "input variable expansion neg. (redef.)"
    (mShouldExit 1 .&&. mStderrContains "input variable redefinition") script

  runScriptExtra ["-DADD=add"] os "input variable expansion neg. (undef.)"
    (mShouldExit 1 .&&. mStderrContains "undefined input variable") script


-------------------------------------------------------------------------------
--
runSequentialAddTest :: Opts -> String -> [String] -> String -> IO ()
runSequentialAddTest os arg_type args result = do
  let script :: String
      script =
        "let B=0:rw\n" ++
        concatMap (\a -> "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl" ++
          "[-DT=" ++ arg_type ++ "]`add<1>(B," ++ a ++ ")\n") args ++
        "diff<" ++ arg_type ++ ">(" ++ result ++ ",B)\n"
  runScript os "sequential add test" (mShouldExit 0) script


-------------------------------------------------------------------------------
--
runSaveTest :: Opts -> IO ()
runSaveTest os = do
  let output_binary :: FilePath
      output_binary = "test.bin"

      script :: String
      script =
        "let B=seq():rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(B,16)\n" ++
        "save(" ++ show output_binary ++ ",B)\n" ++
        ""

      output_file_matches :: Matcher
      output_file_matches _ _ _ = do
        let fail msg = mFail (output_binary ++ ": " ++ msg)
        z <- doesFileExist output_binary
        if not z then fail "output file not found"
          else do
            bs <- BS.readFile output_binary
            if BS.pack (take 8 [16..]) /= bs then fail ("unexpected output:" ++ concatMap (printf " 0x%02X") (BS.unpack bs))
              else mSuccess

  z <- doesFileExist output_binary
  when z $
    removeFile output_binary

  runScript os "save command test" (mShouldExit 0 .&&. output_file_matches) script

  z <- doesFileExist output_binary
  when z $
    removeFile output_binary

runSaveTestNegBadSurf :: Opts -> IO ()
runSaveTestNegBadSurf os = do
  let script :: String
      script =
        "let UNDEF_SURFACE=seq():rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(0:w,16)\n" ++
        "save('test.bin',UNDEF_SURFACE)\n" ++
        ""
  runScript os "save command neg. test (unbound surf)" (mShouldExit 1 .&&. mStderrContains "non-existent surface object") script


-------------------------------------------------------------------------------
-- MEM INIT
runInitConstWithDim :: Opts -> IO ()
runInitConstWithDim os = do
  let script :: String
      script =
        "let A=0:[4*8*2]w\n" ++
        -- first call writes to the first half only
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add<8>(A,3)\n" ++
        -- second call writes to the second half only
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int2]`add<8>(A,(int2)(1,2))\n" ++
        "print<int,8>(A)\n" ++
        "\n"
  let buffer_matches = mHasAllLines $
        "0000:             4             5             4             5             4             5             4             5\n" ++
        "0020:             1             2             1             2             1             2             1             2\n" ++
        ""
  runScript os "init surface with explicit size" (mShouldExit 0 .&&. buffer_matches) script

-------------------------------------------------------------------------------
--
runInitRandomTests :: Opts -> IO ()
runInitRandomTests os = do
  let script_g :: String
      script_g =
        "let A=random<>:rw\n" ++ -- ensure <> empty works
        "let B=random<44>:rw\n" ++ -- ensure same seed generates same sequences
        "let C=random<44>:rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(A,1)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(B,1)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(C,1)\n" ++
        "diff(B,C)\n" ++
        ""
  -- general testing
  runScript os "init random general tests" (mShouldExit 0) script_g

  let randTypeTest :: (Show n,Ord n,Read n) => String -> n -> n -> IO ()
      randTypeTest type_name lo hi = do
        let dispatch buf = "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=" ++ type_name ++ fp_enable_opt ++ "]`add<8>(" ++ buf ++ ",0)\n"
              where fp_enable_opt
                      | type_name == "half"   = " -DENABLE_FP16"
                      | type_name == "double" = " -DENABLE_FP64"
                      | otherwise = ""

            script_t =
              "let B1=random<33>:rw\n" ++
              "let B2=random<33>(" ++ show hi ++ "):rw\n" ++
              "let B3=random<33>(" ++ show lo ++ "," ++ show hi ++ "):rw\n" ++
              dispatch "B1" ++
              dispatch "B2" ++
              dispatch "B3" ++
              "print(B2)\n" ++
              "print(B3)\n" ++
              ""

            valid_output :: Matcher
            valid_output _ out _ = proc 0 (lines out)
              where proc i (ln0:ln1:lns)
                      | "PRINT<" `isPrefixOf` ln0 = allSuffice (drop 1 (words ln1))
                      | otherwise = proc i lns
                      where allSuffice [] = if i == 1 then mSuccess else proc (i+1) lns
                            allSuffice (w:ws) =
                              case reads w of
                                [(x,"")]
                                  | predicate x -> allSuffice ws
                                  | otherwise -> mFail (w ++ ": buffer output violates predicate (" ++ predicate_name ++ ")")
                                _ -> mFail (w ++ ": failed to parse buffer output (" ++ predicate_name ++ ")")
                            predicate = if i == 0 then (<=hi) else (\x -> (x >= lo && x <= hi))
                            predicate_name = if i == 0 then ("(<"++show hi++")") else ("(>=" ++ show lo ++ " && <=" ++ show hi ++ ")")
                    proc i _ = mFail $ "didn't find buffer (" ++ show i ++ ")"

        runScript os ("init random " ++ type_name) (mShouldExit 0 .&&. valid_output) script_t

  randTypeTest "char" (-10) (10 :: Int)
  randTypeTest "uchar" (10) (20 :: Int)
  randTypeTest "short" (-10) (10 :: Int)
  randTypeTest "ushort" (256) (276 :: Int)
  randTypeTest "int" (-10) (10 :: Int)
  randTypeTest "uint" (2^16 + 1) (2^16 + 20 :: Int)
  randTypeTest "long" (-10) (10 :: Int)
  randTypeTest "ulong" (2^32 + 1) (2^32 + 10 :: Integer)
  oup <- readProcess (oClsExe os) ["-l="++show (oDeviceIndex os),"-v"] ""
  let has_fp16 = "cl_khr_fp16" `isInfixOf` oup
  when has_fp16 $
    randTypeTest "half" (-2.0 :: Float) (2.0 :: Float)
  randTypeTest "float" (-2.0 :: Float) (2.0 :: Float)
  let has_fp64 = "cl_khr_fp64" `isInfixOf` oup
  when has_fp64 $
    randTypeTest "double" (-2.0 :: Double) (2.0 :: Double)

  let runNegativeTestLowGtHigh type_name expr = do
        let script_n :: String
            script_n =
              "let A=" ++ expr ++ ":rw\n" ++ -- ensure <> empty works
              "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=" ++ type_name ++ "]`add<8>(A,1)\n" ++
              ""
        runScript os
          ("init random negative test " ++ type_name ++ " " ++ expr)
          (mShouldExit 1 .&&. mStderrContains "low bound > high bound") script_n

  runNegativeTestLowGtHigh "int"   "random(0,-1)"
  runNegativeTestLowGtHigh "float" "random(0,-1)"
  runNegativeTestLowGtHigh "float" "random(-1)"

-------------------------------------------------------------------------------
--
runInitSequenceTests :: Opts -> IO ()
runInitSequenceTests os = do
  let runTest :: (Eq n,Show n,Read n) => String -> String -> [n] -> IO ()
      runTest type_name init ns = do
        let script :: String
            script =
              "let A=" ++ init ++ ":rw\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=" ++ type_name ++ "]`add<4>(A,0)\n" ++
              "print(A)\n" ++
              ""
            matches = mShouldExit 0 .&&. mPrintMatchesValues 0 ns
        runScript os ("init seq test " ++ type_name ++ " " ++ init)  matches script
  -- general testing
  runTest "int"    "seq()"     [0,1,2,3]
  runTest "int"    "seq(0,4)"  [0,4,8,12]
  runTest "int"    "seq(0,-1)" [0,-1,-2,-3]
  runTest "float"  "seq()"     [0,1,2,3 :: Float]


-- let A=file.bin
runInitFileBinTest :: Opts -> IO ()
runInitFileBinTest os = do
  let bin_file = "init.bin"
  BS.writeFile bin_file $ BS.pack (take 8 [12..])
  let script :: String
      script =
        -- all forms are equivalent
        "let A=file('" ++ bin_file ++ "'):rw\n" ++
        "let B=file<>('" ++ bin_file ++ "'):rw\n" ++
        "let C=file<bin>('" ++ bin_file ++ "'):rw\n" ++
        --
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(A,1)\n" ++
        "diff(seq(13):r,A)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(B,1)\n" ++
        "diff(seq(13):r,B)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add<8>(C,1)\n" ++
        "diff(seq(13):r,C)\n" ++
        "\n" ++
        -- with the initializer inline
        "let D=2:rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`addv<8>(D,file<bin>(" ++ show bin_file ++ "):r)\n" ++
        "diff(seq(14):r,D)\n" ++
        ""
  runScript os "init by file binary" (mShouldExit 0) script
  removeFile bin_file

-------------------------------------------------------------------------------
--
runSyntaxErrorTestCL :: Opts -> IO ()
runSyntaxErrorTestCL os = do
  let script =
        "let A=0:rw\n" ++
        -- don't set the -T
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl`add<8>(A,2)\n" ++
        "print<int,8>(A)\n" ++
        ""
  runScript
    os
    "syntax error OpenCL source text"
    ((mShouldExit 1 .&&. mStderrContains "failed to build source"))
    script
--
runSyntaxErrorTestCLS :: Opts -> IO ()
runSyntaxErrorTestCLS os = do
  let script =
        "lt A=0:rw\n" ++
        -- don't set the -T
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl`add<8>(A,2)\n" ++
        "print<int,8>(A)\n" ++
        ""
  runScript
    os
    "syntax error CLS source text"
    (mShouldExit 1)
    script



-------------------------------------------------------------------------------
--
runSlmStaticTest :: Opts -> IO ()
runSlmStaticTest os = do
  let script_s :: String
      script_s =
        "let I=random<33>(0,15):[16]rw // 16 uchar's\n" ++
        "let H3=0:[3*4]rw // 3 integers\n" ++
        "print<char>(I)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/histogram.cl[-DHIST_SIZE=3]`histogram_s<16,4>(I,H3)\n" ++
        "print<int>(H3)\n" ++
        ""
      passed = mShouldExit 0
  runScript os "slm static allocation" passed script_s


runSlmDynamicTest :: Opts -> IO ()
runSlmDynamicTest os = do
  let script_d :: String
      script_d =
        "let I=random<33>(0,15):[16]rw // 16 uchar's\n" ++
        "let H3=0:[3*4]rw // 3 integers\n" ++
        "print<char>(I)\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/histogram.cl`histogram_d<16,4>(I,H3,(3*sizeof(int)),3)\n" ++
        "print<int>(H3)\n" ++
        ""
      passed = mShouldExit 0
  runScript os "slm dynamic allocation" passed script_d


-------------------------------------------------------------------------------
--
runImageTests :: Opts -> IO ()
runImageTests os = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/images.cl`flip_channels<4x4>(image<rgba,un8,4x4>:Sw,image<rgba,un8>(\"tests/test_image.ppm\"):r)" ++
        ""
      --
      passed =
        mShouldExit 0 .&&.
        mFileExists "cls-surface-00.ppm" .&&.
        mFileHasAllLines
          "cls-surface-00.ppm"
          ("P3\n"++
           "4  4\n"++
           "255\n"++
           "  0   0 255    0 255   0  255   0   0  192 192 192\n"++
           "  0   0   0  255 255 255    0   0   0  255 255 255\n"++
           "  0 255 255  255   0 255  255 255   0  128 128 128\n"++
           "255 255 255    0   0   0  255 255 255    0   0   0")
      --
  runScript os "images (flip channels)" passed script
  -- TODO: test all the formats

-------------------------------------------------------------------------------
--

-------------------------------------------------------------------------------
--
runContextIdentifierTest :: Opts -> IO ()
runContextIdentifierTest os = do
  let script :: String
      script =
        "let A=1:rw\n" ++
        "let B=1:rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":a`tests/add.cl[-DT=int]`add<8>(A,2)\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":a`tests/add.cl[-DT=int]`add<8>(A,3)\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":b`tests/add.cl[-DT=int]`add<8>(B,2)\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":b`tests/add.cl[-DT=int]`add<8>(B,3)\n" ++
        "diff(6,A)\n" ++
        "diff(6,B)\n" ++
        "diff(A,B)\n" ++
        ""
  runScript os "queue identifiers" (mShouldExit 0) script

runContextIdentifierNegativeTest :: Opts -> IO ()
runContextIdentifierNegativeTest os = do
  let script :: String
      script =
        "let B=44:rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":a`tests/add.cl[-DT=int]`add<8>(B,16)\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":b`tests/add.cl[-DT=int]`add<8>(B,16)\n" ++
        "diff(44+16,B)\n" ++
        ""
      -- the latter can happen if the surface object belongs to a different context
      passed =
        mShouldExit 1 .&&.
          (mStderrContains "memory object used across cl_context" .||. mStderrContains "CL_INVALID_MEM_OBJECT")
  runScript os "queue identifiers negative test" passed script



-------------------------------------------------------------------------------
--
runIntelBinaryTests :: Opts -> IO ()
runIntelBinaryTests os = do
  -- ...
  -- DEVICE[1]: Intel(R) HD Graphics 530                         GPU     OpenCL C 2.0
  -- DEVICE[2]: Intel(R) Core(TM) i5-6600K CPU @ 3.50GHz         CPU     OpenCL C 2.0
  dev_out <- readProcess (oClsExe os) ["-l="++show (oDeviceIndex os)] ""

  let intel_hd = "HD Graphics" -- 530, 520, 630 etc..
      bin_ext = ".bin"

  -- create a binary from this device and its driver
  let script_sv =
        "let A=0:rw\n" ++
        -- don't set the -T
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-D T=float]`add<8>(A,2)\n" ++
        "print<float,4>(A)\n" ++
        ""
  --
      program_binary = "add" ++ bin_ext

      save_binary_tag = ("dump program binaries (" ++ program_binary ++ ")")
      load_binary_tag = ("load from program binary (" ++ program_binary ++ ")")

  if not (intel_hd `isInfixOf` dev_out) then do
      skipTest os save_binary_tag "Intel GEN only"
      skipTest os load_binary_tag "Intel GEN only"
    else do
      removeIfExists program_binary
      --
      runScriptWith
        (oClsExe os)
        ["-B"]
        os
        save_binary_tag
        (mShouldExit 0 .&&. mFileExists program_binary)
        script_sv
      --
      --
      let script_ld =
            "let A=seq(0):rw\n" ++
            -- don't set the -T
            "#" ++ show (oDeviceIndex os) ++ "`" ++ program_binary ++ "`add<8>(A,1)\n" ++
            "diff(seq(1):r,A)\n" ++
            ""
      --
      runScript
        os
        load_binary_tag
        (mShouldExit 0)
        script_ld
      --
      removeIfExists program_binary


-------------------------------------------------------------------------------
--
runIntelSpirTests :: Opts -> IO ()
runIntelSpirTests os = do
  -- search for cl_khr_spir
  dev_out <- readProcess (oClsExe os) ["-l="++show (oDeviceIndex os),"-v"] ""
  let dev_has_cl_khr_spirv = any ("cl_khr_spir"`isInfixOf`) (lines dev_out)
  let simple_int = "load simple SPIRV program int"
      simple_float2 = "load simple SPIRV program float2"
      -- complex_test_tag = "load complex SPIRV program"
  if not dev_has_cl_khr_spirv then do
      skipTest os simple_int "cl_khr_spir not supported on this device"
      skipTest os simple_float2 "cl_khr_spir not supported on this device"
    else do
      let script_int =
            "let A=0:w\n" ++
            "#" ++ show (oDeviceIndex os) ++ "`tests/add_int.spv`add<16>(A,2)\n" ++
            "diff(2,A)\n" ++
            ""
      runScript
        os
        simple_int
        (mShouldExit 0)
        script_int

      let script_float2 =
            "let A=0:w\n" ++
            "#" ++ show (oDeviceIndex os) ++ "`tests/add_float2.spv`add<16>(A,(float2)(1,2))\n" ++
            "diff((float2)(1,2),A)\n" ++
            ""
      runScript
        os
        simple_float2
        (mShouldExit 0)
        script_float2





-------------------------------------------------------------------------------
--
removeIfExists :: FilePath -> IO ()
removeIfExists fp = do
  z <- doesFileExist fp
  when z $ removeFile fp

runScript :: Opts -> String -> Matcher -> String -> IO ()
runScript = runScriptExtra []
runScriptExtra :: [String] -> Opts -> String -> Matcher -> String -> IO ()
runScriptExtra extra_args os = runScriptWith (oClsExe os) extra_args os

skipTest :: Opts -> String -> String -> IO ()
skipTest os tag why = do
  emitTestLabel tag
  putStrYellow ("  SKIPPED (" ++ why ++ ")\n")
  modifyIORef (oResults os) $ \(total,passed,skipped) -> (total + 1, passed, skipped + 1)

emitTestLabel :: String -> IO ()
emitTestLabel tag = putStr $ printf "%-64s" (tag ++ ":  ")

runScriptWith :: FilePath -> [String] -> Opts -> String -> Matcher -> String -> IO ()
runScriptWith exe extra_opts os tag match script = do
  emitTestLabel tag
  let args = default_arguments ++ ["-e",script] ++ extra_opts
  when (oVerbosity os > 1) $
    writeFile "verbose.cls" $
      "-- " ++ show args ++ "\n" ++
      script
  (ec,out,err) <- readProcessWithExitCode exe args ""
  r <- match ec out err
  let fmtArgList [] = "" -- drop -e arg
      fmtArgList ("-e":_:as) = fmtArgList as
      fmtArgList (a:as) = a ++ " " ++ fmtArgList as
  let emitOutput =
        putStrLn $
          "\n\n" ++
          "******************************************\n" ++
          -- "% " ++ oClsExe os ++ " " ++ intercalate " " args ++ " -e ...\n" ++
          -- script ++
          -- "\n" ++
          "***OUT***:\n" ++
          out ++ "\n" ++
          "***ERR***:\n" ++
          err ++ "\n" ++
          "******************************************"
  case r of
    Nothing -> do
      putStrGreen "  PASSED\n"
      when (oDebug os) $
        emitOutput
      modifyIORef (oResults os) $ \(total,passed,skipped) -> (total + 1,passed + 1,skipped)
    Just msg -> do
      putStrRed "  FAILED\n"
      putStrRed $ msg ++ "\n"
      emitOutput
      hFlush stdout
      let failed_cls = "failed.cls"
      writeFile failed_cls script
      writeFile "failed.out" out
      writeFile "failed.err" err
      writeFile "failed.exited" (show ec)
      when (oFailFast os) $
        die $ "test failed (run " ++ fmtArgList args ++ " " ++ failed_cls ++ ")"
      modifyIORef (oResults os) $ \(total,passed,skipped) -> (total + 1,passed,skipped)



-------------------------------------------------------------------------------
-- MATCHER EDSL
--                          out        err         maybe an error message
type Matcher = ExitCode -> String -> String -> IO (Maybe String)

-- TODO: should rename mFail, mSuccess to something else
-- because they aren't matcher functions
mFail :: String -> IO (Maybe String)
mFail = return . Just

mSuccess :: IO (Maybe String)
mSuccess = return Nothing

(.&&.) :: Matcher -> Matcher -> Matcher
(.&&.) = combineAND
infixl 2 .&&.
(.||.) :: Matcher -> Matcher -> Matcher
(.||.) = combineOR
infixl 1 .||.

combineAND :: Matcher -> Matcher -> Matcher
combineAND m1 m2 = \ec out err -> do
  mr <- m1 ec out err
  case mr of
    Just x -> mFail x
    Nothing -> m2 ec out err
combineOR :: Matcher -> Matcher -> Matcher
combineOR m1 m2 = \ec out err -> do
  mr <- m1 ec out err
  case mr of
    Just x -> m2 ec out err
    Nothing -> mSuccess


-- ensures that output has all the following lines somewhere in the output
mHasAllLines :: String -> Matcher
mHasAllLines = foldl (.&&.) (\_ _ _ -> mSuccess) . map mHasLine . lines

mHasLine :: String -> Matcher
mHasLine ln _ out _
  | words ln `elem` map words (lines out) = mSuccess
  | otherwise =
    mFail $ "failed to find line with words: " ++ ln ++ "\n" ++
      show (words ln) ++ " in output\n" ++
      "===" ++ "\n" ++
      concatMap (\ln -> show (words ln) ++ "\n") (lines out)

mFileHasAllLines :: FilePath -> String -> Matcher
mFileHasAllLines fp inp = \_ _ _ -> do
  z <- doesFileExist fp
  if not z then mFail (fp ++ ": file does not exist")
    else do
      -- [[String]]
      output_file_line_words <- map words . lines <$> readFile fp
      let checkLine [] = mSuccess
          checkLine (ln:lns)
            | words ln`elem`output_file_line_words = checkLine lns
            | otherwise = mFail ("unable to find reference line in output:\n" ++ ln)
      checkLine (lines inp)

mStderrContains :: String -> Matcher
mStderrContains ss _ _ err =
  if ss `isInfixOf` err then mSuccess
    else mFail ("expected " ++ show ss ++ " in output")

mShouldExit :: Int ->  Matcher
mShouldExit expected_ec = \ec out err ->
    case ec of
      ExitFailure ec -> exited ec
      ExitSuccess -> exited 0
  where exited ec
          | ec == expected_ec = mSuccess
          | otherwise = mFail $ "test didn't exit " ++ show expected_ec ++ " (exited " ++ printf "0x%X" (fromIntegral ec :: Word32) ++ ")"

mFileExists :: FilePath -> Matcher
mFileExists path = \_ _ _ -> do
  z <- doesFileExist path
  if z then mSuccess
    else mFail (path ++ ": file not found after run")

mPrintMatchesValues :: (Show n,Read n,Eq n) => Int -> [n] -> Matcher
mPrintMatchesValues pr_ix ns _ out _ = proc 0 (lines out)
  where proc ix (ln0:ln1:lns)
          | "PRINT<" `isPrefixOf` ln0 =
          if ix  < pr_ix then proc (ix+1) lns -- skip it
            else match ns (drop 1 (words ln1))
        proc ix [] = mFail ("failed to find PRINT<..> instance " ++ show pr_ix ++ " in output")

        match (n:ns) (w:ws) =
          case reads w of
            [(x,"")] ->
              if x /= n then mFail (w ++ ": mismatch on element (expected " ++ show n ++ ")")
                else match ns ws
            _ -> mFail (w ++ ": parse error in PRINT elements")
        match [] [] = mSuccess
        match _  _ = mFail "wrong number of elements in output"

