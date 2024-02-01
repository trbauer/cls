import Control.Monad
import Control.Concurrent
import Control.Exception
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

-- TODO: - parallelism (handle temp files, files written on errors)
--

data Opts =
  Opts {
    oClsExe :: !FilePath
  , oDeviceIndex :: !Int
  , oFailFast :: !Bool
  , oFilters :: ![String]
  , oListOnly :: !Bool
  , oPrecheck :: !Bool
  , oVerbosity :: !Int
  }

data Test =
  Test {
    tLabel :: !String
  , tDescription :: String
  , tBody :: !(TestContext -> IO ())
  }
instance Show Test where
  show t =
    "Test {tLabel = " ++
       show (tLabel t) ++
    ", tDescription = " ++ show (tDescription t) ++
    ", ...}"

type TestContext = IORef (IO (),TestResult) -- deferred_io, failed, skipped
data TestResult =
      TestResultNOTSTARTED
    | TestResultPASSED
    | TestResultSKIPPED !String
    | TestResultFAILED
    | TestResultEXCEPTION
  deriving (Show,Eq)
trIsSkipped :: TestResult -> Bool
trIsSkipped tr =
  case tr of
    TestResultSKIPPED _ -> True
    _ -> False

tcIO :: TestContext -> IO () -> IO ()
tcIO tc act = modifyIORef tc $ \(dio,st) -> (dio >> act,st)
tcSetTestResult :: TestContext -> TestResult -> IO ()
tcSetTestResult tc r = modifyIORef tc $ \(dio,_) -> (dio,r)
tcSkipTest :: TestContext -> String -> IO ()
tcSkipTest tc why = tcSetTestResult tc (TestResultSKIPPED why)


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
  return $
    Opts {
      oClsExe = cls64_exe
    , oDeviceIndex = d
    , oFailFast = True
    , oFilters = []
    , oListOnly = False
    , oPrecheck = True
    , oVerbosity = 0
    }

build_root :: FilePath
build_root = "builds/vs2022-64/Debug/"
-- build_root = "builds/vs2019-64/Debug/"

cls64_msvc_exe :: FilePath
cls64_msvc_exe = build_root ++ "cls64.exe"
cls32_msvc_exe :: FilePath
cls32_msvc_exe = build_root ++ "cls32.exe"
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
    "individual tests can be run by key (use -f=... to filter)\n" ++
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
      _ | "-f="`isPrefixOf`a -> continue os{oFilters = oFilters os ++ [val_str]}
      "-F" -> continue os{oFailFast = False}
      "-q" -> continue os{oVerbosity = -1}
      "-v" -> continue os{oVerbosity = 1}
      "-v2" -> continue os{oVerbosity = 2}
      _ | a`elem`["-l","--list-only"] -> continue os{oListOnly = True}
      "--no-precheck" -> continue os{oPrecheck = False}
      "-h" -> do
        putStr $
          "  -d=INT         tests using a given device index\n" ++
          "  -exe=PATH      overrides path to cls*.exe\n" ++
          "  --no-precheck  skip prechecks\n" ++
          "  -f=FILTER      adds a filter\n" ++
          "  -l             list tests only\n" ++
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
putStrLnWarning s = putStrYellow (s ++ "\n")
--
putStrRed :: String -> IO ()
putStrRed = putStrV SCA.Red
putStrDarkRed :: String -> IO ()
putStrDarkRed = putStrD SCA.Red
putStrGreen :: String -> IO ()
putStrGreen = putStrV SCA.Green
putStrYellow :: String -> IO ()
putStrYellow = putStrV SCA.Yellow
putStrV c s = do
  SCA.setSGR [SCA.SetColor SCA.Foreground SCA.Vivid c]
  putStr s
  SCA.setSGR [SCA.Reset]
putStrD c s = do
  SCA.setSGR [SCA.SetColor SCA.Foreground SCA.Dull c]
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
runWithOpts os = newIORef [] >>= runWithOptsWithTrs os

runWithOptsWithTrs :: Opts -> IORef [TestResult] -> IO ()
runWithOptsWithTrs os io_trs = body
  where body = do
          oNormalLn os $ "running with " ++ oClsExe os ++ " on DEVICE[" ++ show (oDeviceIndex os) ++ "]"
          --
          ts <- add_tests
          if oListOnly os
            then list_tests ts
            else run_tests ts


        add_tests :: IO [Test]
        add_tests = do
          ior <- newIORef []
          -- MISC
          addSyntaxErrorTestCL os ior
          addSyntaxErrorTestCLS os ior
          ---------------------

          --
          -- ATOM ARG SETTERS
          addInitAtomTests os ior
          --
          -- DIMENSIONS
          addDimensionTests os ior
          --
          -- MUTABILITY
          addSequentialAddTest os ior "int" ["1","3","-2"] "2"
          --
          -- INPUT VARS ($1)
          addInputVariableTests os ior

          --
          -- PRINT
          addPrintCommandTests os ior
          addPrintAttributeTests os ior -- TODO: move to buffer attributes

          --
          -- SAVE
          addSaveTest os ior
          addSaveTestNegBadSurf os ior
          addSaveImageTest os ior

          --
          -- DIFF(U)
          addDiffUniformTestMatch os ior
          addDiffUniformTestMatchFuzzy os ior
          addDiffUniformTestMismatch os ior
          addDiffUniformTestMismatchFuzzy os ior
          -- DIFF(S)
          addDiffSurfaceTestMatchVar os ior
          addDiffSurfaceTestMatchImm os ior
          addDiffSurfaceTestMismatchVar os ior
          addDiffSurfaceTestMismatchImm os ior


          -- MEM INITIALIZERS
          --   CONSTANT INIT
          addInitConstTests os ior
          --   EXPLICIT DIMENSION
          addInitConstWithDimTests os ior
          --
          --   SEQUENCE VARIABLES
          addInitSequenceTests os ior
          --   FINITE SEQUENCE VARIABLES
          addInitFiniteSequenceTests os ior
          --   CYCLE VARIABLES
          addInitCycleTests os ior
          --   RANDOM VARIABLES
          addInitRandomTests os ior

          --
          --   BINARY FILE MEM INITIALIZERS
          addInitFileBinTest os ior
          -- addInitFileTextTest os
          -- addInitFileTextColTests os

          --
          -- SLM
          addSlmDynamicTest os ior
          addSlmStaticTest os ior
          --
          -- IMAGE
          addImageTests os ior

          -- MULTIPLE CONTEXTS
          addContextIdentifierTest os ior
          addContextIdentifierNegativeTest os ior

          -- binary and SPIR support
          addGenericBinaryTests os ior
          addIntelBinaryTests os ior
          addSpirvTests os ior
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

          -----------------------------
          -- sanity check labels
          ts <- readIORef ior
          forM_ ts $ \t -> do
            let lbl_freq :: Int
                lbl_freq = length (filter ((== tLabel t) . tLabel) ts)
            unless (lbl_freq == 1) $
              die $ "INTERNAL ERROR: " ++ show (tLabel t) ++ ": test label is repeated"
          -- sortOn tLabel <$> readIORef ior
          --
          let ts_filtered = filter matches_filter (map tLabel ts)
          when (null ts_filtered) $
            putStrYellow "WARNING" >> putStrLn ": filters match no tests (run with -l)"
          -----------------------------
          reverse <$> readIORef ior

        list_tests :: [Test] -> IO ()
        list_tests ts = do
          forM_ ts $ \t -> do
            when (matches_filter (tLabel t)) $
              putStrLn (printf "%-48s" (tLabel t ++ ": ") ++ tDescription t)

        run_tests :: [Test] -> IO ()
        run_tests ts = do
          oup <- readProcess (oClsExe os) ["-l"] ""
          let isDeviceIx :: String -> Bool
              isDeviceIx = (("DEVICE[" ++ show (oDeviceIndex os) ++ "]")`isInfixOf`)
          case filter isDeviceIx (lines oup) of
            [x] -> oNormalLn os x
            _ ->
              -- this indicates cls did not produce a valid device index for the chosen device
              -- or the tester was given an invalid index
              dieOrError os $
                "device not found on machine, or cls output is malformed:\n" ++
                "OUTPUT: " ++ show oup
          putStrLn "==============================================="
          forM_ ts $ \t -> do
            when (matches_filter (tLabel t)) $ do
              tc <- newIORef (return (),TestResultNOTSTARTED)
              let t_handler :: SomeException -> IO ()
                  t_handler se = do
                    tcIO tc $ putStrRed $ "INTERNAL ERROR: uncaught exception in test: " ++ show se ++ "\n"
                    tcSetTestResult tc TestResultEXCEPTION

              putStr $ printf "%-48s" (tLabel t ++ ": ")
              hFlush stdout
              --
              tBody t tc`catch`t_handler
              (dio,tr) <- readIORef tc
              case tr of
                TestResultPASSED -> putStrGreen "PASSED"
                TestResultSKIPPED why -> putStrYellow ("SKIPPED (" ++ why ++ ")")
                TestResultFAILED -> putStrRed "FAILED"
                TestResultEXCEPTION -> putStrDarkRed "EXCEPTION"
                _ -> putStrDarkRed $ show tr ++ "?"
              putStrLn ""
              -- run the deferred IO, if it fails, that interrupts execution here
              dio
              modifyIORef io_trs (tr:)
              --
              when (oFailFast os && (tr /= TestResultPASSED && not (trIsSkipped tr))) $ do
                putStrRed "stopping due to fail fast (-f)\n"
                exitFailure

        matches_filter :: String -> Bool
        matches_filter t_lbl
          | null (oFilters os) = True
          | otherwise = any (\f -> f`isInfixOf`t_lbl) (oFilters os)

        print_summary :: IO ()
        print_summary = do
          putStrLn "==============================================="
          trs <- readIORef io_trs
          let total = length trs
              passed = length $ filter (== TestResultPASSED) trs
              skipped = length $ filter trIsSkipped trs
          if total == passed then
              putStrGreen ("passed all " ++ show passed ++ " tests\n")
            else if passed + skipped == total then
              putStrYellow ("passed " ++ show passed ++ " (skipped " ++ show skipped ++ ")\n")
            else
              putStrRed ("passed " ++ show passed ++ " of " ++ show total ++ "\n")

        exit :: IO ()
        exit = do
          trs <- readIORef io_trs
          let total = length trs
              passed = length $ filter (== TestResultPASSED) trs
              skipped = length $ filter trIsSkipped trs
          if total == passed + skipped then exitSuccess
            else exitFailure


-------------------------------------------------------------------------------
--
addSyntaxErrorTestCL :: Opts -> IORef [Test] -> IO ()
addSyntaxErrorTestCL os ts_ior = do
  let script =
        "let A=0:rw\n" ++
        -- don't set the -T
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl`add(A,2)<8>\n" ++
        "print<int,8>(A)\n" ++
        ""
  addScript
    os
    ts_ior
    "err.syn.cl"
    "syntax error OpenCL source text"
    ((mShouldExit 1 .&&. mStderrContains "failed to build source"))
    script
--
addSyntaxErrorTestCLS :: Opts -> IORef [Test] -> IO ()
addSyntaxErrorTestCLS os ts_ior = do
  let script =
        "lt A=0:rw\n" ++
        -- don't set the -T
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl`add(A,2)<8>\n" ++
        "print<int,8>(A)\n" ++
        ""
  addScript
    os
    ts_ior
    "err.syn.cls"
    "syntax error CLS source text"
    (mShouldExit 1)
    script


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


-- tests evaluators (kernel arg evaluates)
addInitAtomTests :: Opts -> IORef [Test] -> IO ()
addInitAtomTests os ts_ior = do
  let label_pfx = "init.atom."
  let addArgTestImm :: ShowNum a => Opts -> String -> String -> a -> IO ()
      addArgTestImm os ty arg_expr expect_value =
        addArgTest
          os
          ts_ior
          (label_pfx ++ ty ++ "." ++ arg_expr)
          ("imm. arg: " ++ arg_expr)
          ty
          arg_expr
          (showNum expect_value)

  let sintImmTest arg_expr val_expr = addArgTestImm os "int" arg_expr (val_expr :: Int64)
  let uintImmTest arg_expr val_expr = addArgTestImm os "uint" arg_expr (val_expr :: Word64)
  let floatImmTest arg_expr val_expr = addArgTestImm os "float" arg_expr (val_expr :: Float)
      vectorImmTest :: String -> String -> String -> IO ()
      -- -D EXPECT=(int2)(1,2) fails, so to expand in CPP correctly
      -- so use runArgTestF
      vectorImmTest ty arg_expr val_expr =
        addArgTestVec
          os ts_ior
          (label_pfx ++ ty ++ "." ++ arg_expr) ("test atomic expressions for uniform kernel arguments")
          ty arg_expr val_expr
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

addArgTest :: Opts -> IORef [Test] -> String -> String -> String -> String -> String -> IO ()
addArgTest os ts_ior t_label arg_desc arg_type arg_expr show_expect_value = do
  let script :: String
      script =
        "let R=0:w\n" ++
        "let V=0:w\n" ++
        "#" ++ show (oDeviceIndex os) ++
          "`tests/args.cl[-DT=" ++ arg_type ++ " -DEXPECT=" ++ show_expect_value ++ "]" ++
            "`test(R," ++ arg_expr ++ ",V)<1>\n" ++
        "diff<" ++ arg_type ++ ">(" ++ show_expect_value ++ ",V)\n"
        -- "diff<" ++ arg_type ++ ">(0,R)\n"
  addScript os ts_ior t_label arg_desc (mShouldExit 0) script

-- runArgTest with "-D EXPECT=(int2)(1,2)" fails, so to expand in CPP correctly
-- so use runArgTestF
addArgTestVec :: Opts -> IORef [Test] -> String -> String -> String ->  String -> String -> IO ()
addArgTestVec os ts_ior t_label arg_desc arg_type arg_expr show_expect_value = do
  let temp_cl_file = "last.cl"

      source :: String
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

      script :: String
      script =
        "let R=0:w\n" ++
        "let V=0:w\n" ++
        "#" ++ show (oDeviceIndex os) ++"`" ++ temp_cl_file ++ "`test(R," ++ arg_expr ++ ",V)<1>\n" ++
        "diff<" ++ arg_type ++ ">(" ++ show_expect_value ++ ",V)\n"
        -- "diff<" ++ arg_type ++ ">(0,R)\n"

  let t_setup = writeFile temp_cl_file source
      t_teardown = removeFile temp_cl_file
  --
  addScriptWithSetupTearDown
    t_setup
    t_teardown
    (oClsExe os) -- exe
    []           -- extra_opts
    os
    ts_ior
    t_label
    arg_desc
    (mShouldExit 0)
    script

-------------------------------------------------------------------------------
addDimensionTests :: Opts -> IORef [Test] -> IO ()
addDimensionTests os ts_ior = do
  let addNegative :: String -> IO ()
      addNegative dims = do
        let script :: String
            script =
              "let G=0:w\n" ++
              "let L=0:w\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/dims.cl`dims" ++ dims ++ "(G,L)\n"
        addScript
          os
          ts_ior
          ("dims.syn.neg." ++ dims)
          ("dimension syntax (negative): " ++ dims)
          (mShouldExit 1)
          script
  --
  addNegative "<1x>"
  addNegative "<1024x16xDSF>"
  addNegative "<-4>"
  addNegative "<1024,-4>"
  --
  let addOne :: String -> [Int] -> [Int] -> IO ()
      addOne dims expect_gs expect_ls = do
        let mkDiffStatement sym ds0
              | null ds0 = ""
              | otherwise = "diff((int4)(" ++ intercalate "," (map show ds_padded) ++ ")," ++ sym ++ ")\n"
              where ds_padded = ds0 ++ replicate (3 - length ds0) 1 ++ [0] -- e.g. [256] -> [256,1,1,0]
        let script :: String
            script =
              "let G=0:w\n" ++
              "let L=0:w\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/dims.cl`dims(G,L)" ++ dims ++ "\n" ++
              mkDiffStatement "G" expect_gs ++
              mkDiffStatement "L" expect_ls ++
              ""
        addScript os ts_ior ("dims.syn.pos." ++ dims) ("testing dimension syntax: " ++ dims) (mShouldExit 0) script
  --
  addOne "<1024>"                       [1024]      []
  addOne "<2*512>"                      [1024]      []
  addOne "<1024,nullptr>"               [1024]      []
  addOne "<1024,NULL>"                  [1024]      []
  addOne "<1024,32>"                    [1024]      [32]
  addOne "<1024,2*16>"                  [1024]      [32]
  addOne "<1024,min(g.x/16,128)>"       [1024]      [64]
  --
  addOne "<1024x64>"                    [1024,64]   []
  addOne "<1024 x 64>"                  [1024,64]   []
  addOne "<1024 x64>"                   [1024,64]   []
  addOne "<1024x 64>"                   [1024,64]   []
  addOne "<1024x(g.x/256)>"             [1024, 4]   []
  addOne "<(1024)x (2*32)>"             [1024,64]   []
  addOne "<1024x64,32x1>"               [1024,64]   [32,1]
  addOne "<1024x64,(g.x/256)x(g.y/4)>"  [1024,64]   [4,16]
  --
  addOne "<1024x16x4>"                  [1024,16,4] []
  addOne "<1024x16 x4>"                 [1024,16,4] []
  addOne "<1024x16x 4>"                 [1024,16,4] []
  addOne "<1024x16 x 4>"                [1024,16,4] []
  addOne "<1024x16x(4)>"                [1024,16,4] []
  addOne "<1024x16 x(4)>"               [1024,16,4] []
  addOne "<1024x16x4,16x2x2>"           [1024,16,4] [16,2,2]
  addOne "<1024x16x4,16x(2*2)x1>"       [1024,16,4] [16,4,1]

-------------------------------------------------------------------------------
--
addSequentialAddTest :: Opts -> IORef [Test] -> String -> [String] -> String -> IO ()
addSequentialAddTest os ts_ior arg_type args result = do
  let script :: String
      script =
        "let B=0:rw\n" ++
        concatMap (\a -> "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl" ++
          "[-DT=" ++ arg_type ++ "]`add<1>(B," ++ a ++ ")\n") args ++
        "diff<" ++ arg_type ++ ">(" ++ result ++ ",B)\n"
  addScript
    os ts_ior
    "mut" "tests that successive dispatches apply sequentially"
    (mShouldExit 0) script


-------------------------------------------------------------------------------
--
addInputVariableTests :: Opts -> IORef [Test] -> IO ()
addInputVariableTests os ts_ior = do
  addInputVariableTestPos os ts_ior
  addInputVariableTestNeg os ts_ior

addInputVariableTestPos :: Opts -> IORef [Test] -> IO ()
addInputVariableTestPos os ts_ior = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/${A}dd.cl[-DT=int]`$ADD(0:w,1)<1>"
  addScriptExtra ["-DA=a","-DADD=add"]
    os ts_ior
    "inp.var.pos"
    "tests input variable expansion from command line (e.g. -DA=..)" (mShouldExit 0) script
addInputVariableTestNeg :: Opts -> IORef [Test] -> IO ()
addInputVariableTestNeg os ts_ior = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/${A}dd.cl[-DT=int]`$ADD(0:w,1)<1>"
  addScriptExtra ["-DA=a","-DA=a","-DADD=add"]
    os ts_ior
    "inp.var.neg.redef"
    "input variable expansion negative case: redefinition of variable on the command line"
    (mShouldExit 1 .&&. mStderrContains "input variable redefinition") script

  addScriptExtra ["-DADD=add"]
    os ts_ior
    "inp.var.neg.unbound"
    "input variable expansion negative case: use of an undefined symbol"
    (mShouldExit 1 .&&. mStderrContains "undefined input variable") script


-------------------------------------------------------------------------------
--
addPrintCommandTests :: Opts -> IORef [Test] -> IO ()
addPrintCommandTests os ts_ior = do
  let script_s :: String
      script_s =
        "let B=seq():rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uint]`add(B,16)<8>\n" ++
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
  addScript
    os ts_ior
    "stmt.print.utype" "print command test (int)"
    (mShouldExit 0 .&&. has_lines_s) script_s
  -----------------------------------------------------------------------------
  let script_v :: String
      script_v =
        "let B=(uint2)(1,2):rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uint2]`add(B,(uint2)(1,2))<8>\n" ++
        "print(B)\n" ++
        "print<int2,4>(B)\n" ++
        ""
  let has_lines_v = mHasAllLines $
        "0000:  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)  (0x00000002,0x00000004)\n" ++
        "0000:  (           2,           4)  (           2,           4)  (           2,           4)  (           2,           4)\n" ++
        "0020:  (           2,           4)  (           2,           4)  (           2,           4)  (           2,           4)\n" ++
        ""
  addScript
    os ts_ior
    "stmt.print.vtype" "print command test (int2)"
    (mShouldExit 0 .&&. has_lines_v) script_v


addPrintAttributeTests :: Opts -> IORef [Test] -> IO ()
addPrintAttributeTests os ts_ior = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uint]`add(seq(2,2):rwPp4,16)<8>\n"
  let has_lines = mHasAllLines $
        "0000:  0x00000002  0x00000004  0x00000006  0x00000008  0x0000000A  0x0000000C  0x0000000E  0x00000010\n" ++
        "0000:  0x00000012  0x00000014  0x00000016  0x00000018\n" ++
        "0010:  0x0000001A  0x0000001C  0x0000001E  0x00000020\n" ++
        ""
  addScript
    os ts_ior
    "mattr.print.utype" "print attribute test"
    (mShouldExit 0 .&&. has_lines) script



-------------------------------------------------------------------------------
--
addSaveTest :: Opts -> IORef [Test] -> IO ()
addSaveTest os ts_ior = do
  let output_binary :: FilePath
      output_binary = "test.bin"

      script :: String
      script =
        "let B=seq():rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(B,16)<8>\n" ++
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

  let t_setup = removeIfExists output_binary
      t_teardown = removeIfExists output_binary
  addScriptWithSetupTearDown t_setup t_teardown (oClsExe os) []
    os ts_ior
    "stmt.save.buf.pos" "save command test"
    (mShouldExit 0 .&&. output_file_matches) script


addSaveTestNegBadSurf :: Opts -> IORef [Test] -> IO ()
addSaveTestNegBadSurf os ts_ior = do
  addScript
    os ts_ior
    "stmt.save.buf.neg.badsurf" "save command neg. test (unbound surf)"
    (mShouldExit 1 .&&. mStderrContains "non-existent surface object") $
      "let UNDEF_SURFACE=seq():rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(0:w,16)<8>\n" ++
      "save('test.bin',UNDEF_SURFACE)\n" ++
      ""

-------------------------------------------------------------------------------
-- image saving
addSaveImageTest :: Opts -> IORef [Test] -> IO ()
addSaveImageTest os ts_ior = do
  let mkScript :: String -> String
      mkScript tystr =
        "let A=0:w\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/save-image.cl`save_" ++ tystr ++ "(A)<8x8>\n" ++
        "save_image<" ++ tystr ++ ">('save-image-" ++ tystr ++ "-sut.ppm',A)\n"

      mkSutFile :: String -> FilePath
      mkSutFile tystr = "save-image-" ++ tystr ++ "-sut.ppm"

      mkOutputMatcher :: String -> Matcher
      mkOutputMatcher tystr _ _ _ = do
        let ref_file = "tests/save-image-" ++ tystr ++ "-ref.ppm"
            sut_file = mkSutFile tystr
        let fail msg = mFail (ref_file ++ ": " ++ msg)
        zr <- doesFileExist ref_file
        zs <- doesFileExist sut_file
        if not zr then mFail (ref_file ++ ": output file not found")
          else if not zs then mFail (sut_file ++ ": INTERNAL ERROR: reference file not found")
          else do
            bs1 <- BS.readFile sut_file
            bs2 <- BS.readFile ref_file
            if bs1 /= bs2 then mFail (sut_file ++ ": mismatch in image output")
              else mSuccess

  let t_setup0 = removeIfExists (mkSutFile "uchar4")
      t_teardown0 = removeIfExists (mkSutFile "uchar4")
  addScriptWithSetupTearDown t_setup0 t_teardown0 (oClsExe os) [] os ts_ior
    "stmt.save.img.pos.uchar4" "save image command test (uchar4)"
    (mShouldExit 0 .&&. mkOutputMatcher "uchar4") (mkScript "uchar4")
  --
  let t_setup1 = removeIfExists (mkSutFile "uchar4")
      t_teardown1 = removeIfExists (mkSutFile "uchar4")
  addScriptWithSetupTearDown t_setup1 t_teardown1 (oClsExe os) []
    os ts_ior
    "stmt.save.img.pos.float4" "save image command test (float4)"
    (mShouldExit 0 .&&. mkOutputMatcher "float4") (mkScript "float4")

-------------------------------------------------------------------------------
--
addDiffUniformTestMatch :: Opts -> IORef [Test] -> IO ()
addDiffUniformTestMatch os ts_ior = do
  addScript os ts_ior "stmt.diffu.pos.basic" "diff uniform match" (mShouldExit 0) $
    "let B=44:rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add(B,16)<8>\n" ++
    "diff(44+16,B)\n" ++
    ""
  addScript
    os ts_ior
    "stmt.diffu.pos.nan" "diff uniform match (NaN)"
    (mShouldExit 0) $
      "let A=(0.0/0.0):rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initClean(A,0.0/0.0)<8>\n" ++
      "diff<float>(0.0/0.0,A)\n" ++
      ""

addDiffUniformTestMatchFuzzy :: Opts -> IORef [Test] -> IO ()
addDiffUniformTestMatchFuzzy os ts_ior = do
  addScript os ts_ior
    "stmt.diffu.pos.fuzzy" "diff uniform fuzzy match"
    (mShouldExit 0) $
      "let A=0:rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initDirty(A,0.0000,0.0009)<8>\n" ++
      "diff<float,0.0010>(0.0000,A)\n" ++
      ""

addDiffUniformTestMismatch :: Opts -> IORef [Test] -> IO ()
addDiffUniformTestMismatch os ts_ior = do
  addScript os ts_ior
    "stmt.diffu.neg.basic" "diff uniform mismatch (negative)"
    (mShouldExit 1 .&&. mStderrContains "element 7") $
      "let B=44:rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add_buggy.cl[-DT=int]`add(B,16)<8>\n" ++
      "diff(44+16,B)\n" ++
      ""

addDiffUniformTestMismatchFuzzy :: Opts -> IORef [Test] -> IO ()
addDiffUniformTestMismatchFuzzy os ts_ior = do
  addScript
    os ts_ior
    "stmt.diffu.neg.fuzzy" "diff uniform mismatch fuzzy (negative 1)"
    (mShouldExit 1 .&&. mStderrContains "element 3") $
      "let A=0:rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initDirty(A,1.000,0.0010)<8>\n" ++
      "diff<float,0.0009>(1.000,A)\n" ++
      ""
  --
  addScript
    os ts_ior
    "stmt.diffu.neg.nanfuzzy" "diff uniform mismatch fuzzy (negative NaN)"
    (mShouldExit 1 .&&. mStderrContains "element 3") $
      "let A=0:rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/diff.cl`initDirty(A,0.000,0.0/0.0)<8>\n" ++
      "diff<float,0.000>(0.000,A)\n" ++
    ""


addDiffSurfaceTestMatchImm :: Opts -> IORef [Test] -> IO ()
addDiffSurfaceTestMatchImm os ts_ior = do
  addScript os ts_ior
    "stmt.diffs.pos.immsurf" "diff surface matching immediate surface"
    (mShouldExit 0) $
      "let A=seq(120):rw\n" ++ -- 120-127
      "let B=seq(122):rw\n" ++ -- 122-129
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add(A,1)<8>\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add(B,-1)<8>\n" ++
      "diff(seq(121):rw,A)\n" ++ -- immediate reference value
      ""


addDiffSurfaceTestMatchVar :: Opts -> IORef [Test] -> IO ()
addDiffSurfaceTestMatchVar os ts_ior = do
  addScript os ts_ior
    "stmt.diffs.pos.var" "diff surface matching var"
    (mShouldExit 0) $
      "let A=seq(120):rw\n" ++ -- 120-127
      "let B=seq(122):rw\n" ++ -- 122-129
      "let C=seq(121):r\n" ++ -- 121-128
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add(A,1)<8>\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add(B,-1)<8>\n" ++
      "diff(A,A)\n" ++ -- identity compare (ILLEGAL because it double maps A, which is non-deterministic)
      "diff(A,B)\n" ++ -- transitive compare
      "diff(C,A)\n" ++ -- immediate reference value (indirect)
      ""

addDiffSurfaceTestMismatchVar :: Opts -> IORef [Test] -> IO ()
addDiffSurfaceTestMismatchVar os ts_ior = do
  let script :: String
      script =
        "let A=seq(0):rw\n" ++
        "let B=seq(0):rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add(A,1)<8>\n" ++
        "#" ++ show (oDeviceIndex os) ++ "`tests/add_buggy.cl[-DT=int]`add(B,1)<8>\n" ++
        "diff(A,B)\n" ++
        ""
  addScript
    os ts_ior
    "stmt.diffs.neg.var"
    "diff surface mismatching var"
    (mShouldExit 1 .&&. mStderrContains "element 7") script
  addScriptExtra ["-Xno-exit-on-diff-fail"]
    os ts_ior
    "stmt.diffs.neg.noexit"
    "diff surface mismatch (no exit)"
    (mShouldExit 0 .&&. mStderrContains "element 7") script

addDiffSurfaceTestMismatchImm :: Opts -> IORef [Test] -> IO ()
addDiffSurfaceTestMismatchImm os ts_ior = do
  addScript os ts_ior
    "stmt.diffs.neg.immsurf" "diff surface mismatching imm"
    (mShouldExit 1 .&&. mStderrContains "element 7") $
      "let A=seq(0):rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add_buggy.cl[-DT=int]`add(A,1)<8>\n" ++
      "diff(seq(1):r,A)\n" ++
      ""

-------------------------------------------------------------------------------
-- MEM INIT
addInitConstTests :: Opts -> IORef [Test] -> IO ()
addInitConstTests os ts_ior = do
  let addTest :: (Eq n,Show n,Read n) => String -> String -> [n] -> IO ()
      addTest type_name init ns = do
        let arg_zero
              | isDigit (last type_name) = "(" ++ type_name ++ ")0"
              | otherwise = "0"
        let script :: String
            script =
              "let A=" ++ init ++ ":rw\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=" ++ type_name ++ "]`add(A," ++ arg_zero ++ ")<4>\n" ++
              "print(A)\n" ++
              ""
            matches = mShouldExit 0 .&&. mPrintMatchesValues 0 ns
        addScript
          os ts_ior
          ("mem.init.const." ++ type_name ++ ".(" ++ init ++ ")")
          ("init seq test " ++ type_name ++ " " ++ init)  matches script


  -- general testing
  addTest "int"    "4"   [4,4,4,4 :: Int]
  addTest "int"    "-1"  [-1,-1,-1,-1 :: Int]
  addTest "float"  "-2"  [-2,-2,-2,-2 :: Float]
  -- vector types
  addTest "int2"   "(int2)4"     [4,4, 4,4, 4,4, 4,4 :: Int]
  addTest "int2"   "(int2)(3,4)" [3,4, 3,4, 3,4, 3,4 :: Int]

addInitConstWithDimTests :: Opts -> IORef [Test] -> IO ()
addInitConstWithDimTests os ts_ior = do
  let script :: String
      script =
        "let A=0:[4*8*2]w\n" ++
        -- first call writes to the first half only
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int]`add(A,3)<8>\n" ++
        -- second call writes to the second half only
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=int2]`add(A,(int2)(1,2))<8>\n" ++
        "print<int,8>(A)\n" ++
        "\n"
  let buffer_matches = mHasAllLines $
        "0000:             4             5             4             5             4             5             4             5\n" ++
        "0020:             1             2             1             2             1             2             1             2\n" ++
        ""
  addScript
    os ts_ior
    "mem.init.size.const" "init surface with constant and explicit size"
    (mShouldExit 0 .&&. buffer_matches) script

-------------------------------------------------------------------------------
--
addInitSequenceTests :: Opts -> IORef [Test] -> IO ()
addInitSequenceTests os ts_ior = do
  let addTest :: (Eq n,Show n,Read n) => String -> String -> [n] -> IO ()
      addTest type_name init ns = do
        let arg_zero
              | isDigit (last type_name) = "(" ++ type_name ++ ")0"
              | otherwise = "0"
        let script :: String
            script =
              "let A=" ++ init ++ ":rw\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=" ++ type_name ++ "]`" ++
                "add(A," ++ arg_zero ++ ")<4>\n" ++
              "print(A)\n" ++
              ""
            matches = mShouldExit 0 .&&. mPrintMatchesValues 0 ns
        addScript
          os ts_ior
          ("mem.init.seq." ++ type_name ++ ".(" ++ init ++ ")")
          ("init seq test " ++ type_name ++ " " ++ init)  matches script
  -- general testing
  addTest "int"    "seq()"     [0,1,2,3]
  addTest "int"    "seq(0,4)"  [0,4,8,12]
  addTest "int"    "seq(0,-1)" [0,-1,-2,-3]
  addTest "float"  "seq()"     [0,1,2,3 :: Float]
  -- -- TODO: vector types
  -- -- addTest "(int2)(0,0)" "int2"   "seq(1,2)" [(1 :: Int),2, 3,4, 5,6, 7,8]

-------------------------------------------------------------------------------
--
addInitFiniteSequenceTests :: Opts -> IORef [Test] -> IO ()
addInitFiniteSequenceTests os ts_ior = do
  let addTest :: (Eq n,Show n,Read n) => String -> String -> [n] -> IO ()
      addTest type_name init ns = do
        let arg_zero
              | isDigit (last type_name) = "(" ++ type_name ++ ")0"
              | otherwise = "0"
        let script :: String
            script =
              "let A=" ++ init ++ ":rw\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=" ++ type_name ++ "]`" ++
                "add(A," ++ arg_zero ++ ")<4>\n" ++
              "print(A)\n" ++
              ""
            matches = mShouldExit 0 .&&. mPrintMatchesValues 0 ns
        addScript
          os ts_ior
          ("mem.init.fseq." ++ type_name ++ ".(" ++ init ++ ")")
          ("init seq test " ++ type_name ++ " " ++ init)
          matches script


  -- general testing
  addTest "int"    "fseq(1)"     [(1 :: Int),1,1,1]
  addTest "int"    "fseq(4,0)"   [(4 :: Int),0,0,0]
  addTest "int"    "fseq(0,-1)"  [(0 :: Int),-1,-1,-1]
  addTest "float"  "fseq(0,1,2)" [0,1,2,2 :: Float]
  -- vector types
  addTest "int2"   "fseq(1,2,3)" [(1 :: Int),2, 3,3, 3,3, 3,3]

-------------------------------------------------------------------------------
--
addInitCycleTests :: Opts -> IORef [Test] -> IO ()
addInitCycleTests os ts_ior = do
  let addTest :: (Eq n,Show n,Read n) => String -> String -> [n] -> IO ()
      addTest type_name init ns = do
        let arg_zero
              | isDigit (last type_name) = "(" ++ type_name ++ ")0"
              | otherwise = "0"
        let script :: String
            script =
              "let A=" ++ init ++ ":rw\n" ++
              "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=" ++ type_name ++ "]`" ++
                "add(A," ++ arg_zero ++ ")<4>\n" ++
              "print(A)\n" ++
              ""
            matches = mShouldExit 0 .&&. mPrintMatchesValues 0 ns
        addScript
          os ts_ior
          ("mem.init.cyc." ++ type_name ++ ".(" ++ init ++ ")")
          ("init cyc test " ++ type_name ++ " " ++ init)
          matches script
  -- general testing
  addTest "int"    "cyc(12)"  [12,12,12,12]
  addTest "int"    "cyc(1,-1)" [1,-1,1,-1]
  addTest "float"  "cyc(-2,2)" [-2,2,-2,2 :: Float]
  -- -- TODO: vector types
  -- -- addTest "(int2)(0,0)" "int2"   "cyc(1,2,3)" [(1 :: Int),2, 3,1, 2,3, 1,2]


-------------------------------------------------------------------------------
--
addInitRandomTests :: Opts -> IORef [Test] -> IO ()
addInitRandomTests os ts_ior = do
  -- general testing
  addScript os ts_ior
    "mem.init.rand"
    "init random general tests"
    (mShouldExit 0) $
      "let A=random<>:rw\n" ++ -- ensure <> empty works
      "let B=random<44>:rw\n" ++ -- ensure same seed generates same sequences
      "let C=random<44>:rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(A,1)<8>\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(B,1)<8>\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(C,1)<8>\n" ++
      "diff(B,C)\n" ++
      ""

  let randTypeTest :: (Show n,Ord n,Read n) => String -> n -> n -> IO ()
      randTypeTest type_name lo hi = do
        let dispatch buf =
                "#" ++ show (oDeviceIndex os) ++ "`" ++
                  "tests/add.cl[-DT=" ++ type_name ++ fp_enable_opt ++ "]`" ++
                    "add(" ++ buf ++ ",0)<8>\n"
              where fp_enable_opt
                      | type_name == "half"   = " -DENABLE_FP16"
                      | type_name == "double" = " -DENABLE_FP64"
                      | otherwise = ""

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

        addScript os ts_ior
          ("mem.init.rand." ++ type_name ++ ".(" ++ show lo ++ "," ++ show hi ++ ")")
          ("init random " ++ type_name)
          (mShouldExit 0 .&&. valid_output) $
            "let B1=random<33>:rw\n" ++
            "let B2=random<33>(" ++ show hi ++ "):rw\n" ++
            "let B3=random<33>(" ++ show lo ++ "," ++ show hi ++ "):rw\n" ++
            dispatch "B1" ++
            dispatch "B2" ++
            dispatch "B3" ++
            "print(B2)\n" ++
            "print(B3)\n" ++
            ""

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
  --
  let addNegativeTestLowGtHigh type_name expr = do
        addScript os ts_ior
          ("mem.init.rand." ++ type_name ++ ".(" ++ expr ++ ")")
          ("init random negative test " ++ type_name ++ " " ++ expr)
          (mShouldExit 1 .&&. mStderrContains "low bound > high bound") $
            "let A=" ++ expr ++ ":rw\n" ++ -- ensure <> empty works
            "#" ++ show (oDeviceIndex os) ++ "`" ++
              "tests/add.cl[-DT=" ++ type_name ++ "]`" ++
              "add(A,1)<8>\n" ++
            ""
  --
  addNegativeTestLowGtHigh "int"   "random(0,-1)"
  addNegativeTestLowGtHigh "float" "random(0,-1)"
  addNegativeTestLowGtHigh "float" "random(-1)"


-- let A=file.bin
addInitFileBinTest :: Opts -> IORef [Test] -> IO ()
addInitFileBinTest os ts_ior = do
  let bin_file = "init.bin"
      t_setup = BS.writeFile bin_file $ BS.pack (take 8 [12..])
      t_teardown = removeIfExists bin_file

  addScriptWithSetupTearDown
    t_setup
    t_teardown
    (oClsExe os) -- exe
    []           -- extra_opts
    os ts_ior
    "mem.init.file.bin"
    "init by file binary"
    (mShouldExit 0) $
    -- all forms are equivalent
    "let A=file('" ++ bin_file ++ "'):rw\n" ++
    "let B=file<>('" ++ bin_file ++ "'):rw\n" ++
    "let C=file<bin>('" ++ bin_file ++ "'):rw\n" ++
    --
    "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(A,1)<8>\n" ++
    "diff(seq(13):r,A)\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(B,1)<8>\n" ++
    "diff(seq(13):r,B)\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`add(C,1)<8>\n" ++
    "diff(seq(13):r,C)\n" ++
    "\n" ++
    -- with the initializer inline
    "let D=2:rw\n" ++
    "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-DT=uchar]`addv(D,file<bin>(" ++ show bin_file ++ "):r)<8>\n" ++
    "diff(seq(14):r,D)\n" ++
    ""


-------------------------------------------------------------------------------
--
addSlmStaticTest :: Opts -> IORef [Test] -> IO ()
addSlmStaticTest os ts_ior = do
  addScript
    os ts_ior
    "slm.static"
    "slm static allocation"
    (mShouldExit 0) $
      "let I=random<33>(0,15):[16]rw // 16 uchar's\n" ++
      "let H3=0:[3*4]rw // 3 integers\n" ++
      "print<char>(I)\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/histogram.cl[-DHIST_SIZE=3]`histogram_s(I,H3)<16,4>\n" ++
      "print<int>(H3)\n" ++
      ""

addSlmDynamicTest :: Opts -> IORef [Test] -> IO ()
addSlmDynamicTest os ts_ior = do
  addScript
    os
    ts_ior
    "slm.dynamic"
    "slm dynamic allocation"
    (mShouldExit 0) $
      "let I=random<33>(0,15):[16]rw // 16 uchar's\n" ++
      "let H3=0:[3*4]rw // 3 integers\n" ++
      "print<char>(I)\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/histogram.cl`histogram_d(I,H3,(3*sizeof(int)),3)<16,4>\n" ++
      "print<int>(H3)\n" ++
      ""

-------------------------------------------------------------------------------
--
addImageTests :: Opts -> IORef [Test] -> IO ()
addImageTests os ts_ior = do
  let script :: String
      script =
        "#" ++ show (oDeviceIndex os) ++ "`tests/images.cl`" ++
          "flip_channels(image<rgba,un8,4x4>:Sw,image<rgba,un8>(\"tests/test_image.ppm\"):r)<4x4>" ++
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
  addScript
    os ts_ior
    "img.transpose.rgba.un8" "images (flip channels)"
    passed
    script
  -- TODO: test all the formats

-------------------------------------------------------------------------------
--
addContextIdentifierTest :: Opts -> IORef [Test] -> IO ()
addContextIdentifierTest os ts_ior = do
  addScript
    os ts_ior
    "ctx.ident.pos"
    "queue identifiers positive test"
    (mShouldExit 0) $
      "let A=1:rw\n" ++
      "let B=1:rw\n" ++
      "#" ++ show (oDeviceIndex os) ++ ":a`tests/add.cl[-DT=int]`add(A,2)<8>\n" ++
      "#" ++ show (oDeviceIndex os) ++ ":a`tests/add.cl[-DT=int]`add(A,3)<8>\n" ++
      "#" ++ show (oDeviceIndex os) ++ ":b`tests/add.cl[-DT=int]`add(B,2)<8>\n" ++
      "#" ++ show (oDeviceIndex os) ++ ":b`tests/add.cl[-DT=int]`add(B,3)<8>\n" ++
      "diff(6,A)\n" ++
      "diff(6,B)\n" ++
      "diff(A,B)\n" ++
      ""
addContextIdentifierNegativeTest :: Opts -> IORef [Test] -> IO ()
addContextIdentifierNegativeTest os ts_ior = do
  let script :: String
      script =
        "let B=44:rw\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":a`tests/add.cl[-DT=int]`add(B,16)<8>\n" ++
        "#" ++ show (oDeviceIndex os) ++ ":b`tests/add.cl[-DT=int]`add(B,16)<8>\n" ++
        "diff(44+16,B)\n" ++
        ""
      -- the latter can happen if the surface object belongs to a different context
      passed =
        mShouldExit 1 .&&.
          (mStderrContains "memory object used across cl_context" .||. mStderrContains "CL_INVALID_MEM_OBJECT")
  addScript
    os ts_ior
    "ctx.ident.neg"
    "queue identifiers negative test (memory object cannot be used across contexts)"
    passed
    script

-------------------------------------------------------------------------------
--
addGenericBinaryTests :: Opts -> IORef [Test] -> IO ()
addGenericBinaryTests os ts_ior = do
  dev_out <- readProcess (oClsExe os) ["-l="++show (oDeviceIndex os)] ""
  let script_sv =
        "let A=0:rw\n" ++
        -- don't set the -T
        "#" ++ show (oDeviceIndex os) ++ "`tests/add.cl[-D T=float]`add<8>(A,2)\n" ++
        "print<float,4>(A)\n" ++
        ""
  --
      (ext,supported)
        | "HD Graphics"`isInfixOf`dev_out = ("bin",True)
        | "NVIDIA"`isInfixOf`dev_out = ("ptx",True)
        | otherwise = (error "unreachable",False)

      program_binary :: String
      program_binary  = "add." ++ ext

      cannot_run :: IO (Maybe String)
      cannot_run
        | not supported = return (Just "only \"HD Graphics\" or \"NVidia\" supported")
        | otherwise = return Nothing

      t_setup = removeIfExists program_binary
      t_teardown = removeIfExists program_binary
  --
  addScriptWithSetupTearDownFilter
    cannot_run
    t_setup
    t_teardown
    (oClsExe os) -- exe
    ["-B"] -- extra opts (save binary)
    os
    ts_ior
    "prog.bin.save"
    ("dump program binaries (" ++ program_binary ++ ")")
    (mShouldExit 0 .&&. mFileExists program_binary)
    script_sv

-- TODO:
--  - add PTX support for kernel arg parsing
addIntelBinaryTests :: Opts -> IORef [Test] -> IO ()
addIntelBinaryTests os ts_ior = do
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

      cannot_run :: IO (Maybe String)
      cannot_run
        | not (intel_hd `isInfixOf` dev_out) = return (Just "Intel GEN only")
        | otherwise = return Nothing

      t_setup = removeIfExists program_binary
      t_teardown = removeIfExists program_binary
  --
  addScriptWithSetupTearDownFilter
    cannot_run
    t_setup     -- setup: remove file if it exists from previous run
    (return ()) -- teardown: nothing (retain file)
    (oClsExe os) -- exe
    ["-B"] -- extra opts (save binary)
    os
    ts_ior
    "prog.bin.intel.save"
    ("dump program binaries (" ++ program_binary ++ ")")
    (mShouldExit 0 .&&. mFileExists program_binary)
    script_sv
  --
  --
  let script_ld =
        "let A=seq(0):rw\n" ++
        -- don't set the -T
        "#" ++ show (oDeviceIndex os) ++ "`" ++ program_binary ++ "`add(A,1)<8>\n" ++
        "diff(seq(1):r,A)\n" ++
        ""
  --
  addScriptWithSetupTearDownFilter
    cannot_run
    (return ()) -- setup: no op
    t_teardown -- teardown: delete after test
    (oClsExe os) -- exe
    []           -- extra_opts
    os
    ts_ior
    "prog.bin.intel.load"
    ("load from program binary (" ++ program_binary ++ ")")
    (mShouldExit 0)
    script_ld

-------------------------------------------------------------------------------
--
addSpirvTests :: Opts -> IORef [Test] -> IO ()
addSpirvTests os ts_ior = do
  -- search for cl_khr_spir
  dev_out <- readProcess (oClsExe os) ["-l="++show (oDeviceIndex os),"-v"] ""
  let dev_has_cl_khr_spirv = any ("cl_khr_spir"`isInfixOf`) (lines dev_out)
  let simple_int = "load simple SPIRV program int"
      simple_float2 = "load simple SPIRV program float2"
      -- complex_test_tag = "load complex SPIRV program"

      cannot_run :: IO (Maybe String)
      cannot_run
        | not dev_has_cl_khr_spirv = return (Just "cl_khr_spir not supported on this device")
        | otherwise = return Nothing

  addScriptWithSetupTearDownFilter
    cannot_run -- only run if dev_has_cl_khr_spirv
    (return ()) -- setup: no op
    (return ()) -- teardown: nop
    (oClsExe os) -- exe
    []           -- extra_opts
    os ts_ior
    "prog.bin.spirv.int"
    simple_int
    (mShouldExit 0) $
      "let A=0:w\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add_int.spv`add(A,2)<16>\n" ++
      "diff(2,A)\n" ++
      ""

  addScriptWithSetupTearDownFilter
    cannot_run -- only run if dev_has_cl_khr_spirv
    (return ()) -- setup: no op
    (return ()) -- teardown: nop
    (oClsExe os) -- exe
    []           -- extra_opts
    os ts_ior
    "prog.bin.spirv.float2"
    simple_float2
    (mShouldExit 0) $
      "let A=0:w\n" ++
      "#" ++ show (oDeviceIndex os) ++ "`tests/add_float2.spv`add(A,(float2)(1,2))<16>\n" ++
      "diff((float2)(1,2),A)\n" ++
      ""

-------------------------------------------------------------------------------
addScript :: Opts -> IORef [Test] -> String -> String -> Matcher -> String -> IO ()
addScript = addScriptExtra []

addScriptExtra :: [String] -> Opts -> IORef [Test] -> String -> String -> Matcher -> String -> IO ()
addScriptExtra extra_args os ts_ior = addScriptWith (oClsExe os) extra_args os ts_ior

addScriptWith :: FilePath -> [String] -> Opts -> IORef [Test] -> String -> String -> Matcher -> String -> IO ()
addScriptWith = addScriptWithSetupTearDown (return ()) (return ())

addScriptWithSetupTearDown ::
  IO () -> IO () -> FilePath -> [String] ->
  Opts -> IORef [Test] ->
  String -> String -> Matcher -> String -> IO ()

addScriptWithSetupTearDown = addScriptWithSetupTearDownFilter (return Nothing)
addScriptWithSetupTearDownFilter ::
  IO (Maybe String) -> IO () -> IO () -> FilePath -> [String] ->
  Opts -> IORef [Test] ->
  String -> String -> Matcher -> String -> IO ()
addScriptWithSetupTearDownFilter cannot_run t_setup t_teardown exe extra_opts os ts_ior t_label t_desc match script =
    modifyIORef ts_ior (Test t_label t_desc t_body:)
  where t_body tc = do
          m_reason <- cannot_run
          case m_reason of
            Just reason -> tcSkipTest tc reason
            Nothing -> do
              t_setup
              t_body2 tc
              t_teardown

        t_body2 :: TestContext -> IO ()
        t_body2 tc = do
          let args = default_arguments ++ ["-e",script] ++ extra_opts
          when (oVerbosity os >= 2) $
            tcIO tc $ do
              putStrLn $ "=== writing debug.cls"
              writeFile "debug.cls" $
                "-- " ++ show args ++ "\n" ++
                script
          (ec,out,err) <- readProcessWithExitCode exe args ""
          r <- match ec out err
          let fmtArgList [] = "" -- drop -e arg
              fmtArgList ("-e":_:as) = fmtArgList as
              fmtArgList (a:as) = a ++ " " ++ fmtArgList as
          let emit_output =
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
              tcSetTestResult tc TestResultPASSED
              when (oDebug os) $ tcIO tc emit_output

            Just msg -> do
              tcSetTestResult tc TestResultFAILED
              tcIO tc $ do
                putStrRed $ msg ++ "\n"
                emit_output
                hFlush stdout
                let failed_cls = "failed.cls"
                writeFile failed_cls script
                writeFile "failed.out" out
                writeFile "failed.err" err
                writeFile "failed.exited" (show ec)
                when (oFailFast os) $
                  die $ "test failed (run " ++ fmtArgList args ++ " " ++ failed_cls ++ ")"

--
removeIfExists :: FilePath -> IO ()
removeIfExists fp = do
  z <- doesFileExist fp
  when z $ removeFile fp

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
          if ix  < pr_ix then proc (ix + 1) lns -- skip it
            else do
              -- strip out ( ) so that we can flatten vector types
              let tks = drop 1 (words (filter (`notElem`"(,)") ln1))
              -- print tks
              match ns tks
        proc ix [] = mFail ("failed to find PRINT<..> instance " ++ show pr_ix ++ " in output")

        match (n:ns) (w:ws) =
          case reads w of
            [(x,"")] ->
              if x /= n then mFail (w ++ ": mismatch on element (expected " ++ show n ++ ")")
                else match ns ws
            _ -> mFail (w ++ ": parse error in PRINT elements")
        match [] [] = mSuccess
        match _  _ = mFail "wrong number of elements in output"

