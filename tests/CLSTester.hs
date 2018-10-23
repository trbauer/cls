
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
import qualified Data.ByteString as BS

data Opts =
  Opts {
    oVerbosity :: !Int
  , oFailFast :: !Bool
  } deriving Show
oVerbose :: Opts -> Bool
oVerbose = (>0) . oVerbosity
oDebug :: Opts -> Bool
oDebug = (>1) . oVerbosity
dft_opts :: Opts
dft_opts = Opts 0 True

cls64_exe :: FilePath
cls64_exe = "builds/vs2017-64/Debug/cls64.exe"

parseOpts :: [String] -> Opts -> IO Opts
parseOpts []     os = return os
parseOpts (a:as) os =
    case a of
      "-q" -> continue $ os{oVerbosity = -1}
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
  -- ATOM ARG SETTERS
  runInitAtomTests os
  --
  --
  -- MUTABILITY
  runSequentialAddTest os "int" ["1","3","-2"] "2"
  --
  -- PRINT
  runPrintCommandTests os
  runPrintAttributeTests os
  --
  -- SAVE
  runSaveTest os
  --
  -- DIFF(U)
  runDiffUniformTestMatch os
  runDiffUniformTestMismatch os
  -- DIFF(S)
  runDiffSurfaceTestMatchVar os
  runDiffSurfaceTestMatchImm os
  runDiffSurfaceTestMismatchVar os
  runDiffSurfaceTestMismatchImm os
  --
  -- MEM INITIALIZERS
  --   EXPLICIT DIMENSION
  runInitConstWithDim os

  --   BINARY FILE MEM INITIALIZERS
  runInitFileBinTest os
  -- runInitFileTextTest os
  -- runInitFileTextColTests os
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


-- tests evaluators (kernel arg evaluates)
runInitAtomTests :: Opts -> IO ()
runInitAtomTests os = do
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
    ("imm. arg: " ++ arg_expr)
    ty
    arg_expr
    (showNum expect_value)

runArgTest :: Opts -> String -> String -> String -> String -> IO ()
runArgTest os arg_desc arg_type arg_expr show_expect_value = do
  let script :: String
      script =
        "let R=0:w\n" ++
        "let V=0:w\n" ++
        "#0`tests/args.cl[-DT=" ++ arg_type ++ " -DEXPECT=" ++ show_expect_value ++ "]`test<1>(R," ++ arg_expr ++ ",V)\n" ++
        "diff<" ++ arg_type ++ ">(" ++ show_expect_value ++ ",V)\n"
        -- "diff<" ++ arg_type ++ ">(0,R)\n"
  runScript os arg_desc (mShouldExit 0) script


-------------------------------------------------------------------------------
--
runDiffUniformTestMatch :: Opts -> IO ()
runDiffUniformTestMatch os = do
  let script :: String
      script =
        "let B=44:rw\n" ++
        "#0`tests/add.cl[-DT=int]`add<8>(B,16)\n" ++
        "diff(44+16,B)\n" ++
        ""
  runScript os "diff uniform match" (mShouldExit 0) script

runDiffUniformTestMismatch :: Opts -> IO ()
runDiffUniformTestMismatch os = do
  let script :: String
      script =
        "let B=44:rw\n" ++
        "#0`tests/add_buggy.cl[-DT=int]`add<8>(B,16)\n" ++
        "diff(44+16,B)\n" ++
        ""
  runScript os "diff uniform mismatch" (mShouldExit 1 .&&. mContainsString "element 7") script

runDiffSurfaceTestMatchImm :: Opts -> IO ()
runDiffSurfaceTestMatchImm os = do
  let script :: String
      script =
        "let A=seq(120):rw\n" ++ -- 120-127
        "let B=seq(122):rw\n" ++ -- 122-129
        "#0`tests/add.cl[-DT=int]`add<8>(A,1)\n" ++
        "#0`tests/add.cl[-DT=int]`add<8>(B,-1)\n" ++
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
        "#0`tests/add.cl[-DT=int]`add<8>(A,1)\n" ++
        "#0`tests/add.cl[-DT=int]`add<8>(B,-1)\n" ++
        "diff(A,A)\n" ++ -- identity compare
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
        "#0`tests/add.cl[-DT=int]`add<8>(A,1)\n" ++
        "#0`tests/add_buggy.cl[-DT=int]`add<8>(B,1)\n" ++
        "diff(A,B)\n" ++
        ""
  runScript os "diff surface mismatching var" (mShouldExit 1 .&&. mContainsString "element 7") script
runDiffSurfaceTestMismatchImm :: Opts -> IO ()
runDiffSurfaceTestMismatchImm os = do
  let script :: String
      script =
        "let A=seq(0):rw\n" ++
        "#0`tests/add_buggy.cl[-DT=int]`add<8>(A,1)\n" ++
        "diff(seq(1):r,A)\n" ++
        ""
  runScript os "diff surface mismatching imm" (mShouldExit 1 .&&. mContainsString "element 7") script

-------------------------------------------------------------------------------
--
runPrintCommandTests :: Opts -> IO ()
runPrintCommandTests os = do
  let script :: String
      script =
        "let B=seq():rw\n" ++
        "#0`tests/add.cl[-DT=uint]`add<8>(B,16)\n" ++
        "print(B)\n" ++
        "print<int>(B)\n" ++
        "print<int,4>(B)\n" ++
        ""
  let has_lines = mHasAllLines $
        "00000:  0x00000010  0x00000011  0x00000012  0x00000013  0x00000014  0x00000015  0x00000016  0x00000017\n" ++
        "00000:            16            17            18            19            20            21            22            23\n" ++
        "00000:            16            17            18            19\n" ++
        "00010:            20            21            22            23\n" ++
        ""
  runScript os "print command test (int)" (mShouldExit 0 .&&. has_lines) script

  let script2 :: String
      script2 =
        "let B={1,2}:rw\n" ++
        "#0`tests/add.cl[-DT=uint2]`add<8>(B,{1,2})\n" ++
        "print(B)\n" ++
        "print<int2,4>(B)\n" ++
        ""
  let has_lines2 = mHasAllLines $
        "00000:  {0x00000002,0x00000004}  {0x00000002,0x00000004}  {0x00000002,0x00000004}  {0x00000002,0x00000004}  {0x00000002,0x00000004}  {0x00000002,0x00000004}  {0x00000002,0x00000004}  {0x00000002,0x00000004}\n" ++
        "00000:  {           2,           4}  {           2,           4}  {           2,           4}  {           2,           4}\n" ++
        "00020:  {           2,           4}  {           2,           4}  {           2,           4}  {           2,           4}\n" ++
        ""
  runScript os "print command test (int2)" (mShouldExit 0 .&&. has_lines2) script2

runPrintAttributeTests :: Opts -> IO ()
runPrintAttributeTests os = do
  let script :: String
      script =
        "#0`tests/add.cl[-DT=uint]`add<8>(seq(2,2):rwPp4,16)\n"
  let has_lines = mHasAllLines $
        "00000:  0x00000002  0x00000004  0x00000006  0x00000008  0x0000000A  0x0000000C  0x0000000E  0x00000010\n" ++
        "00000:  0x00000012  0x00000014  0x00000016  0x00000018\n" ++
        "00010:  0x0000001A  0x0000001C  0x0000001E  0x00000020\n" ++
        ""
  runScript os "print attribute test" (mShouldExit 0 .&&. has_lines) script


-------------------------------------------------------------------------------
--
runSequentialAddTest :: Opts -> String -> [String] -> String -> IO ()
runSequentialAddTest os arg_type args result = do
  let script :: String
      script =
        "let B=0:rw\n" ++
        concatMap (\a -> "#0`tests/add.cl[-DT=" ++ arg_type ++ "]`add<1>(B," ++ a ++ ")\n") args ++
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
        "#0`tests/add.cl[-DT=uchar]`add<8>(B,16)\n" ++
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

-------------------------------------------------------------------------------
-- MEM INIT
runInitConstWithDim :: Opts -> IO ()
runInitConstWithDim os = do
  let script :: String
      script =
        "let A=0:[4*8*2]w\n" ++
        -- first call writes to the first half only
        "#0`tests/add.cl[-DT=int]`add<8>(A,3)\n" ++
        -- second call writes to the second half only
        "#0`tests/add.cl[-DT=int2]`add<8>(A,{1,2})\n" ++
        "print<int,8>(A)\n" ++
        "\n"
  let buffer_matches = mHasAllLines $
        "00000:             4             5             4             5             4             5             4             5\n" ++
        "00020:             1             2             1             2             1             2             1             2\n" ++
        ""
  runScript os "init by binary file" (mShouldExit 0 .&&. buffer_matches) script

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
        "#0`tests/add.cl[-DT=uchar]`add<8>(A,1)\n" ++
        "diff(seq(13):r,A)\n" ++
        "#0`tests/add.cl[-DT=uchar]`add<8>(B,1)\n" ++
        "diff(seq(13):r,B)\n" ++
        "#0`tests/add.cl[-DT=uchar]`add<8>(C,1)\n" ++
        "diff(seq(13):r,C)\n" ++
        "\n" ++
        -- with the initializer inline
        "let D=2:rw\n" ++
        "#0`tests/add.cl[-DT=uchar]`addv<8>(D,file<bin>(" ++ show bin_file ++ "):r)\n" ++
        "diff(seq(14):r,D)\n" ++
        ""
  runScript os "init by binary file" (mShouldExit 0) script
  removeFile bin_file


-------------------------------------------------------------------------------
--
runScript :: Opts -> String -> Matcher -> String -> IO ()
runScript os tag match script = do
  putStr $ printf "%-64s" (tag ++ ":  ")
  (ec,out,err) <- readProcessWithExitCode cls64_exe ["-e",script] ""
  r <- match ec out err
  case r of
    Nothing -> do
      putStrLn $ "  PASSED"
      when (oDebug os) $
        putStrLn $ out ++ err
    Just msg -> do
      putStrLn "  FAILED"
      putStrLn $ msg
      when (oVerbose os) $
        putStrLn $
          "\n\n" ++
          "******************************************\n" ++
          "***OUT***:\n" ++
          out ++ "\n" ++
          "***ERR***:\n" ++
          err ++ "\n" ++
          "******************************************"
      hFlush stdout
      let failed_cls = "failed.cls"
      writeFile failed_cls script
      when (oFailFast os) $
        die $ "test failed (run " ++ failed_cls ++ ")"



-------------------------------------------------------------------------------
-- MATCHER EDSL
type Matcher = ExitCode -> String -> String -> IO (Maybe String)

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
  | otherwise = mFail $ "failed to find line with words: " ++ ln

mContainsString :: String -> Matcher
mContainsString ss _ out _ =
  if ss `isInfixOf` out then mSuccess
    else mFail ("expected " ++ show ss ++ " in output")

mShouldExit :: Int ->  Matcher
mShouldExit expected_ec = \ec out err ->
    case ec of
      ExitFailure ec -> exited ec
      ExitSuccess -> exited 0
  where exited ec
          | ec == expected_ec = mSuccess
          | otherwise = mFail $ "test didn't exit " ++ show expected_ec ++ " (exited " ++ printf "0x%X" (fromIntegral ec :: Word32) ++ ")"
