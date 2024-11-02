import Control.Monad
import Data.Bits
import Data.List
import Data.Word
import Debug.Trace
import System.FilePath
import System.IO
import Text.Printf
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as DS

{-
FACTS:
largest difference on a single SM?

REDO the entire thing after sync'ing smclocks with globaltimer?

OPENS:
  is the entire thing believable (29 us to run the whole workload)
  0.00022 s
  0.00010 min after warmup
  0.00015 s single runs (don't corrupt the log)
  150us which is much larger than 23us that I see in the profile
   -t=CL
   #0`blur-analyze.cl`blur<512x512>(__LOG, sizeof(__LOG), IMG1, SRC)      1   0.00004   0.00004        0.0   0.00004      0.00004            0.0
    40us (could be 35..us) vs deduced 25 us
   very short kernel
-}
-- "blur9x-32x1.bin"

formatFile :: FilePath -> IO ()
formatFile fp = do
  lbs <- LBS.readFile fp
  case parseLog lbs of
    Left err -> putStrLn err
    Right evts -> do
      -- print (length sm0_evts)
      let unique :: Ord a => (Event -> a) -> [a]
          unique f = DS.toList (DS.fromList (map f evts))

      let sm_ns = maximum (map eGlobalTimer evts) - minimum (map eGlobalTimer evts)
      let max_delta_c = maximum $ map smDeltaC (unique eSmId)
            where smDeltaC smid = maximum (map eSmClock sm_evts) - minimum (map eSmClock sm_evts)
                      where sm_evts = filter ((== smid) . eSmId) evts
      putStrLn $ "workload nano delta: " ++ show sm_ns
      putStrLn $ "workload max c delta: " ++ show max_delta_c

      -- let sm00_evts = filter (\e -> (0,0) == (eSmId e,eWarpId e)) evts
      -- mapM_ print sm00_evts
      forM_ (unique eSmId) $ \smid -> do
        let sm_evts = filter ((== smid) . eSmId) evts
        -- mapM_ print sm_evts
        let sm_clk = maximum (map eSmClock sm_evts) - minimum (map eSmClock sm_evts)
        putStrLn $ printf "sm%02d" smid ++ " clocks: "  ++ show sm_clk

--      print $ unique eSmId
--      print $ unique eWarpId
--      print $ unique $ \e -> (eSmId e,eWarpId e)
--      print $ length $ unique $ \e -> (eSmId e,eWarpId e)
      let grp_evts = groupByUnit evts
      let gt_min_ns = minimum (unique eGlobalTimer)
{-
      putStrLn "FIRST EVENTS"
      forM_ grp_evts $ \((sm,w),es) -> do
        let (e0:e1:_) = es
        let sm_evts = filter ((== sm) . eSmId) evts
            sm_min_clks = minimum $ map eSmClock sm_evts
            sm_min_ns = minimum $ map eGlobalTimer sm_evts
        let fmtEvt e =
              show (eOp e) ++ " " ++
              printf "sm%02d.w%02d.  %10d ns (T_dev+%d ns) (T_sm+%d ns); %10d c (T+%d c)"
                sm
                w
                (eGlobalTimer e)
                (eGlobalTimer e - gt_min_ns)
                (eGlobalTimer e - sm_min_ns)
                (eSmClock e)
                (eSmClock e - sm_min_clks)
        putStrLn $ fmtEvt e0
        putStrLn $ "  " ++ fmtEvt e1
-}
      let ts = unique eGlobalTimer
      let t0 = head ts
      -- mapM_ (\t -> print (t - t0)) ts
      let out_fp = dropExtension fp ++ "-svg.html"
      emitSVG out_fp evts
      return ()

groupByUnit :: [Event] -> [((SMID,WarpID),[Event])]
groupByUnit evts =
    map (\k -> (k,filter ((== k) . mkKey) evts)) keys
  where domain :: Ord a => (Event -> a) -> [a]
        domain f = DS.toList (DS.fromList (map f evts))

        keys :: [(SMID,WarpID)]
        keys = domain mkKey

        mkKey e = (eSmId e,eWarpId e)


emitSVG :: FilePath -> [Event] -> IO ()
emitSVG fp = writeFile fp . formatSVG

formatSVG :: [Event] -> String
formatSVG evts = trace (show (max_ns_time - min_ns_time) ++ " ns elapsed") $
    "<svg" ++
--      width="120"
--      height="220"
--      viewPort="0 0 120 120"
    " width=\"" ++ show (ceiling plot_width) ++ "\"" ++
    " height=\"" ++ show (y_end + 16) ++ "\"" ++
    " version=\"1.1\"" ++
    " xmlns=\"http://www.w3.org/2000/svg\"" ++
    ">\n" ++
    style ++
    sm_svg ++
    faded_vert_lines ++
    "</svg>\n"

  where (y_end,sm_svg) = formatSMs y_top [] smids
        ts = domain eGlobalTimer
        max_ns_time = maximum ts
        min_ns_time = minimum ts
        delta_ns = max_ns_time - min_ns_time

        faded_vert_lines :: String
        faded_vert_lines =
            concatMap mkFadedVertLine (xs ++ [maximum xs + 1000])
          where xs = takeWhile (< delta_ns) [0,1000..]

        mkFadedVertLine :: Word64 -> String
        mkFadedVertLine x_ns =
          "  " ++
          "<text" ++
          " class=\"usec\"" ++
          " x=\"" ++ x_str ++ "\"" ++
          " y=\"" ++ show usec_px ++ "\"" ++
          " stroke=\"" ++ "black" ++ "\">" ++ show (x_ns`div`1000) ++ " &micro;s</text>\n" ++
          "  " ++
          "<line" ++
          " class=\"axis\"" ++
          " x1=\"" ++ x_str ++ "\"" ++
          " y1=\"" ++ show y_top ++ "\"" ++
          " x2=\"" ++ x_str ++ "\"" ++
          " y2=\"" ++ show y_end ++ "\" />" ++
          "\n"
          where x_str = printf "%.3f" val :: String
                  where val = fromIntegral evt0_indent + nsOffsetX (min_ns_time + x_ns) :: Double

        sm_px :: Int
        sm_px = 6

        usec_px :: Int
        usec_px = 8

        style :: String
        style =
          "<style>\n" ++
          "  <![CDATA[\n" ++
          "    text.smid {\n" ++
          "      font: normal " ++ show sm_px ++ "px monospace;\n" ++
          "    }\n" ++
          "    text.usec {\n" ++
          "      font: normal " ++ show usec_px ++ "px monospace;\n" ++
          "    }\n" ++
          "    text.wrpid {\n" ++
          "      font: normal " ++ show (evt_height_ns + evt_height_c) ++ "px monospace;\n" ++
          "    }\n" ++
          "    rect.thr {\n" ++
--          "      stroke: green;\n" ++
          "      fill:   green;\n" ++
          "    }\n" ++
          "    rect.thr:hover {\n" ++
--          "      stroke: red;\n" ++
          "      fill:   red;\n" ++
          "    }\n" ++
          "    line.thrstt {\n" ++
          "      stroke:  #30FF30;\n" ++
          "    }\n" ++
          "    rect.cthr {\n" ++
--          "      stroke: #4040FF;\n" ++
          "      fill:   #C0C0FF;\n" ++
          "    }\n" ++
          "    rect.cthr:hover {\n" ++
--          "      stroke: red;\n" ++
          "      fill:   red;\n" ++
          "    }\n" ++
          "    line.axis {\n" ++
          "      stroke:           #FF3030;\n" ++
          "      stroke-dasharray: 2;\n" ++
          "    }\n" ++
          "  ]]>\n" ++
          "</style>\n" ++
          "\n"

        warpids = domain eWarpId
        smids = domain eSmId

        nsToWidth :: Word64 -> Double
        nsToWidth ns = plot_width * fromIntegral ns / fromIntegral delta_ns

        nsOffsetX :: Word64 -> Double
        nsOffsetX x_ns = nsToWidth (x_ns - min_ns_time)

        y_top :: Int
        y_top = 16

        sm_indent :: Int
        sm_indent = 4

        wrp_indent :: Int
        wrp_indent = sm_indent + 16

        evt0_indent :: Int
        evt0_indent = wrp_indent + 16

        evt_height_ns :: Int
        evt_height_ns = 4
        evt_height_c :: Int
        evt_height_c = 4
        evt_heights :: Int
        evt_heights = evt_height_ns + evt_height_c

        plot_width :: Double
        plot_width = 2400.0

        warp_y_spacing :: Int -- gap spacing
        warp_y_spacing = 1

        domain :: Ord a => (Event -> a) -> [a]
        domain f = DS.toList (DS.fromList (map f evts))

        formatSMs :: Int -> [String] -> [Int] -> (Int,String)
        formatSMs y_sm_off rsm_svgs [] = (y_sm_off,concat (reverse rsm_svgs))
        formatSMs y_sm_off rsm_svgs (smid:smids) =
            formatWarpId y_sm_off (sm_lbl:rsm_svgs) warpids
          where sm_lbl =
                  "<text" ++
                  " class=\"smid\"" ++
                  " x=\"" ++ show sm_indent ++ "\"" ++
                  " y=\"" ++ show y_sm_off ++ "\">" ++ printf "sm%02d" smid ++ "</text>\n"

                sm_evts :: [Event]
                sm_evts = filter ((== smid) . eSmId) evts

                min_sm_clock :: Word64
                min_sm_clock = minimum (map eSmClock sm_evts)
                max_sm_clock :: Word64
                max_sm_clock = maximum (map eSmClock sm_evts)

                cToWidth :: Word64 -> Double
                cToWidth c = plot_width * fromIntegral c / fromIntegral (max_sm_clock - min_sm_clock)

                cOffsetX :: Word64 -> Double
                cOffsetX x_c = cToWidth (x_c - min_sm_clock)

                formatWarpId :: Int -> [String] -> [Int] -> (Int,String)
                formatWarpId y_wid_off rsm_svgs [] =
                  -- formatSMs (y_wid_off + evt_heights) rsm_svgs []
                  formatSMs (y_wid_off + evt_heights) rsm_svgs smids
                formatWarpId y_wid_off rsm_svgs (warpid:warpids) =
                    formatWarpId
                      (y_wid_off + evt_heights + warp_y_spacing)
                      (evts_lbl:wid_lbl:rsm_svgs)
                      warpids
                  where wid_lbl :: String
                        wid_lbl =
                          "  " ++
                          "<text" ++
                            " class=\"wrpid\"" ++
                            " x=\"" ++ show wrp_indent ++ "\"" ++
                            " y=\"" ++ show (y_wid_off + evt_height_ns + evt_height_c) ++ "\">" ++
                            printf "w%02d" warpid ++ "</text>\n"

                        evts_lbl :: String
                        evts_lbl =
                          case sm_w_evts of
                            [] -> ""
                            e0:evts
                              | eOp e0 == EventOpTS -> formatEvts e0 evts

                        sm_w_evts :: [Event]
                        sm_w_evts = filter (\e -> eSmId e == smid && eWarpId e == warpid) evts

                        sm_w_clock_min, sm_w_clock_max :: Word64
                        sm_w_clock_min = minimum (map eSmClock sm_w_evts)
                        sm_w_clock_max = maximum (map eSmClock sm_w_evts)

                        cWidToWidth :: Word64 -> Double
                        cWidToWidth c = plot_width * fromIntegral c / fromIntegral (sm_w_clock_max - sm_w_clock_min)

                        cWidOffsetX :: Word64 -> Double
                        cWidOffsetX x_c = cToWidth (x_c - min_sm_clock)

                        -- keep track of the last event?
                        formatEvts :: Event -> [Event] -> String
                        formatEvts _ [] = ""
                        formatEvts e_thr_st (e_thr_end:evts)
                          | eOp e_thr_end == EventOpTE =
                            ns_thr_rect ++
                            c_thr_rect ++
                            sfx
                          where tooltip_ns :: String
                                tooltip_ns =
                                    printf "&lt;%d,%d,%d&gt;: %d ns long (offset T+%d ns)"
                                      gx gy gz
                                      ns_thr_len
                                      (eGlobalTimer e_thr_st - min_ns_time)
                                  where (gx,gy,gz) = eGid e_thr_st

                                tooltip_c :: String
                                tooltip_c =
                                    printf "&lt;%d,%d,%d&gt;: %d c long (offset C+%d c)"
                                      gx gy gz
                                      c_thr_len
                                      (eSmClock e_thr_st - sm_w_clock_min)
                                  where (gx,gy,gz) = eGid e_thr_st

                                ns_thr_rect :: String
                                ns_thr_rect =
                                  "    " ++
                                  "<rect" ++
                                  " class=\"thr\"" ++
                                  " x=\"" ++ printf "%.3f" x_ns_thr_off ++ "\"" ++
                                  " y=\"" ++ show y_wid_off ++ "\"" ++
                                  " width=\"" ++ printf "%.3f" (nsToWidth ns_thr_len) ++ "\"" ++
                                  " height=\"" ++ show evt_height_ns ++ "\">" ++
                                  "<title>" ++ tooltip_ns ++ "</title></rect>\n" ++
                                  "    " ++
                                  "<line" ++
                                  " class=\"thrstt\"" ++
                                  " x1=\"" ++ printf "%.3f" x_ns_thr_off ++ "\"" ++
                                  " y1=\"" ++ show y_wid_off ++ "\"" ++
                                  " x2=\"" ++ printf "%.3f" x_ns_thr_off ++ "\"" ++
                                  " y2=\"" ++ show (y_wid_off + evt_height_ns) ++ "\"" ++
                                  " />\n" ++
                                  ""

                                x_ns_thr_off :: Double
                                x_ns_thr_off = fromIntegral evt0_indent + nsOffsetX (eGlobalTimer e_thr_st)
                                ns_thr_len = eGlobalTimer e_thr_end - eGlobalTimer e_thr_st

                                c_thr_rect :: String
                                c_thr_rect
                                  | evt_height_c <= 0 = ""
                                  | otherwise =
                                    "    " ++
                                    "<rect" ++
                                    " class=\"cthr\"" ++
                                    " x=\"" ++ printf "%.3f" x_c_thr_off ++ "\"" ++
                                    " y=\"" ++ show (y_wid_off + evt_height_ns) ++ "\"" ++
                                    -- " width=\"" ++ printf "%.3f" (cWidToWidth c_thr_len) ++ "\"" ++
                                    " width=\"" ++ printf "%.3f" (cToWidth c_thr_len) ++ "\"" ++
                                    " height=\"" ++ show evt_height_c ++ "\">" ++
                                    "<title>" ++ tooltip_c ++ "</title></rect>\n"
                                x_c_thr_off :: Double
                                -- x_c_thr_off = fromIntegral evt0_indent + cWidOffsetX (eSmClock e_thr_st)
                                x_c_thr_off = fromIntegral evt0_indent + cOffsetX (eSmClock e_thr_st)
                                c_thr_len = eSmClock e_thr_end - eSmClock e_thr_st

                                sfx :: String
                                sfx =
                                  case evts of
                                    [] -> ""
                                    e0:evts
                                      | eOp e0 == EventOpTS -> formatEvts e0 evts

                        formatEvts e0 es =
                          error $ "\n" ++
                            "* " ++ show e0 ++ "\n" ++ concatMap (\e -> "* " ++ show e ++ "\n") es

-----------------------

data Event =
  Event {
    eOp :: !EventOp
  , eSmId :: !SMID
  , eWarpId :: !WarpID
  , eGlobalTimer :: !Word64
  , eSmClock :: !Word64
  , eGid :: !(Word32,Word32,Word32)
  } deriving (Show,Eq)
data EventOp =
    EventOpTS -- 1
  | EventOpTE -- 2
  deriving (Show,Eq)
type SMID = Int
type WarpID = Int

parseLog :: LBS.ByteString -> Either String [Event]
parseLog lbs0 =
    case breakOffRecord (LBS.unpack lbs0) of
      Left err -> Left err
      Right ((w0,_,_,_,_,_,_,_),w8s) -> decodeEvts 32 [] w8s
  where decodeEvts :: Int -> [Event] -> [Word8] -> Either String [Event]
        decodeEvts _   revts [] = return (reverse revts)
        decodeEvts off revts w8s = do
          let errInRec :: Int -> String -> Either String a
              errInRec rec_off msg = Left (printf "offset 0x%06X: " (off + rec_off) ++ msg)
          case breakOffRecord w8s of
            Left err -> Left err
            Right ((0,0,0,0,0,0,0,0),w8s_sfx) -> decodeEvts off revts [] -- early end
            Right ((w0,w1,w2,w3,w4,w5,w6,w7),w8s_sfx) -> do -- trace (printf "w0 0x%08X\n" w0) $ do
              eop <-
                case w0 .&. 0xFF of
                  1 -> return EventOpTS
                  2 -> return EventOpTE
                  v -> errInRec 0 ("invalid event tag " ++ show v)
              let warpid = fromIntegral (w0`shiftR`8) .&. 0xFF
              let smid = fromIntegral (w0`shiftR`16) .&. 0xFF
              let toW64 :: Word32 -> Word32 -> Word64
                  toW64 w32_lo w32_hi = (fromIntegral w32_hi`shiftL`32) .|. fromIntegral w32_lo
              let gtimer = toW64 w1 w2
              let smtimer = toW64 w3 w4
              let evt = w7`seq`
                    Event {
                      eOp = eop
                    , eSmId = smid
                    , eWarpId = warpid
                    , eGlobalTimer = gtimer
                    , eSmClock = smtimer
                    , eGid = (w5,w6,w7)
                    }
              decodeEvts (off + 32) (evt:revts) w8s_sfx


breakOffRecord :: [Word8] -> Either String ((Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32),[Word8])
breakOffRecord bs0 = do
  let breakW32 :: [Word8] -> Either String (Word32,[Word8])
      breakW32 w8s =
        case splitAt 4 w8s of
           ([b0,b1,b2,b3],bs_sfx) ->
               return (toB 0 b0 .|. toB 8 b1 .|. toB 16 b2 .|. toB 24 b3,bs_sfx)
             where toB :: Int -> Word8 -> Word32
                   toB sh b = fromIntegral b `shiftL` sh
           _ -> Left "number of bytes not a multiple of 32"
  (w0,bs_sfx0) <- breakW32 bs0
  (w1,bs_sfx1) <- breakW32 bs_sfx0
  (w2,bs_sfx2) <- breakW32 bs_sfx1
  (w3,bs_sfx3) <- breakW32 bs_sfx2
  (w4,bs_sfx4) <- breakW32 bs_sfx3
  (w5,bs_sfx5) <- breakW32 bs_sfx4
  (w6,bs_sfx6) <- breakW32 bs_sfx5
  (w7,bs_sfx7) <- breakW32 bs_sfx6
  return ((w0,w1,w2,w3,w4,w5,w6,w7),bs_sfx7)

