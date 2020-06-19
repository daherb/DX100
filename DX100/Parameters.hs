module DX100.Parameters where

import Test.QuickCheck 
import Data.Char
import Data.Bits 
import Data.Binary
import Data.Bits.Extras
import qualified Data.ByteString.Lazy as BS

printableChars = [c | c <- map chr [32..127], (isAlpha c || isDigit c || isPunctuation c)]

data Operator = OP
  { ar :: Int
  , d1r :: Int
  , d2r :: Int
  , rr :: Int
  , d1l :: Int
  , ls :: Int
  , rs :: Int
  , ebs :: Int
  , ame :: Int
  , kvs :: Int
  , out :: Int
  , f :: Int
  , det :: Int
  } deriving (Eq,Show)

mkOperator :: [Int] -> Operator
mkOperator [ar, d1r, d2r, rr, d1l, ls, rs, ebs, ame, kvs, out, f, det]
  = OP ar d1r d2r rr d1l ls rs ebs ame kvs out f det

instance Arbitrary Operator where
  arbitrary =
    do
      ar <- elements [0..31]
      d1r <- elements [0..31]
      d2r <- elements [0..31]
      rr <- elements [0..15]
      d1l <- elements [0..15]
      ls <- elements [0..99]
      rs <- elements [0..3]
      ebs <- elements [0..7]
      ame <- elements [0..1]
      kvs <- elements [0..7]
      out <- elements [50..99]
      f <- elements [0..63]
      det <- elements [0..6]
      return $ OP ar d1r d2r rr d1l ls rs ebs ame kvs out f det

data Voice = Voice
  { op4 :: Operator
  , op2 :: Operator
  , op3 :: Operator
  , op1 :: Operator
  , alg :: Int
  , fbl :: Int
  , lfs :: Int
  , lfd :: Int
  , pmd :: Int
  , amd :: Int
  , sync :: Int
  , lw :: Int
  , pams :: Int
  , ams :: Int
  , midC :: Int
  , poly :: Int
  , pBendRange :: Int
  , portaMode :: Int
  , portaTime :: Int
  , fVolRange :: Int
  , sustainSw :: Int
  , portaSustainSw :: Int
  , chorusSw :: Int
  , mwPitch :: Int
  , mwAmpli :: Int
  , bcPitch :: Int
  , bcAmpli :: Int
  , bcPBias :: Int
  , bcEBias :: Int
  , voiceName :: String
  , pitchEGRate1 :: Int
  , pitchEGRate2 :: Int
  , pitchEGRate3 :: Int
  , pitchEGLevel1 :: Int
  , pitchEGLevel2 :: Int
  , pitchEGLevel3 :: Int
  } deriving (Eq,Show)

instance Arbitrary Voice where
  arbitrary =
    do
      op4 <- arbitrary
      op2  <- arbitrary
      op3  <- arbitrary
      op1  <- arbitrary
      alg <- elements [0..7]
      fbl <- elements [0..7]
      lfs <- elements [0..99]
      lfd <- elements [0..99]
      pmd <- elements [0..99]
      amd <- elements [0..99]
      sync <- elements [0..1]
      lw <- elements [0..3]
      pams <- elements [0..7]
      ams <- elements [0..7]
      midC <- return 32 -- elements [0..48]
      poly <- elements [0..1]
      pBendRange <- elements [0..12]
      portaMode <- elements [0..1]
      portaTime <- elements [0..99]
      fVolRange <- return 99
      sustainSw <- elements [0..1]
      portaSustainSw <- return $ 1 - sustainSw -- elements [0..1]
      chorusSw <- return 0
      mwPitch <- elements [0..99]
      mwAmpli <- elements [0..99]
      bcPitch <- elements [0..99]
      bcAmpli <- elements [0..99]
      bcPBias <- elements [0..99]
      bcEBias <- elements [0..99]
      voiceName <-  pad <$> take 10 <$> listOf1 (elements printableChars)
      pitchEGRate1 <- return 63
      pitchEGRate2 <- return 63
      pitchEGRate3 <- return 63
      pitchEGLevel1 <- return 32
      pitchEGLevel2 <- return 32
      pitchEGLevel3 <- return 32
      return $ Voice op4 op2 op3 op1 alg fbl lfs lfd pmd amd sync lw pams ams midC
        poly pBendRange portaMode portaTime fVolRange sustainSw portaSustainSw
        chorusSw mwPitch mwAmpli bcPitch bcAmpli bcPBias bcEBias voiceName
        pitchEGRate1 pitchEGRate2 pitchEGRate3 pitchEGLevel1 pitchEGLevel2 pitchEGLevel3
        where
          pad s =
            s ++ replicate (10 -length s) ' '

data Sysex = Sysex
  {
    channel :: Int,
    voice :: Voice
  } deriving (Eq,Show)

-- Reads a sysex message from a string
readSysex :: String -> Sysex
readSysex bytes =
  let
    (c,v) =readStatus (words bytes)
  in
    Sysex c v
  where
    readStatus ("F0":bs) =
      readID bs
    readStatus (s:_) =
      error $ "Wrong status: " ++ s
    readStatus _ =
      error "No status"
    readID ("43":bs) =
      readChannel bs
    readID (i:_) =
      error $ "Wrong id: " ++ i
    readID _ =
      error "No id"
    readChannel (c:bs) =
      ((hexToInt c),readFormat bs)
    readFormat ("03":bs) =
      readByteCount bs
    readFormat (f:_) =
      error $ "Wrong format: " ++ f
    readFormat _ =
      error "No format"
    readByteCount ("00":"5D":bs) =
      readData bs
    readByteCount (b:b':_) =
      error $ "Wrong byte count: " ++ b ++ b'
    readByteCount _ =
      error "No byte count"
    readData bs =
      readVoice bs
    readVoice bs =
      let
        (op4bs,bs') = splitAt 13 bs
        (op2bs,bs'') = splitAt 13 bs'
        (op3bs,bs''') = splitAt 13 bs''
        (op1bs,bs'''') = splitAt 13 bs'''
        (databs,bs''''') = splitAt 41 bs''''
        (data1bs,data2bs) = splitAt 25 databs
        (voiceName,data2bs') = splitAt 10 data2bs
        [alg, fbl, lfs, lfd, pmd, amd, sync, lw, pams, ams, midC, poly, pBendRange, portaMode, portaTime, fVolRange, sustainSw, portaSustainSw, chorusSw, mwPitch, mwAmpli, bcPitch, bcAmpli, bcPBias, bcEBias] = map hexToInt data1bs
        [pitchEGRate1, pitchEGRate2, pitchEGRate3, pitchEGLevel1, pitchEGLevel2, pitchEGLevel3] = map hexToInt data2bs'
        voiceName' = map (chr. hexToInt) voiceName
        v = Voice
          (mkOperator $ map hexToInt op4bs)
          (mkOperator $ map hexToInt op2bs)
          (mkOperator $ map hexToInt op3bs)
          (mkOperator $ map hexToInt op1bs)
          alg fbl lfs lfd pmd amd sync lw pams ams midC poly pBendRange portaMode portaTime fVolRange sustainSw portaSustainSw chorusSw mwPitch mwAmpli bcPitch bcAmpli bcPBias bcEBias voiceName' pitchEGRate1 pitchEGRate2 pitchEGRate3 pitchEGLevel1 pitchEGLevel2 pitchEGLevel3
      in
        readChecksum v bs'''''
    readChecksum v (_:bs) =
      readEOX v bs
    readEOX v ["F7"] = v
    readEOX _ bs =
      error $ "Problem with EOX: " ++ show bs

-- Converts a Sysex message into a string of hex bytes
writeSysex :: Sysex -> String
writeSysex s =
  let
    header = [ "F0" -- Status
          , "43" -- ID
          , intToHex (channel s) -- Channel (usually 0)
          , "03" -- 1 voice bulk data
          , "00" -- byte count 1 (taken from dumped voice)
          , "5D" -- byte count 2 (taken from dumped voice)
          ]
    voiceData = hexVoice (voice s)
    checksum = intToHex (midiChecksum (map hexToInt voiceData))
  in
    unwords $ header ++ voiceData ++ [checksum] ++
    [ "F7\n" ] -- EOX

-- Converts a hex string into an integer
hexToInt :: String -> Int
hexToInt = foldl ((+) . (16 *)) 0 . map digitToInt

-- Converts an integer into a hex string. only defined for numbers < 256
intToHex :: Int -> String
intToHex i
  | i == 0 = "00"
  | i > 0 = pad $ intToStr i
  | otherwise =
      error ("intToHexString: negative argument " ++ show i)
  where
    intToStr 0  = ""
    intToStr i' = intToStr (i' `div` 16) ++ [fourBitsToChar (i' `mod` 16)]
    pad s
      | length s == 2 = s
      | length s < 2 = "0" ++ s
      | otherwise = error "Cannot handle this here"
      
-- Converts a nible to a hex digit  
fourBitsToChar          :: Int -> Char
fourBitsToChar i        = "0123456789ABCDEF" !! i

-- Converts an operator into a list of hex numbers
hexOp :: Operator -> [String]
hexOp (OP  ar d1r d2r rr d1l ls rs ebs ame kvs out f det) =
  map intToHex [ar, d1r, d2r, rr, d1l, ls, rs, ebs, ame, kvs, out, f, det]

-- Converts a voice into a list of hex numbers
hexVoice :: Voice -> [String]
hexVoice (Voice op4 op2 op3 op1 alg fbl lfs lfd pmd amd sync lw pams ams midC poly pBendRange portaMode portaTime fVolRange sustainSw portaSustainSw chorusSw mwPitch mwAmpli bcPitch bcAmpli bcPBias bcEBias voiceName pitchEGRate1 pitchEGRate2 pitchEGRate3 pitchEGLevel1 pitchEGLevel2 pitchEGLevel3) =
  hexOp op4 ++ hexOp op2 ++ hexOp op3 ++ hexOp op1 ++
  map intToHex [alg, fbl, lfs, lfd, pmd, amd, sync, lw, pams, ams, midC, poly, pBendRange, portaMode, portaTime, fVolRange, sustainSw, portaSustainSw, chorusSw, mwPitch, mwAmpli, bcPitch, bcAmpli, bcPBias, bcEBias] ++
  hexString voiceName ++
  map intToHex [pitchEGRate1, pitchEGRate2, pitchEGRate3, pitchEGLevel1, pitchEGLevel2, pitchEGLevel3]
  where
    hexString s =
      map (intToHex . ord) s

-- Computes the MIDI checksum
midiChecksum d =
  (128-sum d Data.Bits..&. 127)  `mod` 128

-- Writes a hex string to a  binary SYX file
writeSyx :: FilePath -> String -> IO ()
writeSyx fp hs =
  let
    hss = map (w8 . hexToInt) $ words hs
    bs = BS.concat $ map encode hss
  in
    do
      BS.writeFile fp bs
      
