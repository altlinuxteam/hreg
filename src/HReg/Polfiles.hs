-----------------------------------------------------------------------------
--
-- Module      :  Polfiles
-- Copyright   :
-- License     :  NONE
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module HReg.Polfiles (
    parse,
    Action(..),
    Key(..),
    Value(..)
--    generate
) where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.List.NonEmpty ((<|), nonEmpty, map)
import Data.Maybe (fromJust)
import qualified Data.Text as T --(splitOn, stripPrefix, unpack, isPrefixOf)
import Data.Text.Encoding (decodeUtf16LE)
import HReg.Types
import Prelude hiding (map)

lbr, rbr :: Word16
lbr = 0x5b
rbr = 0x5d

semicol,zero :: Word8
semicol = 0x3b
zero = 0x00

data Header = Header{ sig :: !Word32
                    , ver :: !Word32
                    }
    deriving (Eq, Show)

data Rec = Rec{ key :: !Text
              , val :: !Text
              , t   :: !Int
              , sz  :: !Int
              , dt  :: !ByteString
              }
    deriving (Eq, Show)

parse :: BL.ByteString -> NonEmpty Action
parse = runGet getActions

getActions :: Get (NonEmpty Action)
getActions = label "parser" $ do
    _ <- getHeader
    map rec2action <$> getRecs

getHeader :: Get Header
getHeader = label "header" $ do
    h <- getWord32le
    when (h /= 0x67655250) $ fail "signature did not match"
    ver <- getWord32le
    when (ver /= 1) $ fail $ "version " <> show ver <> " did not supported"
    pure $ Header h ver

getRec :: Get Rec
getRec = label "record" $ do
    x <- getWord16le
    when (x /= lbr) $ fail $ "expect '" <> show lbr <> "' but got " <> show x
    k <- getWhileT (/= semicol)
    v <- getWhileT (/= semicol)
    t <- getInt32le
    skip 2
    sz <- getInt32le
    skip 2
    dt <- getByteString (fromIntegral sz)
    x <- getWord16le
    when (x /= rbr) $ fail $ "expect: '" <> show rbr <> "' but got " <> show x
    pure $ Rec k v (fromIntegral t) (fromIntegral sz) dt

getRecs :: Get (NonEmpty Rec)
getRecs = label "getRecs" $ do
    recs <- getRecs'
    case nonEmpty recs of
        Just x -> pure x
        Nothing -> fail "failed to construct non empty records list"

getRecs' :: Get [Rec]
getRecs' = label "getRecs'" $ do
    empty <- isEmpty
    if empty
    then pure []
    else do
        r <- getRec
        rs <- getRecs'
        pure (r:rs)

getWhileT :: (Word8 -> Bool) -> Get Text
getWhileT p = label "whileT" $ do
    bs <- getWhile p
    let bs' = if even (BL.length bs)
              then bs
              else BL.init bs
    pure $ decodeUtf16LE . BL.toStrict . BL.reverse . BL.drop 2 $ bs'

getWhile :: (Word8 -> Bool) -> Get BL.ByteString
getWhile p = label "getWhile" $ res ""
    where res a = do
                    x <- getWord8
                    if p x then res (BL.cons x a) else skip 1 >> pure a

--generate :: Registry -> [Action] -> ByteString
--generate = undefined

rec2action :: Rec -> Action
rec2action (Rec k v t sz dt) = case v of
    "**DeleteValues" -> DelValues k (semicolList v)
    "**DelVals" -> DelSubKeys k
    "**DeleteKeys" -> DeleteKeys k (semicolList v)
    x -> if T.isPrefixOf "**Del." x then DelValue k (fromJust (T.stripPrefix "**Del." x))
         else case T.stripPrefix "**Secure=" x of
            Just s -> SecureKey k (s == "1")
            Nothing -> SetValue (k <> "\\" <> v) (toVal t dt)

toVal :: Int -> ByteString -> Value
toVal  0 v = REG_NONE v
toVal  1 v = REG_SZ $ stripZero v
toVal  2 v = REG_EXPAND_SZ $ stripZero v
toVal  3 v = REG_BINARY v
toVal  4 v = REG_DWORD $ runGet getWord32le (BL.fromStrict v)
toVal  5 v = REG_DWORD_BIG_ENDIAN $ runGet getWord32be (BL.fromStrict v)
toVal  6 v = REG_LINK $ decodeUtf16LE v
toVal  7 v = REG_MULTI_SZ $ fromJust $ nonEmpty $ BS.split zero v
toVal  8 v = REG_RESOURCE_LIST v
toVal  9 v = REG_FULL_RESOURCE_DESCRIPTION v
toVal 10 v = REG_RESOURCE_REQUIREMENTS_LIST v
toVal 11 v = REG_QWORD $ runGet getWord64le (BL.fromStrict v)

stripZero :: ByteString -> ByteString
stripZero = BS.reverse . BS.drop 2 . BS.reverse

semicolList :: Text -> NonEmpty Text
semicolList l = case T.splitOn ";" l of
    [] -> "" :| []
    x -> fromJust $ nonEmpty x
