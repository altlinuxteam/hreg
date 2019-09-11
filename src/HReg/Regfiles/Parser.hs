-----------------------------------------------------------------------------
--
-- Module      :  Regfiles.Parser
-- Copyright   :
-- License     :  BSD-3-Clause
--
-- Maintainer  :  omgbebebe@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module HReg.Regfiles.Parser where

import           Data.Attoparsec.Text.Lazy
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Read as TR
import           HReg.Types
import           HReg.Regfiles.Types
import           Numeric (readHex)
import           Prelude hiding (take)
import           System.IO (openFile, hSetEncoding, utf16, IOMode(..))
import           System.IO.Unsafe

type RegItem = (Path, [(KeyName, Value)])

--parseReg :: FilePath -> IO (Either String [RegItem])
parseReg path = do
    f <- readUtf16 path
--    pure $ parse regParser f
    pure $ eitherResult (parse regParser f)

regParser :: Parser [RegItem]
regParser = do
    header <- string "Windows Registry Editor Version 5.00"
    endOfLine
    items <- many' item
--    choice [endOfLine, endOfInput]
    pure items

branchName :: Parser Path
branchName = do
    endOfLine
    name <- string "[" *> manyTill' anyChar (string "]")
    endOfLine
    pure $ Path name

keyVals :: Parser [(KeyName, Value)]
keyVals = many' keyVal

keyVal :: Parser (KeyName, Value)
keyVal = do
    keyName <- choice [quotedStringT, string "@"]
    _ <- char '='
    val <- choice [justString, binaryVal]
    pure (KeyName (T.unpack keyName), val)

justString :: Parser Value
justString = do
    val <- quotedStringT
    endOfLine
    pure $ REG_SZ (T.encodeUtf8 val)


{- Parse quoted string with escaped symbols -}
quotableChar :: Parser Char
quotableChar = satisfy (inClass "\"")

nonQuotableChar :: Parser Char
nonQuotableChar = satisfy (notInClass "\"")

quotedString :: Parser String
quotedString = do
    char '"'
    v <- many $ choice [skip ('\\'==) >> quotableChar, nonQuotableChar]
    char '"'
    pure v

quotedStringT :: Parser Text
quotedStringT = T.pack <$> quotedString
{-
quotedStringT :: Parser Text
quotedStringT = do
    char '"'
    v <- many (noneOf "\"" <|> (char '\\' >> char '\"'))
    char '"'
--    v <- string "\"" *> manyTill' anyChar (string "\"")
    pure $ T.pack v

quotedString :: Parser String
quotedString = string "\"" *> manyTill' anyChar (string "\"")
-}
binaryVal :: Parser Value
binaryVal = do
    ty <- takeTill (':'==)
    char ':'

    val <- case ty of
            "dword" -> dwordP <?> "dwordP"
            "hex" -> hexP <?> "hexP"
            "hex(2)" -> expandszP <?> "expandszP"
            "hex(4)" -> hexP <?> "hexP"
            "hex(7)" -> multiszP <?> "multiszP"
            "hex(b)" -> hexP <?> "hexP"
            "hex(0)" -> hexP <?> "hexP"
    pure val

dwordP :: Parser Value
dwordP = do
    dw <- take 8
    let val = case TR.hexadecimal dw of
                Right (n, _) -> REG_DWORD n
                Left e -> error $ T.pack e
{-
    val <- case readEither @Text @Word32 dw of
                Right x -> pure $ REG_DWORD x
                Left e -> fail $ T.unpack e
-}
    endOfLine
    pure val

hexP :: Parser Value
hexP = do
    vals <- csvP
    --skip isEndOfLine
    if null vals
    then endOfLine
    else pure ()
    pure $ REG_BINARY $ BS.pack $ fromHexList vals

expandszP :: Parser Value
expandszP = REG_BINARY . BS.pack . fromHexList <$> csvP
{-
    vals <- csvP
    --skip isEndOfLine
    pure $ REG_BINARY $ BS.pack $ fromHexList vals
-}

multiszP :: Parser Value
multiszP = REG_BINARY . BS.pack . fromHexList <$> csvP

--fromHexList :: [Text] -> [[(Integer, String)]]
fromHexList = map (fromHex . T.unpack)
    where fromHex x = case readHex x of
                        [] -> error $ "cannot read '" <> T.pack x <> "' as HEX"
                        (n,_):_ -> n

csvP :: Parser [Text]
csvP = many' next
    where next = do d <- takeWhile1 isHexDigit
                    choice [nextLine, skip (','==), endOfLine]
                    pure d
          nextLine = do skip (','==)
                        skip ('\\'==)
                        endOfLine
                        skipMany space
          isHexDigit = inClass "01234567890abcdefABCDEF"

item :: Parser RegItem
item = do
    endOfLine
    path <- takeTill isEndOfLine
    endOfLine
    vals <- many' keyVal
    pure (Path (T.unpack path), vals)

readUtf16 :: FilePath -> IO TL.Text
readUtf16 p = do
    h <- openFile p ReadMode
    hSetEncoding h utf16
    TL.hGetContents h

--utf16toUtf8T :: Text -> Text
--utf16toUtf8T = T.encodeUtf16LE
