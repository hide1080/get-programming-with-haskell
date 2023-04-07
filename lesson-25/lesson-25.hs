{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

--L25-1
sampleBytes :: B.ByteString
sampleBytes = "Hello!"

--L25-2
-- Data.ByteStringのunpackは[Word8]を返すためエラーになる
-- B.unpack :: ByteString -> [Word8]
-- sampleString = B.unpack sampleBytes
sampleString :: String
sampleString = BC.unpack sampleBytes

--QC25-1
bcInt :: BC.ByteString
bcInt = "6"
toInt :: BC.ByteString -> Int
toInt = read . BC.unpack

--L25-14
yamadatarouBC :: BC.ByteString
yamadatarouBC = "山田太郎"

--L25-15
yamadatarouText :: T.Text
yamadatarouText = "山田太郎"

--L25-15
yamadatarouB :: B.ByteString
yamadatarouB = (BC.pack . T.unpack) yamadatarouText

--L25-17
yamadatarouSafe :: B.ByteString
yamadatarouSafe = TE.encodeUtf8 yamadatarouText
