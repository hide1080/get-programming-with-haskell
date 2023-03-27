{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--L23-8
dharma :: T.Text
dharma = "きょ"

--L23-9
bgText :: T.Text
bgText = "とうきょうとっきょきょかきょく"

--L23-10
highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

main :: IO ()
main = do
  TIO.putStrLn (highlight dharma bgText)
