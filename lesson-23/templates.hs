--QC23-2
--このLANGUAGEプラグマを使う
{-# LANGUAGE TemplateHaskell #-}
--または以下のフラグを指定してコンパイルする
--ghc .\templates.hs -XTemplateHaskell

main :: IO ()
main = do
  print "Hello"
