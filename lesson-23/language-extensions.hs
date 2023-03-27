--P281
--https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts.html
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}           --高度なパターンマッチング
{-# LANGUAGE TemplateHaskell #-}        --Haskell用のメタプログラミングツール
{-# LANGUAGE DuplicateRecordFields #-}  --レコード構文で異なる型に同じフィールド名を使うと起きる競合を解決
{-# LANGUAGE NoImplicitPrelude #-}      --カスタマイズされたPreludeを使用する（デフォルトのPreludeは使用できない）
import qualified Data.Text as T

overloadedStrings :: T.Text
overloadedStrings = "Hello"
