--L34-1
-- import GHC.List (errorEmptyList)
-- head :: [a] -> a
-- head (x : _) = x
-- head [] = errorEmptyList "head"

--L34-2
-- class Monoid m where
--   mempty :: m
--   mappend :: m -> m -> m
--   mconcat :: [m] -> m

--L34-3
head :: Monoid a => [a] -> a
head (x : xs) = x
head [] = mempty

--L34-4
example :: [[Int]]
example = []

-- GHCIで以下のコードを実行する
-- ‘Prelude.head’と‘Main.head’の名前が衝突してエラーになる。

-- ghci> head example 
--
-- <interactive>:67:1: error:
--     Ambiguous occurrence ‘head’
--     It could refer to
--        either ‘Prelude.head’,
--               imported from ‘Prelude’ at lesson-34\lesson-34.hs:1:1
--               (and originally defined in ‘GHC.List’)
--            or ‘Main.head’, defined at lesson-34\lesson-34.hs:16:1

--QC34-1
length :: Int
length = 8
--Preludeのlengthと競合することなく使用するにはモジュール名で修飾する。
--ghci> Main.length
--8
