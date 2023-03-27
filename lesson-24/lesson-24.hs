import System.IO

--QC24-1
openStuffTxt :: IO Handle
openStuffTxt = openFile "stuff.txt" ReadMode

--QC24-3
-- ++はリスト用の演算子のため、Stringでは使えるがTextでは使えない
-- 一方、unwordsならばStringでもTextでも使えるから

--QC24-4
-- readFileは遅延読み込みであるから
-- つまり、実際に読み込む前にファイルを閉じてしまうことになるから
