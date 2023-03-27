--QC13-1
aList :: [String]
aList = ["cat", "dog", "mouse"]

--QC13-2
--Numに除算が含まれていないのは、除算は別の型クラスFractionalの関数だから
--不正解。正解は、Numのどのケースでも (/) による除算が定義されていないから

--list 13-3
class Describable a where
  describe :: a -> String

--list 13-9
data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

--QC13-3
greater = Chocolate < Vanilla

--Q13-1
-- Wordは、次の型クラスのインスタンス。数値を表す型コンストラクタ
--Integral, Real, Bounded, Enum, Eq, Ord, Read, Show, Num
--Intは8バイト整数、Wordは4バイト整数（？）
--不正解。正解は、Wordは符号なしのInt

--Q13-2
inc :: Int -> Int
inc x = x + 1
--Intのincは引数と戻り値の型がIntに限定される
--EnumのsuccはそれぞれEnumのインスタンスである型の値が使える
--不正解。Boundedにsuccが存在しないため、IntのmaxBoundにsuccを適用するとエラーになる

--Q13-3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound
  then minBound
  else succ n
