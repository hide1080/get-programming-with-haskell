import Lib
    ( isPalindrome
    , preprocess
    )
import Data.Char
    ( isPunctuation
    , isSpace
    , toLower
    )
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Text as T

prop_punctuationInvariant text =  preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

--QC36-4
prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

--Q36-1
prop_whiteSpaceInvariant text =  preprocess text == preprocess noWhiteSpaceText
  where noWhiteSpaceText = T.filter (not . isSpace) text
prop_ignorecaseInvariant text =  preprocess text == preprocess lowercaseText
  where lowercaseText = T.map Data.Char.toLower text

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement
  =
    if test
    then putStrLn passStatement
    else putStrLn failStatement

main :: IO ()
main = do
  putStrLn "Running tests..."
  -- assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
  -- assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
  -- assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'"
  -- assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
  -- assert (isPalindrome ":racecar:") "passed ':racecar:'" "FAIL: ':racecar:'"

  quickCheck prop_punctuationInvariant -- try 100 tests
  -- quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant -- try 1000 tests
  --QC36-5
  quickCheck prop_reverseInvariant
  --Q36-1
  quickCheck prop_whiteSpaceInvariant
  quickCheck prop_ignorecaseInvariant

  putStrLn "done!"
