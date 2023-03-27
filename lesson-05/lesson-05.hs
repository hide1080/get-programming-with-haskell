--クロージャと部分適用

import Distribution.SPDX (LicenseId(AFL_1_2, AFL_3_0))
import System.Win32 (xBUTTON1)
--5.1
inc n =
  n + 1
double n =
  n * 2
square n =
  n ^ 2

ifEven f x =
  if even x
  then f x
  else x

genIfEven f = (\ x -> ifEven f x)

ifEvenInc = genIfEven inc

ifEvenDouble = genIfEven double

ifEvenSquare = genIfEven square

--QC5-1
genIfXEven x = (\ f -> ifEven f x)

--5.2
getRequestUrl host apikey resource id =
  host ++
  "/" ++
  resource ++
  "/" ++
  id ++
  "?token=" ++
  apikey

genHostRequestBuilder host =
  (\ apikey resource id ->
    getRequestUrl host apikey resource id)

exampleUrlBuilder =
  genHostRequestBuilder "http://example.com"

genApiRequestBuiler hostBuilder apiKey =
  (\ resource id ->
    hostBuilder apiKey resource id)

myExampleUrlBuilder =
  genApiRequestBuiler exampleUrlBuilder "1337hAsk3ll"

--QC5-2
genApiRequestBuiler_QC5_2 hostBuilder apiKey resource =
  (\ id ->
    hostBuilder apiKey resource id)

myExampleUrlBuilder_QC5_2 =
  genApiRequestBuiler_QC5_2 exampleUrlBuilder "1337hAsk3ll" "book"

areaOfCircle r =
  let
    pi = 3.14
  in do
    r * r * pi

add4 a b c d =
  a + b + c + d

--部分適用を使えるので以下は不要
addXto3 x =
  (\ b c d -> add4 x b c d )

--同じく部分適用で以下を代替可能
addXYto2 x y =
  (\ c d -> addXto3 x y c d )

exampleUrlBuilder2 =
  getRequestUrl "http://example.com"

myExampleUrlBuilder2 =
  exampleUrlBuilder2 "1337hAsk3ll"

--QC5-3
myExampleUrlBuilder_QC5_3 =
  myExampleUrlBuilder2 "book"

--5.3

--(from lesson-04)

sfOffice name =
  if lastName < "L"
  then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"

  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

nyOffice name =
  nameText ++ ": PO Box 789 - New York, NY, 10013"

  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name =
  nameText ++ " - PO Box 456 - Reno, NV, 89523"

  where
    nameText = snd name

dcOffice name =
  nameText ++ " - PO Box 987 - Washington DC, 98765"

  where
    nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq"

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\ name -> (fst name) ++ " " ++ (snd name))

addressLetter name location =
  locationFunction name
  where locationFunction = getLocationFunction location

addressLetterV2 =
  flipBinaryArgs addressLetter

flipBinaryArgs binaryFunction =
  (\ x y -> binaryFunction y x)

addressLetterNY =
  addressLetterV2 "ny"

--QC5-4
subtract2 :: Integer -> Integer
subtract2 =
  flip (-) 2

--Q5-1
ifEvenInc_Q5_1 = ifEven inc

ifEvenDouble_Q5_1 = ifEven double

ifEvenSquare_Q5_1 = ifEven square

--Q5-2
bynaryPartialApplication bf x =
  (\ y -> bf x y)
