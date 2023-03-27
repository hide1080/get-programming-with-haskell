--ファーストクラス関数

import qualified Distribution.FieldGrammar as Data
ifEvenIncOriginal n =
  if even n
  then n + 1
  else n

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

ifEvenInc n =
  ifEven inc n

ifEvenDouble n =
  ifEven double n

ifEvenSquare n =
  ifEven square n

--QC4-1
ifEven3Jou n =
  ifEven (\ a -> a ^ 3) n


names =[
    ("Ian", "Curtis"),
    ("Bernad", "Summer"),
    ("Peter", "Hook"),
    ("Stephen", "Morris"),
    ("Sara", "Morris")
  ]
-- import Data.List
-- sort names

compareLastNames name1 name2 =
  if lastName1 > lastName2
  then GT
  else if lastName1 < lastName2
    then LT
    else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2
    
--QC4-2
compareLastNamesQC name1 name2 =
  if lastName1 > lastName2
  then GT
  else if lastName1 < lastName2
    then LT
    else if firstName1 > firstName2
      then GT
      else if firstName1 < firstName2
        then LT
        else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = fst name1
    firstName2 = fst name2

addressLetter name location =
  nameText ++ " - " ++ location
  where nameText = (fst name) ++ " " ++ (snd name)

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

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\ name -> (fst name) ++ " " ++ (snd name))

addressLetterV2 name location =
  locationFunction name
  where locationFunction = getLocationFunction location

--Q4-1
compareLastNamesByCompare name1 name2 =
  if lastNameCmp == EQ
  then compare (fst name1) (fst name2)
  else lastNameCmp
  where
    lastNameCmp = compare (snd name1) (snd name2)

--Q4-2
dcOffice name =
  nameText ++ " - PO Box 987 - Washington DC, 98765"

  where
    nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq"
