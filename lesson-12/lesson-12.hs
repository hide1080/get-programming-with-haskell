type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
type Age = Int
type Height = Int

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = "PATIENT:" ++ fname ++ ", " ++ lname ++ ", " ++ show age ++ ", " ++ show height

type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
firstName patient = fst patient

lastName :: PatientName -> LastName
lastName patient = snd patient

--QC12-1
patientInfo_QC12_1 :: PatientName -> Age -> Height -> String
patientInfo_QC12_1 patientName age height = "PATIENT:" ++ firstName patientName ++ ", " ++ lastName patientName ++ ", " ++ show age ++ ", " ++ show height
patientInfo_QC12_A :: PatientName -> Int -> Int -> String
patientInfo_QC12_A (fn, ln) age height = name ++ " " ++ ageHeight
  where
    name = ln ++ ", " ++ fn
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

--12.2
data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos
patient2BT :: BloodType
patient2BT = BloodType O Neg
patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

--12.3
-- data Patient = Patient Name Sex Int Int Int BloodType
data Patient = Patient {
  name :: Name,
  sex :: Sex,
  age :: Age,
  height :: Int,
  weight :: Int,
  bloodType :: BloodType
}

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

--QC12-2
janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 25 70 170 (BloodType O Pos)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ b) = b

--QC12-3
jackieSmith :: Patient
jackieSmith = Patient {
  name = Name "Jackie" "Smith",
  age = 43,
  sex = Female,
  weight = 115,
  height = 62,
  bloodType = BloodType O Neg
}

jackieSmithUpdated = jackieSmith { age = 44 }

--Q12-1
canPatientDonateTo :: Patient -> Patient -> Bool
canPatientDonateTo a b = canDonateTo (bloodType a) (bloodType b)

--Q12-2
showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary a = 
  "**********\n" ++
  "Patient Name: " ++ (showName (name a)) ++ "\n" ++
  "Sex:" ++ showSex (sex a) ++ "\n" ++
  "Age: " ++ show (age a) ++ "\n" ++
  "Height: " ++ show (height a) ++ "\n" ++
  "Weight: " ++ show (weight a) ++ "\n" ++
  "Blood Type: " ++ showBloodType (bloodType a) ++ "\n" ++
  "**********\n"
