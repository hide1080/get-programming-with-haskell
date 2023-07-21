import Control.Monad
import Control.Applicative

--L33-1
--Model the student's concept.
data Name = Name {
    firstName :: String
  , lastName :: String
}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

--L33-2
data GradeLevel = Freshman | Sophomore | Junior | Senior
  deriving (Eq, Ord, Enum, Show)

--L33-3
data Student = Student {
    studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name
}

--L33-4
students :: [Student]
students = [
      Student 1 Senior (Name "Audre" "Lorde")
    , Student 2 Junior (Name "Leslie" "Silko")
    , Student 3 Freshman (Name "Judith" "Butler")
    , Student 4 Senior (Name "Guy" "Debord")
    , Student 5 Sophomore (Name "Jean" "Baudrillard")
    , Student 6 Junior (Name "Julia" "Kristeva")
  ]

--L33-5
-- _select ::(a -> b) -> [a] -> [b]
-- _select f tbl = do
--   row <- tbl
--   return (f row)

--L33-6
-- _where ::(a -> Bool) -> [a] -> [a]
-- _where p tbl = do
--   row <- tbl
--   guard (p row)
--   return row

--L33-7
startsWith :: Char -> String -> Bool
startsWith c s = c == head s

--L33-8
data Teacher = Teacher {
    teacherId :: Int
  , teacherName :: Name
} deriving Show

--L33-9
teachers :: [Teacher]
teachers = [
      Teacher 100 (Name "Simone" "Beauvior")
    , Teacher 200 (Name "Susan" "Sontag")
  ]

--L33-10
data Course = Course {
    courseId :: Int
  , courseTitle :: String
  , teacher :: Int
} deriving Show

--L33-11
courses :: [Course]
courses = [
      Course 101 "French" 100
    , Course 201 "English" 200
  ]

--L33-?0
_crossJoin :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_crossJoin data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  return (d1, d2)

--L33-?1
-- _join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
-- _join data1 data2 prop1 prop2 = do
--   d1 <- data1
--   d2 <- data2
--   let pairs = (d1, d2)
--   guard (prop1 (fst pairs) == prop2 (snd pairs))
--   return pairs

--L33-12
joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult

--L33-13
_hinq selectQuery joinQuery whereQuery =
  (\ joinData ->
    (\ whereResult ->
      selectQuery whereResult
    )
    (whereQuery joinData)
  ) joinQuery

--L33-14
finalResult :: [Name]
finalResult = _hinq
  (_select (teacherName . fst))
  (_join teachers courses teacherId teacher)
  (_where ((== "English") . courseTitle . snd))


--L33-5, L33-16
_select :: Monad m => (a -> b) -> m a -> m b
_select f tbl = do
  row <- tbl
  return (f row)

--L33-6, L33-16
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where p tbl = do
  row <- tbl
  guard (p row)
  return row

--L33-?1, L33-16
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) ->  m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let pairs = (d1, d2)
  guard (prop1 (fst pairs) == prop2 (snd pairs))
  return pairs

--L33-?2
data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a)
  | HINQ_ (m a -> m b) (m a)

--L33-17
runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (const True))

--L33-?3
query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ
  (_select (teacherName . fst))
  (_join teachers courses teacherId teacher)
  (_where ((== "English") . courseTitle . snd))

--L33-?4
query2 :: HINQ [] Teacher Name
query2 = HINQ_
  (_select teacherName) teachers

--L33-18
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

--L33-19
maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ
  (_select (teacherName . fst))
  (_join possibleTeacher possibleCourse teacherId teacher)
  (_where ((== "French") . courseTitle . snd))

--L33-20
missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ
  (_select (teacherName . fst))
  (_join possibleTeacher missingCourse teacherId teacher)
  (_where ((== "French") . courseTitle . snd))

--L33-21
data Enrollment = Enrollment {
    student :: Int
  , cource :: Int
} deriving Show

--L33-22
enrollments :: [Enrollment]
enrollments = [
      Enrollment 1 101
    , Enrollment 2 101
    , Enrollment 2 201
    , Enrollment 3 101
    , Enrollment 4 201
    , Enrollment 4 101
    , Enrollment 5 101
    , Enrollment 6 201
  ]

--L33-23
studentEnrollmentsQ =
  HINQ_
    (_select (\ (st, en) -> (studentName st, cource en)))
    (_join students enrollments studentId student)

--L33-24
studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

--L33-25
englishStudentsQ =
  HINQ
    (_select (fst . fst))
    (_join studentEnrollments courses snd courseId)
    (_where ((== "English") . courseTitle . snd))

--L33-26
englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

--L33-27
getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery =
      HINQ
        (_select (fst . fst))
        (_join studentEnrollments courses snd courseId)
        (_where ((== courseName) . courseTitle . snd))
