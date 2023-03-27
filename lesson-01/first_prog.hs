main = do
  print "email: Who is the email for?"
  recipient <- getLine
  print "email: What is the Title?"
  title <- getLine
  print "email: Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)

toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart author = "Thanks,\n" ++ author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author