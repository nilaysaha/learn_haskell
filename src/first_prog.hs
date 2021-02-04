
toPart recipient = "Dear " ++ recipient ++ "\n"
bodyPart  bookTitle = "Thanks for buying "++ bookTitle ++ ".\n"
fromPart author = "Thanks, \n"++author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author


calcChange owed given = if change > 0
                        then change
                        else 0
  where change = given - owed

doublePlusTwo x = doubleX x + 2
  where doubleX x = 2*x

inc val = val + 1

double x = 2*x

square x = x*x

doubleDouble x = (\dubs -> dubs*2) (x*2)

ifEvenInc n = if even n
              then n + 1
              else n

ifEvenDouble n = if even n
                 then n*2
                 else n

ifEvenSquare n = if even n
                 then n*n
                 else n

ifEven func n = if even n
                then func n
                else n

-- First Class Functions

ifEvenInc2 n = ifEven inc n
ifEvenDouble2 n = ifEven double n
ifEvenSquare2 n = ifEven square n

ifEvenCube n = ifEven (\x -> x^3) n  --lambda functions in first class functions.

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else EQ
  where  lastName1 = snd name1
         lastName2 = snd name2
                   
-- Closures

genIfEven f = (\x -> ifEven f x )   --Usage: (genIfEven square) 12    ouputs 144

getRequestUrl host apikey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apikey

genHostRequestBuilder host = (\apiKey resource id  -> getRequestUrl host apiKey resource id)

-- Partial functions
add4 a b c d = a + b + c + d

-- Lists

isPalindrome word = word == reverse word

respond phrase = if (elem) '!'  phrase  -- elem checks the presence of an element in an array
                 then "wow!"
                 else "uh .. okay"

takeLast n aList = reverse (take n (reverse aList))  -- take: takes the first n elements of the list.
dropLast n aList = reverse(drop n (reverse aList))   -- drop: drops the last n elements of a list
ones n = take n (cycle [1]) -- take only first n elements out of the infinite [1] array
assignToGroups n aList = zip groups aList
  where groups = cycle [1 .. n]

subseq n m aList = drop n (take m aList) -- here assumption is n > m


-- Recursion in Haskell as substitute to looping using for, while and until loops. 
myTail [x:xs] = xs
myTail [] = error "No tail for empty list"


-- Higher order functions : Functions which accepts other functions as arguments
-- True cure for recursive headaches is abstraction !
myMap f [] = []
myMap f (x:xs) = (f x):myMap f xs

tfilter = filter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]
myProduct xs = foldl (*) 1 xs

-- Use mapl to reverse an array
rcons x y = y:x
myReverse xs  = foldl rcons [] xs

-- Use filter and length to recreate elem function
myElem e xs = length(filter (\(x:ts) -> x == e)  xs) > 0

-- SKIPPING OBJECT ORIENTED PROGRAMMING IN HASKELL FOR NOW : CHAPTER 10. WILL REVIEW LATER.





main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the title of the book ?"
  bookTitle <- getLine
  print "Who is the Author ?"
  author <- getLine
  print (createEmail recipient bookTitle author)
