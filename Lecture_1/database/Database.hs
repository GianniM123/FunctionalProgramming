module Main
where

type Person  =  (Name, Age, FavouriteCourse)

type Name             =  String
type Age              =  Integer
type FavouriteCourse  =  String

frits, peter, ralf, gianni, pjot :: Person
frits  =  ("Frits",  33,  "Algorithms and Data Structures")
peter  =  ("Peter",  57,  "Imperative Programming")
ralf   =  ("Ralf",   33,  "Functional Programming")
gianni =  ("Gianni", 20,  "Functional Programming")
pjot  =  ("Pjot", 24,  "Imperative Programming")

students   ::  [Person]
students   =  [frits, peter, ralf, gianni, pjot]

age :: Person -> Age
age (_n, a, _c)  =  a

name :: Person -> Name
name (n, _a, _c) = n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_n, _a, c) = c

showPerson :: Person -> String
showPerson (name, age, course) = name ++ " " ++ show age ++ " " ++ course

twins :: Person -> Person -> Bool
twins (n,a,c)(n1,a1,c1) = a == a1

increaseAge :: Person -> Person
increaseAge (n, a, c) = (n, a+1, c)

increaseAgeBy2 :: Person -> Person
increaseAgeBy2 (n, a, c) = (n, a+2, c)

promote :: Person -> Person
promote (n, a, c) = ("dr " ++ n, a, c) 

main = do
    print (map increaseAgeBy2 students)

    print (map promote students)

    print (map(\ p->(age p, name p)) (filter(\ p->name p == "Frits")students))

    print (map(\ p->(name p)) (filter(\ p->favouriteCourse p == "Functional Programming")students))

    print (map(\ p->(age p,name p)) (filter(\ p->age p >= 20 && age p < 30)students))

    print (map(\ p->(age p,name p)) (filter(\ p->age p >= 20 && age p < 30 && favouriteCourse p == "Functional Programming" )students))

    print (map(\ p->(age p,name p)) (filter(\ p->age p >= 20 && age p < 30 && favouriteCourse p == "Imperative Programming" )students))
