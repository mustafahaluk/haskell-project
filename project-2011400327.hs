-- compiling: yes
-- complete: yes
-- mustafa haluk aydın
import Data.List
import Data.Function (on)

students = [("name", "id", "gpa"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]

-- ge1 (a,b)
-- ge2 (a,b)
-- se1 (a,b) c
-- se2 (a,b) c
--
-- se1,se2 and ge1, ge2 functions getting and setting elements on (a,b) tuples
--
-- Ezamples: 
-- ge1 (5,3) => 5
-- ge1 (5,3) => 3
-- se1 (5,3) 4 => (4,3)
-- se2 (5,3) 4 => (5,4)
ge1  (a,_) = a
ge2  (_,a) = a
se1 (a,b) d = (d,b)
se2 (a,b) d = (a,d)

-- get1 (a,b,c)
-- get2 (a,b,c)
-- get2 (a,b,c)
-- set1 (a,b,c) d
-- set2 (a,b,c) d
-- set3 (a,b,c) d
--
-- set1,set2,set3 and get1, get2, get3 functions getting and setting elements on (a,b,c) tuples
--
-- Ezamples: 
-- get1 (5,3,11) => 5
-- get2 (5,3,11) => 3
-- get3 (5,3,11) => 11
-- set1 (5,3,11) 4 => (4,3,11)
-- set2 (5,3,11) 4 => (5,4,11)
-- set3 (5,3,11) 4 => (5,3,4)
get1 (a,_,_) = a
get2 (_,a,_) = a
get3 (_,_,a) = a
set1 (a,b,c) d = (d,b,c)
set2 (a,b,c) d = (a,d,c)
set3 (a,b,c) d = (a,b,d)

-- table x
-- 
-- checks whether given list is a table(3.1 table table-name list)
--
-- Ezamples: 
-- table [("name", "id", "gpa"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]
-- => true
-- table [("name", "id", "gpa"), ("ali", "1", "3.2"), ("ayse", "2", "3.7", "eggs")]
-- => <interactive>
--    Couldn't match expected type `(String, String, String)'
--                with actual type `([Char], [Char], [Char], [Char])'
--    In the expression: ("ayse", "2", "3.7", "eggs")
--    In the first argument of `table', namely
--      `[("name", "id", "gpa"), ("ali", "1", "3.2"),
--        ("ayse", "2", "3.7", "eggs")]'
--    In the expression:
--      table
--        [("name", "id", "gpa"), ("ali", "1", "3.2"),
--         ("ayse", "2", "3.7", "eggs")]
table ::[(String, String, String)]-> Bool
table a = True 


-- createtable (a,b,c) 
-- 
-- creates table from given tuple(3.2 creating table)
-- 
-- Ezamples: 
-- createtable (11,1,2) 
-- => [(11, 1, 2)]
-- createtable ("name", "id", "gpa") 
-- => [("name", "id", "gpa")]
createtable :: (a,b,c) -> [(a, b, c)]
createtable (a,b,c) =[(a, b, c)]

-- get x (a,b,c) d 
-- 
-- gets corresponding element in given tuple (3.3 get table row field)
-- 
-- Ezamples: 
--
-- get students ("ali", "1", "3.2") "gpa" 
-- => "3.2"
-- get students ("ali", "1", "3.2") "name" 
-- => "ali"
get :: [(String, String, String)] ->(String, String , String) -> String -> String
get (x:xs) (a,b,c) d 
            | d==get1 x  = a
            | d==get2 x  = b
            | d==get3 x  = c
-- alter x t a 
-- 
-- alters given tuple elements from given list according to given specified list(3.4 alter table row fieldsvalues)
-- 
-- Ezamples: 
--
-- alter students ("ali", "1", "3.2") [("name", "ahmet"), ("gpa", "3.3")]
-- => ("ahmet", "1", "3.3")
-- alter students ("ali", "1", "3.2") [("id", "3"),("name", "hamit") ]
-- => ("hamit", "3", "3.3")
alter :: [(String, String, String)]->(String, String , String)->[(String, String)]->(String, String , String)
alter (x:xs) (a,b,c) (d:ds) 
            | ge1 d == get1 x = alter (x:xs) (ge2 d,b,c) ds
            | ge1 d == get2 x = alter (x:xs) (a,ge2 d,c) ds
            | ge1 d == get3 x = alter (x:xs) (a,b,ge2 d) ds
alter (x:xs) (a,b,c) (_) = (a,b,c)

-- addrow x (a,b,c) 
-- 
-- adds given tuple to the given list(3.5 addrow table row)
-- 
-- Ezamples: 
--
-- addrow students ("asli", "3", "2.8") 
-- => [("name", "id", "gpa"), ("asli", "3", "2.8"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]
-- addrow students ("veli", "10", "3.2") 
-- => [("name", "id", "gpa"), ("veli", "10", "3.2"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]
addrow :: [(String, String, String)]->(String, String , String)->[(String, String, String)]
addrow (x:xs) a = (x:a:xs)

-- addrows x [(a,b,c)] 
-- 
-- adds given list of tuples to the given list(3.6 addrows table row)
-- 
-- Ezamples: 
--
-- addrow students [("asli", "3", "2.8"), ("ahmet", "4", "0.8")] 
-- => [("name", "id", "gpa"), ("asli", "3", "2.8"), ("ahmet", "4", "0.8"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]
-- addrow students [("asli", "3", "2.8"),("veli", "10", "3.2")] 
-- => [("name", "id", "gpa"), ("asli", "3", "2.8"), ("veli", "10", "3.2"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]
addrows :: [(String, String, String)]->[(String, String , String)]->[(String, String, String)]
addrows (x:xs) a = (x:(a ++ xs))

-- deleterows x [(a,c)] 
-- 
-- deletes tuples from the given list according to given list of specified elements(3.7 deleterows table field-value)
-- 
-- Ezamples:
-- 
-- deleterows students [("name", "ali")] 
-- => [("name", "id", "gpa"), ("ayse", "2", "3.7")]
-- deleterows students [("name", "ayse")] 
-- => [("name", "id", "gpa"), ("ali", "1", "3.2")]
deleterows :: [(String, String, String)]->[(String, String)]->[(String, String, String)]
deleterows (x:xs) a =
                            let dt=deletedtuple ("","","") x a
                            in (x:xs) \\ (extracttuple (x:xs) dt) 

-- extracttuple x (a,b,c)
--
-- extracts tuples from given list according to given tuple's elements
--
-- Ezamples:
--
-- extracttuple students ("ali","","") 
-- => [("ali", "1", "3.2")]
-- extracttuple students ("","1","3.2") 
-- => [("ali", "1", "3.2")]
extracttuple :: [(String, String, String)]->(String, String, String)->[(String, String, String)]
extracttuple (x:xs) a =
                    let
                    t1 = off (x:xs) (get1 a) 1
                    t2 = off (x:xs) (get2 a) 2
                    t3 = off (x:xs) (get3 a) 3
                    in  intersect (intersect t1 t2) t3

-- off x a b
--(b is 1 for searching (c,_,_),b is 2 for searching (_,c,_),b is 3 for searching (_,_,c))
-- extracts tuples from given list according to given elements
--
-- Ezamples:
--
-- off students "ali" 1 
-- => [("ali", "1", "3.2")]
-- off students "1" 2 
-- => [("ali", "1", "3.2")]
off :: [(String, String, String)] -> String-> Int ->[(String, String, String)]
off (x:xs) a n
               |(a=="") = xs
               |(n==1)&&(get1 x == a) = (x:(off xs a 1))
               |(n==1)&&(get1 x /= a) = ((off xs a 1))
               |(n==2)&&(get2 x == a) = (x:(off xs a 2))
               |(n==2)&&(get2 x /= a) = ((off xs a 2))
               |(n==3)&&(get3 x == a) = (x:(off xs a 3))
               |(n==3)&&(get3 x /= a) = ((off xs a 3))
off (_) a b = []

-- deletedtuple x a b
--
-- extracts the general tuple from given specified list
--
-- Ezamples:
--
-- deletedtuple ("","","") ("name", "id", "gpa") [("name", "ali")]
-- => ("ali", "", "")
-- deletedtuple ("","","") ("name", "id", "gpa") [("name", "ali"),("gpa","3.3")]
-- => ("ali", "", "3.3")
deletedtuple ::(String, String, String)->(String, String, String)->[(String, String)]->(String, String ,String)
deletedtuple (a,b,c) x (ax:axs)
            | ge1 ax == get1 x = deletedtuple (ge2 ax,b,c) x axs
            | ge1 ax == get2 x = deletedtuple (a,ge2 ax,c) x axs
            | ge1 ax == get3 x = deletedtuple (a,b,ge2 ax) x axs
deletedtuple (a,b,c) x (_) = (a,b,c)

-- takerow x a
--
-- extracts tuples from given list according to given specified list's first element
--
-- Ezamples:
--
-- takerow students [("name", "ali")]
-- => [("ali", "1", "3.2")]
-- takerow students [("gpa", "3.7")]
-- => [("ayse", "2", "3.7")]
takerow :: [(String, String, String)] -> [(String, String)] -> [(String, String, String)]
takerow (x:xs) (ax:axs) 
                | get1 x==ge1 ax  = off (x:xs) (ge2 ax) 1
                | get2 x==ge1 ax  = off (x:xs) (ge2 ax) 2
                | get3 x==ge1 ax  = off (x:xs) (ge2 ax) 3

-- changed x a
--
-- changes the given list elements according to given specified list
--
-- Ezamples:
-- changed ("name", "id", "gpa") [("ayse", "2", "3.7")]  [("name", "ayse"),("gpa","1.1")] 
-- =>[("ayse", "2", "1.1")]
-- changed ("name", "id", "gpa") [("ayse", "2", "3.7")]  [("gpa", "3.7"),("name","muhittin")] 
-- =>[("muhittin", "2", "3.7")]
changed :: (String, String, String)->[(String, String, String)] -> [(String, String)] -> [(String, String, String)]
changed f (x:xs) (ax:axs) 
                           | (get1 f==ge1 ax)&&(get2 f==ge1 (head axs)) = (((get1 x),(ge2 (head axs)),(get3 x)): changed f (xs) (ax:axs))
                           | (get1 f==ge1 ax)&&(get3 f==ge1 (head axs)) = (((get1 x),(get2 x),(ge2 (head axs))): changed f (xs) (ax:axs))
                           | (get2 f==ge1 ax)&&(get1 f==ge1 (head axs)) = (((ge2 (head axs)),(get2 x),(get3 x)): changed f (xs) (ax:axs))
                           | (get2 f==ge1 ax)&&(get3 f==ge1 (head axs)) = (((get1 x),(get2 x),(ge2 (head axs))): changed f (xs) (ax:axs))
                           | (get3 f==ge1 ax)&&(get1 f==ge1 (head axs)) = (((ge2 (head axs)),(get2 x),(get3 x)): changed f (xs) (ax:axs))
                           | (get3 f==ge1 ax)&&(get2 f==ge1 (head axs)) = (((get1 x),(ge2 (head axs)),(get3 x)): changed f (xs) (ax:axs))
changed f (_) (ax:axs) = []

-- updaterows x [(a,c)] 
-- 
-- updates tuples from the given list according to given list of specified elements(3.8updaterows table fields-values)
-- 
-- Ezamples:
-- 
-- updaterows students [("name", "ali"), ("gpa", "3.9")] 
-- => [("name", "id", "gpa"), ("ali", "1", "3.9"), ("ayse", "2", "3.7"))]
-- updaterows students [("name", "ayse"), ("id", "12")] 
-- => [("name", "id", "gpa"), ("ali", "1", "3.2"), ("ayse", "12", "3.7"))]
updaterows :: [(String, String, String)] -> [(String, String)] -> [(String, String, String)]
updaterows (x:xs) (ax:axs) =
                        let
                        u = takerow (x:xs) (ax:axs)
                        up = (x:xs) \\ u
                        in up ++ (changed x u (ax:axs))  
-- selectrows x  (a,c) (d,e) 
-- 
-- extract 2-element-tuples from the given list according to given two elements(3.9 selectrows table fields field-value)
-- 
-- Ezamples:
-- 
-- selectrows students ("gpa", "id") ("name", "ali") 
-- => [("gpa", "id"), ("3.2", "1")]
--  selectrows students ("name", "id") ("gpa", "3.2") 
-- => [("name","id"),("ali","1")]
selectrows :: [(String, String, String)] -> (String, String) -> (String, String) -> [(String, String)]
selectrows (x:xm:xs) a b 
                    | (ge1 b == get1 x)&&(ge1 a == get2 x)&&(get1 xm == ge2 b)       =(selectrows (x:xs) a b ) ++ [((get2 xm),(get3 xm))]
                    | (ge1 b == get1 x)&&(ge2 a == get2 x)&&(get1 xm == ge2 b)       =(selectrows (x:xs) a b ) ++ [((get3 xm),(get2 xm))]
                    | (ge1 b == get1 x)&&(ge1 a == get2 x)&&(get1 xm /= ge2 b)       =(selectrows (x:xs) a b )
                    | (ge1 b == get1 x)&&(ge2 a == get2 x)&&(get1 xm /= ge2 b)       =(selectrows (x:xs) a b )
                    | (ge1 b == get2 x)&&(ge1 a == get1 x)&&(get2 xm == ge2 b)       =(selectrows (x:xs) a b ) ++ [((get1 xm),(get3 xm))]
                    | (ge1 b == get2 x)&&(ge1 a == get3 x)&&(get2 xm == ge2 b)       =(selectrows (x:xs) a b ) ++ [((get3 xm),(get1 xm))]
                    | (ge1 b == get2 x)&&(ge1 a == get1 x)&&(get2 xm /= ge2 b)       =(selectrows (x:xs) a b )
                    | (ge1 b == get2 x)&&(ge1 a == get3 x)&&(get2 xm /= ge2 b)       =(selectrows (x:xs) a b )
                    | (ge1 b == get3 x)&&(ge1 a == get1 x)&&(get3 xm == ge2 b)       =(selectrows (x:xs) a b ) ++ [((get1 xm),(get2 xm))]
                    | (ge1 b == get3 x)&&(ge1 a == get2 x)&&(get3 xm == ge2 b)       =(selectrows (x:xs) a b ) ++ [((get2 xm),(get1 xm))]
                    | (ge1 b == get3 x)&&(ge1 a == get1 x)&&(get3 xm /= ge2 b)       =(selectrows (x:xs) a b ) 
                    | (ge1 b == get3 x)&&(ge1 a == get2 x)&&(get3 xm /= ge2 b)       =(selectrows (x:xs) a b )
selectrows (x:xm:[]) a b=[(a)] 
selectrows (x:[]) a b=[(a)]

-- allrows x a 
-- 
-- gets all same fields elements  from given list according to given elements(3.10 allrows table row)
-- 
-- Ezamples:
-- 
-- allrows students "gpa" 
-- => ["gpa", "3.7", "3.2"]
-- allrows students "name" 
-- => ["name","ayse","ali"]
allrows :: [(String, String, String)] -> String -> [String]
allrows (x:xm:xs) a 
                    | (a == get1 x) = (allrows (x:xs) a ) ++ [(get1 xm)]
                    | (a == get2 x) = (allrows (x:xs) a ) ++ [(get2 xm)]
                    | (a == get3 x) = (allrows (x:xs) a ) ++ [(get3 xm)]
allrows (x:xm:[]) a =[(a)] 
allrows (x:[]) a =[(a)]

-- sortby x a 
-- 
-- sorts given list according to given elements(3.11 sortby table field)
-- 
-- Ezamples:
-- 
-- sortby students "id" 
-- => [("name", "id", "gpa"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]
-- sortby students "gpa" 
-- => [("name", "id", "gpa"), ("ali", "1", "3.2"), ("ayse", "2", "3.7")]
sortby::[(String, String, String)]->String->[(String, String, String)]
sortby (x:xs) b 
                 | b==get1 x = x:(sortBy sortfir (sortBy sortsec (sortBy sortthr xs)))
                 | b==get2 x = x:(sortBy sortsec (sortBy sortfir (sortBy sortthr xs)))
                 | b==get3 x = x:(sortBy sortthr (sortBy sortfir (sortBy sortsec xs)))

-- sortfir a b  for sorting two tuples according to first elements
-- sortsec a b  for sorting two tuples according to second elements
-- sortthr a b  for sorting two tuples according to third elements
-- 
-- returns GT or LT  according to given elements
-- 
-- Ezamples:
-- 
-- sortfir ("veli", "1", "3.2")  ("ayse", "2", "3.7") 
-- => GT
-- sortthr ("veli", "1", "3.2")  ("ayse", "2", "3.7") 
-- => LT
sortfir (a1, b1, c1) (a2, b2, c2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2 = compare b1 b2
sortsec (a1, b1, c1) (a2, b2, c2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | b1 == b2 = compare a1 a2
sortthr (a1, b1, c1) (a2, b2, c2)
  | c1 < c2 = LT
  | c1 > c2 = GT
  | c1 == c2 = compare a1 a2

-- distinct x
-- 
-- removes any duplicate tuples in given list(3.12 distinct table)
-- 
-- Ezamples:
-- 
-- distinct [("name", "id", "gpa"), ("ali", "1", "3.3"), ("ayse", "2", "3.4"),("ali", "2", "3.3"), ("ali", "1", "3.3")] 
-- => [("name", "id", "gpa"), ("ali", "1", "3.3"),("ali", "2", "3.3"), ("ayse", "2", "3.4")]
-- distinct [("name", "id", "gpa"), ("ali", "1", "3.3"),("ali", "1", "3.3"), ("ali", "1", "3.3")] 
-- => [("name", "id", "gpa"), ("ali", "1", "3.3")]
distinct ::[(String, String, String)] ->[(String, String, String)]
distinct (x:xm:xs) =  x:abo (tail (sortby (x:xm:xs) (get1 x)))

--abo x
--
-- takes a sorted list and removes adjacent tuples that are same 
-- 
-- Ezamples:
-- in example below, abo function does nothing
-- abo [("ali", "1", "3.3"), ("ayse", "2", "3.4"),("ali", "1", "3.3")] 
-- => [("ali", "1", "3.3"), ("ayse", "2", "3.4"),("ali", "1", "3.3")] 
-- but for same list in order to "name", it removes duplicates
-- abo [("ali", "1", "3.3"),("ali", "1", "3.3"), ("ayse", "2", "3.4")] 
-- => [("ali", "1", "3.3"), ("ayse", "2", "3.4")] 
abo ::[(String, String, String)] ->[(String, String, String)]
abo (x:xm:xs)
             | (x==xm) = (abo (xm:xs))
             | (x/=xm) = x:(abo (xm:xs))
abo (x:xm:_) 
             | (x==xm) = [x]
             | (x/=xm) = [x]++[xm] 
abo (x:_) = [x]