main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"


sumList (x:xs) = x + sumList xs
sumList [] = 0

third (a, b, c) = c

complicated (True, a, x:xlist, 5) = (a, x:xlist)

data BookInfo = BookInfo Int String [String]
                deriving (Show)

bookId (BookInfo id title authors) = id

type Address = [String]
data Customer = Customer {
    customerID :: Int,
    customerName :: String,
    customerAddress :: Address
  } deriving (Show)

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }

data List a = Cons a (List a) 
              | Nil
              deriving (Show)

data Fruit = Apple | Orange
             deriving (Show)

whichFruit2 :: String -> Fruit
whichFruit2 apple = Apple
betterFruit f = case f of
                  "apple"  -> Apple
                  "orange" -> Orange
                  _ -> Apple



data Tree a = Node a (Tree a) (Tree a)
              |Empty
              deriving (Show)

simpleTree = Node "parent"(Node "left child" Empty Empty)
                          (Node "right child" Empty Empty)

nodesAreSame (Node a _ _) (Node b _ _) | a == b = Just a
nodesAreSame _ _ = Nothing

-- 作业 http://cnhaskell.com/chp/3.html
-- 1
myLen (x:xs) = (+1) (myLen xs)
myLen [] = 0

-- 2 略
-- 3
mySumList (x:xs) = x + (mySumList xs) 
mySumList [] = 0
myAverageList xs = (mySumList xs) / (myLen xs)
-- 4 把一个列表变成回文序列
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
myPalindromSeq xs = xs ++ (myReverse xs)
-- 5 写一个函数，用来确定他的输入是否是一个回文序列．
myIsPalindromSeq xs = myReverse xs == xs
--  6 创造一个函数，用于排序一个包含许多列表的列表，其排序规则基于他的子列表的长度．（你可能要看看 Data.List 模块的 sortBy 函数．）
-- 不会
-- 7 定义一个函数，其用一个分隔符将一个包含许多列表的列表连接在一起
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = [] 
myIntersperse _ [x] = x
myIntersperse s (x:xs) = x ++ [s] ++ (myIntersperse s xs)

-- 8
-- 结束











