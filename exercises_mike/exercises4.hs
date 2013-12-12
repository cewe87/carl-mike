import Data.Char
import System.IO
import Data.List
import Test.QuickCheck
import Data.Maybe

-- Exercise 0 a

readSum :: IO ()
readSum = do 
			n <- readInt "How many?"
			x <- sequence [ readInt "Nr" | x <- [1..(n)]]
			print ("Sum> " ++ show (sum x))
			return ()


readInt :: String -> IO Int
readInt msg = do
    putStr (msg ++ "> ")
    hFlush stdout
    readLn
 
 -- Exercise 0 b

readIntList :: IO () 
readIntList = do
 			   		n <- readUntilZero []
 			   		print (sort n)
 			   		return ()
 			   			where
 			   				readUntilZero :: [Int] -> IO [Int]
 			   				readUntilZero zs = do
 			   										z <- readInt "Nr"
 			   										if (z == 0) then
 			   											return zs
 			   										else
 			   											readUntilZero (z:zs)

-- Exercise 0 c

repeat' :: IO Bool -> IO () -> IO ()
repeat' test op = do 
				x <- test
				o <- op
				if (x == True) then
					return ()
				else
					repeat' test op

-- HOW TO TEST ABOVE?

-- Exercise 1 
prop_LookNothing :: Eq a => a -> [(a,b)] -> Bool
prop_LookNothing a ab = case lookup a ab of 
									Nothing -> True
									_		-> False

prop_LookJust :: Eq a => a -> [(a,b)] -> Bool
prop_LookJust a ab = case lookup a ab of 
									(Just _) -> True
									_	   -> False

prop_Look :: Eq a => a -> [(a,b)] -> Bool
prop_Look a ab = prop_LookJust a ab || prop_LookNothing a ab

-- Exercise 3

game :: IO ()
game = do 
		let mn = 1 
		let mx = 100
		let l  = [mn..mx]
		putStrLn ("Think of a number between " ++ show mn ++ " and " ++ show mx ++ "!")		
		guess l
		putStrLn "Yes I WON!!"
		return ()
			where 
				guess :: [Int] -> IO ()
				guess is = do
							let i = (last is - head is) `div` 2
							let g = is !! i
							putStr ("Is it " ++ show g ++ "?> ")
							x <- getLine
							case x of
								"higher" -> guess (drop (fromJust $ elemIndex g is) is)
								"lower"  -> guess (take (fromJust $ elemIndex g is) is)
								"yes"    -> return ()
								_		 -> error "Wrong input!"

-- Above contains bug for number 2 and 100

-- Exercise 5 a

listOf' :: Integer -> Gen a -> Gen [a]
listOf' i g = sequence [ g | x <- [1..i]]


--TODO!! How to test gen
prop_listOf :: Integer -> Gen a -> Bool
prop_listOf i g = undefined


-- Exercise 5 b

pairOfLists :: Gen Integer -> Gen a -> Gen ([a],[a])
pairOfLists gi ga =	do 
						i <- gi
						li <- listOf' i ga
						li' <- listOf' i ga
						return (li,li')
							  
--Exercise 5 c

--TODO!! How to test gen
prop_zip :: Eq a => Eq b => [a] -> [b] -> Bool
prop_zip a b = unzip (zip a b) == (a,b)

--TODO!! How to test gen
prop_unzip :: [(a,b)] -> Bool
prop_unzip ab = undefined

