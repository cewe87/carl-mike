import Data.Char
import Test.QuickCheck
import Data.List
-- assignment 0 

-- (a)
sumOfNInt :: Int -> IO ()
sumOfNInt x = do
				x <- sumOfNInt' x
				print "Summan är:"
				print x

sumOfNInt' :: Int -> IO Int
sumOfNInt'  n | n == 0 = return 0
      | otherwise  = 
      				do
      				 print "Mata in ett heltal:"
      				 s <- getLine
      				 rec <- sumOfNInt' (n-1)
      				 let i = read s :: Int
      				 return (i + rec)

-- b 

sortInts :: IO ()
sortInts = 
		 do 
		  x <- readLine0
		  let sorts = sort x
		  print x
		  print sorts
		  return ()

readLine0 :: IO [Int]
readLine0 = 
			do
			input <- getLine
			let num  = read input :: Int --convert to Int
			if num /= 0 
				then
					do
					r <- readLine0
					return ([num] ++ r)
			else 
				return [num]
				--return [] depends on if you want the 0 or not
 				

-- assignment 3 Number guessing game
game :: IO ()
game = 
	   do
		print "Think of a number between 1-100"
	   	guessNumber 1 100
	   	return ()

guessNumber :: Int -> Int -> IO ()
guessNumber	x y = 
			do
			   let val = ((y-x) `div` 2 ) + x
			   print ("is it " ++ (show val) ++ "?")
		   	   answer <- getLine
		   	   if answer == "yes" 
		   	   		then
		   	   			do
		   	   				return ()
			   else if answer == "higher" 
			   	then
			   	 do
			   	  guessNumber val y
			   else 
			   	  guessNumber x val




--listOf' :: Int -> Gen a -> Gen [a]
--listOf' n g = [arbitrary :: g] 

























