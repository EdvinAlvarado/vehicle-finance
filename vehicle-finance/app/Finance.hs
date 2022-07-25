module Finance where
import Data.List (transpose)


data Loan = Loan {down :: Float, interest:: Float, period :: Int}

capitalCosts :: Float -> Loan -> [Float]
capitalCosts p loan = do
 	let down_pay = down loan
  	let years = period loan
  	let rate = interest loan
	let capital = [down_pay * p] ++ (replicate years $ (1-down_pay) * p / fromIntegral years)
	let interests = [0] ++ [rate * (p - (sum $ take (y+1) capital)) | y <- [1..years]]
  	map negate $ zipWith (+) capital interests

lumpSums :: (Num a) => a -> a -> Int -> [a]
lumpSums begin end period = [if i == 1 then begin else 0 | i <- [0..period-1]] ++ [end]

cashFlow :: (RealFrac a) => [[a]] -> Int -> [a]
cashFlow ll years = take (years+1) $ [sum t | t <- transpose ll]

discount :: (RealFrac a) => a -> [a] -> [a]
discount rate xs = [v / (1+rate)^i | (i, v) <- zip [0..] xs]

npv :: (RealFrac a) => a -> [a] -> a
npv rate xs = sum $ discount rate xs

roroi :: (RealFrac a, Ord a) => a -> a -> a -> [a] -> a
roroi rate step tolerance xs =
	if npv rate xs > tolerance
		then roroi (rate+step) step tolerance xs
	else if npv rate xs < negate tolerance
		then roroi (rate-step) step tolerance xs
	else rate

