module Main where
import Vehicle
import Finance
import System.Environment
import Text.Printf



main :: IO ()
main = do 
	args <- getArgs
	let car_price =  read $ head args :: Float
	let v = Car "Hyundai Ioniq" car_price 110 Electric 
	let v_comp = Car "Hyunda Sonata" 15000 20 Gas
	let loan = Loan {down = 20.0e-2, period = 5, interest = 3.50e-2}
	let miles = 7160
	let years = 10
	let hurdle_rate = 5.00e-2 
	let loan_owed = 3680
	let loan_owed_years = 3


	let capital = capitalCosts (price v) loan
	let netIncome = repeat (miles * compareCost miles v v_comp)
	let saleBegin = price v_comp + taxDiscount v - loan_owed
	let sales  = lumpSums saleBegin ((price v) / 2) years
	let loanSaved = [if i >= 1 && i <= loan_owed_years then loan_owed/loan_owed_years else 0| i <- [0..]]
	let ret = cashFlow [capital, netIncome, sales, loanSaved] years

	putStrLn $ printf "%.4f" $ roroi hurdle_rate 0.01e-2 10 ret 
