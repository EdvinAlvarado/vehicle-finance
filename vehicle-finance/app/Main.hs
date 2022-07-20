module Main where
import Vehicle
import Finance


v = Car "Hyundai Ioniq" 49000 110 Electric 
v_comp = Car "Hyunda Sonata" 15000 20 Gas
loan = Loan {down = 20.0e-2, period = 5, interest = 3.50e-2}
miles = 7160
years = 10
hurdle_rate = 5.00e-2 
loan_owed = 3680
loan_owed_years = 3


sell = price v_comp + taxDiscount v - loan_owed
capital = capitalCosts (price v) loan
netIncome = repeat ((*miles) $ compareCost v v_comp miles)
sales  = lumpSums (sell) ((price v) / 2) years
loanSaved = [if i >= 1 && i <= loan_owed_years then loan_owed/loan_owed_years else 0| i <- [0..]]
ret = cashFlow [capital, netIncome, sales, loanSaved] years


main :: IO ()
main = putStrLn $ show $ roroi hurdle_rate 0.01e-2 10 ret 
