module Vehicle where

data Engine = Gas | Electric deriving (Show, Eq)

fuelPrice :: Engine -> Float 
fuelPrice (Gas) 	 = 4.2
fuelPrice (Electric) = 0.128


data Vehicle = Car  {name :: String, price :: Float, efficiency :: Float, engine :: Engine}
			 | Bike {name :: String, price :: Float, efficiency :: Float, engine :: Engine}
	deriving (Show)

insurance :: Vehicle -> Float
insurance (Car _ _ _ _)  = 894.30
insurance (Bike _ _ _ _) = 80

fuelCost :: Vehicle -> Float
fuelCost v = (fuelPrice $ engine v) / (efficiency v / if engine v == Electric then 33.71 else 1)

tireCost :: Vehicle -> Float
tireCost (Car _ _ _ _) = 100 * 4
tireCost (Bike _ _ _ _)= 100 * 2

taxes :: Vehicle -> Float
taxes (Car _ _ _ _) = 75.5 
taxes (Bike _ _ _ _) = 40

maintenance :: Vehicle -> Float
maintenance (Bike _ _ _ Gas) = 100 / 10000
maintenance (Bike _ _ _ Electric) = 2600 / 200000
maintenance (Car _ _ _ Gas) = 100 / 5000
maintenance (Car _ _ _ Electric) = 100 / 10000

cost :: Vehicle -> Float -> Float
cost v miles = fuelCost v + maintenance v + ((/miles) . sum) [f v | f <- [insurance, tireCost, taxes]]

taxDiscount :: Vehicle -> Float 
taxDiscount (Car _ _ _ Electric) = 10000
taxDiscount v = 0

compareCost :: Vehicle -> Vehicle -> Float -> Float
compareCost v1 v2 miles = cost v2 miles - cost v1 miles
