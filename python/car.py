from __future__ import annotations
import enum
from typing import Dict, List
import pandas as pd
import finance


class VehicleType(enum.Enum):
    car = 1
    bike = 2

class EngineType(enum.Enum):
    gas = 1
    electric = 2


# Assuming that if bike, I wouldn't sell car. This impacts what we charge the insurance
class Vehicle():
    gas_cost = 4.7
    elec_cost = 0.128

    car_ins = 894.30
    bike_ins = 80

    tire_cost = 100

    def __init__(self, name, vehicle_type: VehicleType, engine_type: EngineType, distance: int, efficiency: int, price) -> None:
        self.vehicle_type = vehicle_type
        self.engine_type = engine_type
        self.distance = distance
        # gas   mpg
        # elec  m/kWh
        self.efficiency = efficiency
        self.price = price
        self.name = name

    @property
    def fuel_cost(self) -> float:
        match self.engine_type:
            case EngineType.gas:
                return self.gas_cost / self.efficiency
            case EngineType.electric:
                return self.elec_cost / self.efficiency
    @property
    def insurance(self) -> float:
        match self.vehicle_type:
            case VehicleType.car:
                return self.car_ins
            case VehicleType.bike:
                return self.bike_ins
    @property
    def tires(self) -> float:
        match self.vehicle_type:
            case VehicleType.car:
                return self.tire_cost * 4
            case VehicleType.bike:
                return self.tire_cost * 2
    @property
    def taxes(self) -> float:
        match self.vehicle_type:
            case VehicleType.car:
                return 75.5
            case VehicleType.bike:
                return 40

    @property
    def maintenance(self) -> float:
        if self.vehicle_type == VehicleType.bike and self.engine_type == EngineType.electric:
            return 100 / 10000 + 2600 / 200000
        else:
            return 100 / 5000

    def costs(self) -> Dict[str, float]:
        return {
                "tires": self.tires / 50000,
                "insurance": self.insurance / self.distance,
                "taxes": self.taxes / self.distance,
                "fuel": self.fuel_cost,
                "maintenance": self.maintenance 
                }
    def total_cost(self) -> float:
        return sum(n for k,n in self.costs().items())
    
    @property
    def _tax_discount(self) -> float:
        return 7500 if self.vehicle_type == VehicleType.car and self.engine_type == EngineType.electric else 0

    def economic_analysis(self) -> pd.DataFrame:
        vehicle_standard = Vehicle("", VehicleType.car, EngineType.gas, self.distance, 20,3500).total_cost()
        revenue_src = vehicle_standard*self.distance
        
        down_payment = 0.2
        loan_years = 5
        loan_rate = 0.06
        years = 10

        capital_cost = [self.price*down_payment] + [(1-down_payment) * self.price / loan_years]*loan_years
        interest = [0] + [(self.price - sum(capital_cost[:i+1])) * loan_rate for i in range(1,loan_years+1)]

        capital_cost = capital_cost + [0]*(years+1 - len(capital_cost))
        capital_cost[-1] = -self.price/2
        interest = interest + [0]*(years+1 - len(interest))
        cost = [0.] + [self.total_cost()*self.distance]*years
        revenue = [self._tax_discount] + [revenue_src]*years
        df = pd.DataFrame({"capital cost": capital_cost, "interest": interest, "cost": cost, "revenue": revenue})
        df["cash flow"] = df["revenue"] - df["cost"] - df["interest"] - df["capital cost"]
        # print(df)
        return df

    def roroi(self) -> float:
        df = self.economic_analysis() 
        return finance.roroi(df["cash flow"])



if __name__ == "__main__":
    car = Vehicle("Hyundai Sonata 2017", VehicleType.car, EngineType.gas, 5000, 20,3500)
    zero = Vehicle("Honda Rebel 300 ABS", VehicleType.bike, EngineType.gas, 5000, 70, 6000)
    print(zero.costs())
    print(zero.total_cost())
    print(zero.economic_analysis())
    print(f"{zero.roroi():.2%}")



