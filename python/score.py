from __future__ import annotations
from pandas.core.series import Series
import car
import pandas as pd


class VehicleScore:
    def __init__(self, data: pd.DataFrame) -> None:
        self.data = data

    def _speed_f(self, spd: int) -> int:
        if spd < 35:
            return 0
        if spd < 45:
            return 4
        elif spd < 55:
            return 8
        elif spd < 65:
            return 12
        elif spd < 75:
            return 16
        else:
            return 20

    def speed(self) -> pd.Series:
        return self.data["mph"].map(self._speed_f)

    def motor(self) -> pd.Series:
        b = ((self.data["fuel injection"] == True) | (self.data["EngineType"] == 2)).map(lambda x: 10 if x else 0)
        a = ((self.data["Liquid-cooled"] == True) | (self.data["EngineType"] == 2)).map(lambda x: 10 if x else 0)
        return a + b

    def storage(self) -> pd.Series:
        return self.data["storage"].map(lambda x: 5 if x == True else 0)   
    def windshield(self) -> pd.Series:
        return self.data["windshield"].map(lambda x: 2 if x == True else 0)
    def passenger_seat(self) -> pd.Series:
        return self.data["passenger seat"].map(lambda x: 5 if x == True else 0)
    def abs(self) -> pd.Series:
        return self.data["abs"].map(lambda x: 5 if x == True else 0)
    def dc_charging(self) -> pd.Series:
        return self.data["dc_charging"].map(lambda x: 3 if x == True else 0)

    def _transmission_f(self, val: str) -> int:
        if "manual" in val:
            return 0
        elif "Semi-auto" in val:
            return 3
        elif "automatic" in val:
            return 5
        else:
            return -9999
    def transmission(self) -> pd.Series:
        return self.data["transmission"].map(self._transmission_f)

    def _brakes_f(self, val: str) -> int:
        if "double" in val:
            return 5
        elif "disc" == val:
            return 4
        elif "disc/drum" == val:
            return 2
        elif "drum" == val:
            return 0
        else:
            return 0
    def brakes(self) -> pd.Series:
        return self.data["brakes"].map(self._brakes_f)

    def warranty(self) -> pd.Series:
        return self.data["warranty"].map(lambda x: int(x[0]))

    def _roroi_f(self, val: float) -> int:
        n = int(round(val*10,0))
        return n if n < 20 else 20
    def roroi(self) -> pd.Series:
        return self.data["roroi"].map(self._roroi_f)

    def _reliability_f(self, brand: str) -> int:
        if brand in ["Yamaha", "Suzuki", "Honda", "Kawasaki", "Victory", "Kymco"]:
            return 5
        elif brand in ["Harley Davidson", "Indian"]:
            return 4
        elif brand in ["Ducati", "Triumph", "Zero"]:
            return 3
        elif brand in ["BMW", "Can Am"]:
            return 0
        else:
            return 1
    def reliability(self) -> pd.Series:
        return self.data["brakes"].map(self._reliability_f)   
    
    def score(self) -> pd.Series:
        return self.speed() + self.motor() + self.storage() + self.windshield() + self.passenger_seat() + self.abs() + self.dc_charging() + self.transmission() + self.brakes() + self.roroi() + self.reliability()



    @property
    def full_data(self) -> pd.DataFrame:
        df = self.data
        df["score"] = self.score()
        return df

    @property
    def summary_data(self) -> pd.DataFrame:
        return self.full_data.filter(items=['brand', 'Model', 'style', 'score'])


df = pd.read_excel(r'~/Insync/silryk31@gmail.com/Google Drive/Finances/Car Expenses.ods', sheet_name="motorcycle", engine='odf')
c = VehicleScore(df)

# print(c.summary_data)

with pd.ExcelWriter("result.ods", engine="odf") as writer:
    c.full_data.to_excel(writer)
