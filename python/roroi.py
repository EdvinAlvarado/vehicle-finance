from car import *


df = pd.read_excel(r'~/Insync/silryk31@gmail.com/Google Drive/Finances/Car Expenses.ods', sheet_name="motorcycle", engine='odf')
print(df)

vehicle_list = [Vehicle(f'{row["brand"]} {row["Model"]}', VehicleType(row["VehicleType"]), EngineType(row["EngineType"]), 5000, row["mpg or m/kWh"], row["total price"]) for i, row in df.iterrows()]


# print(vehicle_list)
# ret = {v.name:v.roroi() for v in vehicle_list}
# ret = {k:v for k,v in sorted(ret.items(), key=lambda item: item[1])}

with open('result.txt', 'w') as f:
    for v in vehicle_list:
        f.write(f"{v.roroi():.2%}")
        f.write('\n')

# ret = pd.DataFrame.from_dict({'name': [v.name for v in vehicle_list], 'roroi': [v.roroi() for v in vehicle_list]})
# ret = ret.sort_values(by=['roroi'], ascending=False)
# print(ret)
