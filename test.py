import subprocess
import matplotlib.pyplot as plt

price = range(32000, 50000, 100)
ret = [float(subprocess.run([r'./roroi', str(p)], stdout=subprocess.PIPE).stdout.decode('utf-8')) for p in price]

# print(ret)

plt.plot(price, ret)
plt.ylabel("RoRoI")
plt.xlabel(price)

ax = plt.gca()
ax.set_ylim([0.1, 2])

plt.show()
