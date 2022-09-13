import numpy as np
import matplotlib.pyplot as plt
import os
dirpath = os.path.dirname(__file__)

x = np.linspace(-15, 15, 1000)
y = np.zeros(len(x))

plt.figure(figsize=(10, 8))

# abs
plt.plot(x, abs(x), color='blue', label='abs')
abs1 = [-1, 1]
abs2 = [1.5, 1.5]
absx = [abs1[0], abs2[0]]
absy = [abs1[1], abs2[1]]
plt.plot(absx, absy, 'bo', linestyle='--')

# log
plt.plot(x, np.log(x), color='green', label='ln')
ln1 = [0.5, np.log(0.5)]
ln2 = [2, np.log(2)]
lnx = [ln1[0], ln2[0]]
lny = [ln1[1], ln2[1]]
plt.plot(lnx, lny, 'go', linestyle='--')


# exp(-x^2)
plt.plot(x, np.exp(-(x**2)), color='red', label='exp(-x$^2$)')
exp1 = [-2.2, np.exp(-((-2.2)**2))]
exp2 = [0.5, np.exp(-(0.5**2))]
expx = [exp1[0], exp2[0]]
expy = [exp1[1], exp2[1]]
plt.plot(expx, expy, 'ro', linestyle='--')

plt.xlabel('x')
plt.ylabel('y')
plt.xlim(-3, 3)
plt.ylim(-1, 2)
plt.grid()
plt.legend(prop={'size': 20})
plt.savefig(os.path.join(dirpath, 'conv_conc_functions'))
plt.show()
