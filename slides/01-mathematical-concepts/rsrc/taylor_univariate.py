import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(-15, 15, 1000)
y = np.zeros(len(x))

a = 2

plt.figure(figsize=(10, 8))

plt.plot(x, np.sin(x), 'k', label='sin(x)')

y0 = y + np.sin(a)
plt.plot(x, y0, color='red', label='m=0')

y1 = y0 + np.cos(a) * (x-a)/np.math.factorial(1)
plt.plot(x, y1, color='blue', label='m=1')

y2 = y1 - np.sin(a) * ((x-a)**2)/np.math.factorial(2)
plt.plot(x, y2, color='orange', label='m=2')

y3 = y2 - np.cos(a) * ((x-a)**3)/np.math.factorial(3)
plt.plot(x, y3, color='green', label='m=3')

y4 = y3 + np.sin(a) * ((x-a)**4)/np.math.factorial(4)
plt.plot(x, y4, color='cyan', label='m=4')

plt.grid()
plt.title('Taylor polynomial for various orders at a=2', fontsize=20)
plt.xlabel('x', fontsize=20)
plt.ylabel('y', fontsize=20)
plt.xlim(-6, 10)
plt.ylim(-8, 8)
plt.legend(prop={'size': 15})
plt.savefig('C:/Users/mariu/Documents/Arbeit/SLDS/Optimization/1.2/taylor_univariate.png')
plt.show()
