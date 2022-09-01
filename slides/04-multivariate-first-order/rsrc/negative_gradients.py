import numpy as np
import matplotlib.pyplot as plt
import os
dirpath = os.path.dirname(__file__)

# from: Mathematics for Machine Learning, p227, Figure 7.2

x = np.linspace(-6, 2, 50)
y = x**4 + 7*(x**3) + 5*(x**2) - 17*x + 3

# function
nsx = -4.48
nsy = nsx**4 + 7*(nsx**3) + 5*(nsx**2) - 17*nsx + 3

# global minimum
ns = [nsx, nsy]
point1 = [nsx, -60]
point1x = [ns[0], point1[0]]
point1y = [ns[1], point1[1]]
point2 = [-6.5, nsy]
point2x = [ns[0], point2[0]]
point2y = [ns[1], point2[1]]



plt.figure()
plt.plot(x, y, color='red', label='x$^4$ + 7x$^3$ + 5x$^2$ - 17x +3')
plt.plot(point1x, point1y, linestyle='--', color='blue')
plt.plot(point2x, point2y, linestyle='--', color='blue')

#negative gradients
plt.arrow(-6, 20, 0.3, -40, head_width=0.15, head_length=4, color='black')
plt.arrow(-2.5, 0, -1, -35, head_width=0.15, head_length=4, color='black')
plt.arrow(-1, 15, 0.85, -15, head_width=0.15, head_length=4, color='black')
plt.arrow(2, 40, -0.4, -30, head_width=0.15, head_length=4, color='black')

plt.xlabel('x')
plt.ylabel('y')
plt.xlim(-6.5, 2.5)
plt.ylim(-60, 70)
plt.legend()
plt.grid()
plt.savefig(os.path.join(dirpath, 'negative_gradients'))
plt.show()

