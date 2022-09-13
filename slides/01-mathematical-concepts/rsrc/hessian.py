import math

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import os
dirpath = os.path.dirname(__file__)

f = lambda x, y: np.sin(x) * np.cos(y)
x = np.linspace(-3,3,50)
y = np.linspace(-3,3,50)
X, Y = np.meshgrid(x,y)
F = f(X, Y)

# determine concavity at the following points via eigenvalues:
def calc_concav(point):
    return np.array([[-np.cos(point[1])*np.sin(point[0]), -np.cos(point[0])*np.sin(point[1])],
                    [-np.cos(point[0])*np.sin(point[1]), -np.cos(point[1])*np.sin(point[0])]])


a = [np.pi/2, 0]
wa, va = np.linalg.eig(calc_concav(a))  # lamba1=lambda2=-1, v1=(0,1), v2=(1,0)

b = [0, -np.pi/2]
eig_b = np.linalg.eig(calc_concav(b))[0]  # lamba1=-1, lambda2=1, v1=(-1,1), v2=(1,1)

c = [-np.pi/2, 0]
eig_c = np.linalg.eig(calc_concav(c))[0]  # lamba1=lambda2=1, v1=(0,1), v2=(1,0)


# 3D plot f
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.set_zlim(-1.2, 1.2)
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('f(x,y)')
ax.plot_surface(X, Y, F, cmap=cm.coolwarm, lw=0.5, rstride=1, cstride=1, zorder=0)
ax.contour(X, Y, F, 10, lw=3, cmap=cm.coolwarm, linestyles="solid", offset=-0.9)
ax.view_init(22, -200)
ax.text(np.pi/2, 0, 1, 'a', size=15, color='lime', zorder=20)
ax.text(-2.4, -0.4, 0.43, 'b', size=15, color='purple', zorder=20)
ax.text(-4, 0.5, -0.7, 'c', size=15, color='orange', zorder=20)
plt.savefig(os.path.join(dirpath, 'hessian_3d'))
plt.show()

# contour plot
plt.contourf(X, Y, F, cmap=cm.coolwarm)
off = 0.09
plt.text(x=np.pi/2 - off, y=0 - off, s='a', fontsize=20, color='lime')
plt.arrow(np.pi/2, 0, 0, 1, head_width=0.15, head_length=0.2, color='black')
plt.arrow(np.pi/2, 0, 1, 0, head_width=0.15, head_length=0.2, color='black')

plt.text(x=0 - off, y=-np.pi/2 - off, s='b', fontsize=20, color='purple')
plt.arrow(0, -np.pi/2, -1/math.sqrt(2), 1/math.sqrt(2), head_width=0.15, head_length=0.2, color='black')
plt.arrow(0, -np.pi/2, 1/math.sqrt(2), 1/math.sqrt(2), head_width=0.15, head_length=0.2, color='black')

plt.text(x=-np.pi/2 - off, y=0 - off, s='c', fontsize=20, color='orange')
plt.arrow(-np.pi/2, 0, 0, 1, head_width=0.15, head_length=0.2, color='black')
plt.arrow(-np.pi/2, 0, 1, 0, head_width=0.15, head_length=0.2, color='black')

plt.colorbar()
plt.xlabel('x')
plt.ylabel('y')
plt.savefig(os.path.join(dirpath, 'hessian_contour'))
plt.show()
