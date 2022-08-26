# -*- coding: utf-8 -*-
"""
Created on Thu Oct 28 08:52:54 2021

@author: Katharina Rath
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
def f(x, y):
    return (0.5*x**2 + y**2 + x*y)

def grad_f(x,y):
    return 1*x + y, 2*y +x

xmin = -2.5
xmax = 2.5
t = np.linspace(0, 2*np.pi, 300)
xv = np.linspace(xmin, xmax, 100)
yv = np.linspace(xmin, xmax, 100)
Xv, Yv = np.meshgrid(xv, yv)
xp = 0.5
yp = 0.5
fig = plt.figure()
ax = fig.add_subplot(111)
plt.contourf(xv, yv, f(Xv, Yv))
plt.arrow(xp, yp, grad_f(xp,yp)[0], grad_f(xp,yp)[1], width=0.02, edgecolor = 'red', facecolor = 'red')
plt.annotate(r'$\nabla f(x_1,x_2)$', xy=(1.0, 0.9), color = 'red', fontsize = 18)
plt.arrow(xp, yp, 0, grad_f(xp,yp)[1], width=0.02, edgecolor = 'red', facecolor = 'red', alpha = 0.5)
plt.arrow(xp, yp, grad_f(xp,yp)[0], 0, width=0.02, edgecolor = 'red', facecolor = 'red', alpha = 0.5)
plt.annotate(r'$\frac{\partial f(x_1,x_2)}{\partial x_1}$', xy=(1.15, 0.1), color = 'red', fontsize = 15)
plt.annotate(r'$\frac{\partial f(x_1,x_2)}{\partial x_2}$', xy=(-0.4, 1.8), color = 'red', fontsize = 15)
plt.xlabel(r'$x_1$', fontsize = 20)
plt.ylabel(r'$x_2$', fontsize = 20)
plt.xticks([])
plt.yticks([])
ax.set_aspect('equal', adjustable='box')
plt.savefig('../figure_man/grad_unit_vectors.png')

#%%

#3D plot
fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
surf = ax.plot_surface(Xv, Yv, f(Xv, Yv), cmap=cm.viridis,
                        linewidth=0, antialiased=False)

ax.set_xlabel(r'$x_1$')
ax.set_ylabel(r'$x_2$')
ax.set_zlabel(r'$f(x_1, x_2)$')
# fig.colorbar(surf)
plt.tight_layout()
plt.savefig('../figure_man/gradient2.png')