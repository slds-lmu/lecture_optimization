# -*- coding: utf-8 -*-
"""
Created on Thu Oct 28 08:35:57 2021

@author: Katharina Rath
"""

# constrained optimization
import numpy as np
import matplotlib.pyplot as plt
def f(x, y):
    return x + y

def g(x, y):
    return x**2 + y**2 - 1

xmin = -1.5
xmax = 1.5
t = np.linspace(0, 2*np.pi, 300)
xv = np.linspace(xmin, xmax, 100)
yv = np.linspace(xmin, xmax, 100)
Xv, Yv = np.meshgrid(xv, yv)
plt.figure()
plt.contourf(xv, yv, f(Xv, Yv))
plt.plot(np.cos(t), np.sin(t), 'k-')
plt.plot(-np.sqrt(2)/2, -np.sqrt(2)/2, 'r.', markersize = 15)
plt.xlabel(r'$x_1$')
plt.ylabel(r'$x_2$')
plt.xlim([xmin, xmax])
plt.ylim([xmin, xmax])
cbar = plt.colorbar()
cbar.set_label(r'$f(x_1, x_2)$')
plt.axis('equal')
plt.savefig('../figure_man/unit_circle.png')