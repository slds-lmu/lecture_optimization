# -*- coding: utf-8 -*-
"""
Created on Mon Nov  1 18:21:09 2021

@author: Katharina Rath
"""

#taylor series approximation 2D

import numpy as np
import matplotlib.pyplot as plt


xmin = -1
xmax = 3
t = np.linspace(0, 2*np.pi, 300)
xv = np.linspace(xmin, xmax, 50)
yv = np.linspace(xmin, xmax, 50)
Xv, Yv = np.meshgrid(xv, yv)

xp = 1
yp = 1
off = 1
xt = np.linspace(xp-off, xp+off, 100)
yt = np.linspace(yp-off, yp+off, 100)
Xt, Yt = np.meshgrid(xt, yt)

def f(x, y): 
    return np.sin(2*x) + np.cos(y)

def dfdx(x, y):
    return 2*np.cos(2*x)

def dfdy(x, y):
    return -np.sin(y)

def dfdxdx(x, y): 
    return -4*np.sin(2*x)

def dfdxdy(x, y):
    return 0

def dfdydy(x, y): 
    return -np.cos(y)

def taylor1(x, y, xp, yp):
    return f(xp, yp) + (x - xp)*dfdx(xp, yp) + (y - yp)*dfdy(xp, yp) 

def taylor2(x, y, xp, yp):
    return taylor1(x, y, xp, yp) + 0.5*((x-xp)**2*dfdxdx(xp, yp) + (y - yp)**2*dfdydy(xp, yp) + 2*(x-xp)*(y-yp)*dfdxdy(xp, yp))

fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
surf = ax.plot_surface(Xv, Yv, f(Xv, Yv), alpha = 0.5)

# ax.plot_surface(Xt, Yt, taylor1(Xt, Yt, xp, yp))
ax.plot(xp, yp, f(xp, yp), 'rx')
ax.plot_surface(Xt, Yt, taylor2(Xt, Yt, xp, yp))
ax.set_xlabel(r'$x_1$', fontsize = 15)
ax.set_ylabel(r'$x_2$',  fontsize = 15)
ax.set_zlabel(r'$f(x_1, x_2)$',  fontsize = 15)
# fig.colorbar(surf)
plt.tight_layout()
for ii in range(0,360,1):
    ax.view_init(elev=10., azim=ii)
    plt.savefig("../figure_man/Taylor2D/Taylor2D_2nd%d.png" % ii)
