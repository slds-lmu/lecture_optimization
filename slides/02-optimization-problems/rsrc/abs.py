# -*- coding: utf-8 -*-
"""
Created on Tue Nov  2 09:23:26 2021

@author: Katharina Rath
"""

# subgradient
import numpy as np
import matplotlib.pyplot as plt
def f(x):
    return np.abs(x)
def df(x, c):
    return c*x

xmin = -1.5
xmax = 1.5
xv = np.linspace(xmin, xmax, 1000)
xgrad = np.linspace(-1, 1, 100)
cv = np.linspace(-0.9 , 0.9, 10)
xp = 0.5
yp = 0.5

labels = [r'$g_1$', r'$g_2$', r'$g_3$', r'$g_4$', r'$g_5$', r'$g_6$', r'$g_7$', r'$g_8$', r'$g_9$', r'$g_{10}$']
fig = plt.figure()
# ax = fig.add_subplot(211)
plt.plot(xv,f(xv), linewidth = 2)
plt.annotate(r'$f(x) = |x|$', xy=(-1.2, 1.2), color = 'black', fontsize = 15)
plt.xlabel('x')

# ax = fig.add_subplot(212)
cidx = 0
for c in cv:
    plt.plot(xgrad, df(xgrad, c), '--')
    plt.annotate(labels[cidx], xy=(1.05, -c), color = 'black', fontsize = 10)
    cidx = cidx + 1
# plt.xlabel('x')
# plt.ylabel(r'$\partial f(x)$')
plt.tight_layout()
plt.savefig('../figure_man/abs.png')