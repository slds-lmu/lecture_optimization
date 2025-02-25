# ------------------------------------------------------------------------------
# univariate optimization

# FIG: plot secant method process
# ------------------------------------------------------------------------------

x = seq(3, 5.5, length = 200)
f = function(x) sin(x) * -1 - 0.3

png('../figure_man/regula_falsi.png', width = 480, height = 320)

plot(f(x) ~ x, type = "l")
x1 = 3.2; x2 = 5.3
lines(c(x1, x2), c(f(x1), f(x2)), lwd = 2, col = "lightgray")
text(x1, f(x1), expression(x[left]), pos = 2)
text(x2, f(x2), expression(x[right]), pos = 4)
x3 = x2 + f(x2) / (f(x2) - f(x1)) * (x1 - x2)
x4 = x3 + f(x3) / (f(x3) - f(x1)) * (x1 - x3)
lines(c(x3, x3), c(0, f(x3)), lwd = 2, col = "lightgray")
#lines(c(x4, x4), c(0, f(x4)), lwd = 2, col = "lightgray")
#lines(c(x1, x3), c(f(x1), f(x3)), lwd = 2, col = "lightgray")
#lines(c(x1, x4), c(f(x1), f(x4)), lwd = 2, col = "lightgray")
lines(f(x) ~ x)
points(x3, f(x3), pch = 16)
#points(x4, f(x4), pch = 16)
abline(h = 0, lty = 2, lwd = 0.5)
points(x3, 0, pch = 16, col = "lightgray")
points(c(x1, x2), c(f(x1), f(x2)), pch = 16)
text(x3, f(x3), expression(x[new]), pos = 2)
#text(x4, f(x4), expression(x^(4)), pos = 2)

dev.off()
