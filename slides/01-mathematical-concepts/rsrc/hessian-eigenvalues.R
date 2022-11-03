library(ggplot2)
library(ggpubr)

# Two quadratic functions

# Matrix 1: positive eigenvalues 
D = diag(c(2, 5))
V = matrix(c(2, 1, -1, 2), 2, byrow=FALSE) / sqrt(5)
A = V %*% D %*% t(V)

print(A)
print(eigen(A)$values)

# Plot function 
x1 = seq(-5, 5, by = 0.1)
x2 = x1
X = expand.grid(x1, x2)
y = apply(X, 1, function(x) t(x) %*% A %*% x)

df = data.frame(X, y)

p1 = ggplot(data = df, aes(x = Var1, y = Var2, z = y)) + geom_contour_filled()
p1 = p1 + xlab(expression(x[1])) + ylab(expression(x[2])) + theme_bw()


D = diag(c(2, -5))
V = matrix(c(2, 1, -1, 2), 2, byrow=FALSE) / sqrt(5)
A = V %*% D %*% t(V)

print(A)
print(eigen(A)$values)

# Plot function 
x1 = seq(-5, 5, by = 0.1)
x2 = x1
X = expand.grid(x1, x2)
y = apply(X, 1, function(x) t(x) %*% A %*% x)

df = data.frame(X, y)

p2 = ggplot(data = df, aes(x = Var1, y = Var2, z = y)) + geom_contour_filled()
p2 = p2 + xlab(expression(x[1])) + ylab(expression(x[2])) + theme_bw()


D = diag(c(2, 0))
V = matrix(c(2, 1, -1, 2), 2, byrow=FALSE) / sqrt(5)
A = V %*% D %*% t(V)

print(A)
print(eigen(A)$values)

# Plot function 
x1 = seq(-5, 5, by = 0.1)
x2 = x1
X = expand.grid(x1, x2)
y = apply(X, 1, function(x) t(x) %*% A %*% x)

df = data.frame(X, y)

p3 = ggplot(data = df, aes(x = Var1, y = Var2, z = y)) + geom_contour_filled()
p3 = p3 + xlab(expression(x[1])) + ylab(expression(x[2])) + theme_bw()


p = ggarrange(p1, p2, p3, nrow=1, common.legend = TRUE, legend="right")

ggsave("figure_man/hessian-eigenvalues.pdf", p, width = 12, height = 4)