# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: 2D contour and 3D plots of branin function
# ------------------------------------------------------------------------------

library(DiceKriging)
library(plotly)
library(numDeriv)
library(rootSolve)
library(rgl)
library(plotly)
library(reshape2)

# ------------------------------------------------------------------------------

braninm <- function(x, y) { 
	a = 1
	b = 5.1 / (4 * pi^2) 
	c = 5 / pi
	r = 6
	s = 10
	t = 1 / (8 * pi)
	a * (y - b * x^2 + c * x - r)^2 + s * (1 - t) * cos(x) + s
}

branin <- function(x) { 
  a = 1
  b = 5.1 / (4 * pi^2) 
  c = 5 / pi
  r = 6
  s = 10
  t = 1 / (8 * pi)
  a * (x[2] - b * x[1]^2 + c * x[1] - r)^2 + s * (1 - t) * cos(x[1]) + s
}

# 2D Plot 

x1 = seq(-5, 10, by = 0.1)
x2 = seq(0, 15, by = 0.1)
df = expand.grid(x1, x2)
df$y = apply(df, 1, branin)

xx <- c(-pi, pi, 9.42478)
yy <- c(12.275, 2.275, 2.475)

p = ggplot(data = df, aes(x = Var1, y = Var2, z = y)) + geom_contour_filled()
p = p + xlab(expression(x[1])) + ylab(expression(x[2])) + theme_bw()
p = p + geom_point(data = data.frame(x = xx, y = yy), aes(x = x, y = y), color = "#F8766D")
p
ggsave("../figure_man/branin3d/branin2D.pdf", p, width = 6, height = 4)


# Full 3D Plot

x1 = seq(-5, 10, by = 0.5)
x2 = seq(0, 15, by = 0.5)
z = outer(x1, x2, braninm)

nrz <- nrow(z)
ncz <- ncol(z)
jet.colors <- colorRampPalette( c("blue", "green", "yellow", "orange", "red") )
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)

# 3D PLOT 
pdf("../figure_man/branin3d/branin3D.pdf",5,7.5, colormodel = "cmyk")
par(xaxs = "i", yaxs = "i")
p = persp(x1, x2, z, col = color[facetcol], theta = 15, phi = 20)
xx <- c(-pi, pi, 9.42478)
yy <- c(12.275, 2.275, 2.475)
zz <- c(0.39, 0.39, 0.39)
mypoints <- trans3d(xx,yy,zz,pmat = p)
points(mypoints,pch = 16,col = 2)

dev.off()


# Zoom in optimum one

for (i in 1:3) {

	x1 = seq(xx[i] - 1, xx[i] + 1, by = 0.1)
	x2 = seq(yy[i] - 1, yy[i] + 1, by = 0.1)
	z = outer(x1, x2, braninm)

	nrz <- nrow(z)
	ncz <- ncol(z)
	jet.colors <- colorRampPalette( c("blue", "green", "yellow", "orange", "red") )
	nbcol <- 100
	color <- jet.colors(nbcol)
	# Compute the z-value at the facet centres
	zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
	# Recode facet z-values into color indices
	facetcol <- cut(zfacet, nbcol)

	# 3D PLOT 
	pdf(paste0("../figure_man/branin3d/branin3D-optim-", i, ".pdf"),5,7.5, colormodel = "cmyk")
	par(xaxs = "i", yaxs = "i")
	p = persp(x1, x2, z, col = color[facetcol], theta = 15, phi = 20)
	mypoints <- trans3d(xx[i],yy[i],zz[i],pmat = p)
	points(mypoints,pch = 16,col = 2)

	dev.off()
}


modbranin <- function(xx, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi)) {
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- a * (x2 - b*x1^2 + c*x1 - r)^2
  term2 <- s*(1-t)*cos(x1)
  
  y <- term1 + term2 + s + 5*x1
  return(y)
}

# 3D Plot of Branin function 
xmin.grid <- seq(-5,10,by = 0.1)
ymin.grid <- seq(0,15,by = 0.1)
df <- expand.grid(xmin.grid, ymin.grid)
df$y <- apply(df, 1, branin)

dfp = acast(df, Var2 ~ Var1, value.var = "y") 

plot_ly(z = ~ as.matrix(dfp)) %>% add_surface()

# 2D Plot of Branin function 


persp(xmin.grid, ymin.grid, as.matrix(dfp))

zmin.grid <- matrix(responsemin.grid, n.grid, n.grid)

min <- c(0.5427730, 0.15)

min_gradient <- -gradient(branin, min)
sprintf(min_gradient[1], fmt='%#.3f')
sprintf(min_gradient[2], fmt='%#.3f')

plot.new()
contour(xmin.grid,ymin.grid,zmin.grid,40)
points(rbind(t(min)), pch=19, col="red")

opend3d()
z1 <- modbranin(c(min))
plot3d(xmin.grid, ymin.grid, z1, col = rainbow(1000))

plot_ly(z=modbranin(c(min)), type="surface")

#####
saddle <- c(0.333, 0.4)
saddle_gradient <- -gradient(branin, saddle)
sprintf(saddle_gradient[1], fmt='%#.3f')
sprintf(saddle_gradient[2], fmt='%#.3f')


n.grid <- 20
xsaddle.grid <- seq(0.3,0.4,length=n.grid)
ysaddle.grid <- seq(0.35,0.45,length=n.grid)
designsaddle.grid <- expand.grid(xsaddle.grid, ysaddle.grid)
responsesaddle.grid <- apply(designsaddle.grid, 1, branin)
zsaddle.grid <- matrix(responsesaddle.grid, n.grid, n.grid)

plot.new()
contour(xsaddle.grid,ysaddle.grid,zsaddle.grid,40)
points(rbind(t(saddle)), pch=19, col="red")

hessian = function(x, y) {
	v1 = -0.516738
	v2 = -0.129185
	v3 = 0.258369
	matrix(c(v1 * (v2 * x^2 + 5 * x / pi + y - 6) + 2 * (5 / pi - v3 * x)^2 - 10 * (1 - 1 / (8 * pi) * cos(x)),
		2 * (5 / pi - v3 * x), 2 * (5 / pi - v3 * x), 2 
		), ncol = 2)
}

H1 = hessian(xx[1], yy[1])
eigen(H1)$values

H2 = hessian(xx[2], yy[2])
eigen(H2)$values

H3 = hessian(xx[3], yy[3])
eigen(H3)$values

