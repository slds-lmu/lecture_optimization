




int = seq(0,4, length.out = 500)
f = function(x) x^1.1 * sin(x-2) + 1 - 2.5*sin(3*x-1.5)
plot(int, f(int), type="l", xlab = "", ylab = "f(x)", xaxt="n")
x = 2.5
rad = 0.5
lines(c(x, x), c(-3, f(x)), lty = 2)
area = seq(x - rad, x+rad, length.out=200)
col_vec = c("red", "green")
points(area, rep(f(x), 200), col = col_vec[(f(area)<f(x))+1], pch=15, cex=0.5)

x = 2.8
rad = 0.5
lines(c(x, x), c(-3, f(x)), lty = 2)
area = seq(x - rad, x+rad, length.out=200)
col_vec = c("red", "green")
points(area, rep(f(x), 200), col = col_vec[(f(area)<f(x))+1], pch=15, cex=0.5)


lines(c(3.05,3.05), c(-3, f(3.05)), lty=2)
axis(1, c(2.5, 2.8, 3.05), labels = c(expression(paste("x"^"[0]")), expression(paste("x"^"[1]")), expression(paste("x"^"[2]"))),tick=F)
text(x = c(2.5, 2.8), y= c(f(2.5) + 0.5, f(2.8) + 0.5), labels = c(expression(paste("V(x"^"[0]",")")), expression(paste("V(x"^"[1]",")"))))


################

PDelta = function(Delta_f, Temp) {
  ret = exp(- Delta_f / (Temp))
  ret[ret>1] = 1
  ret
}


layout(matrix(1:2, nrow = 1), width = c(5,1.5), height = rep(1,2))


int = seq(0,4, length.out = 500)
f = function(x) x^1.1 * sin(x-2) + 1 - 2.5*sin(3*x-1.5)
plot(int, f(int), type="l", xlab = "", ylab = "f(x)", xaxt="n")
x = 2.5
rad = 0.5
lines(c(x, x), c(-3, f(x)), lty = 2)
area = seq(x - rad, x+rad, length.out=200)
col_vec = c("yellow", "green")

rbPal <- colorRampPalette(c('red','orange', "yellow", "green"))
col_smooth <- rbPal(50)[as.numeric(cut(seq(0,1,0.02),breaks = 50))]

points(area, rep(f(x), 200),
       col = col_smooth[findInterval(PDelta(f(area)-f(x), Temp=3), seq(0,1,0.02))],
       pch=15, cex=0.5)

x = 2.75
rad = 0.5
lines(c(x, x), c(-3, f(x)), lty = 2)
area = seq(x - rad, x+rad, length.out=200)
col_vec = c("orange", "green")
points(area, rep(f(x), 200),
       col = col_smooth[findInterval(PDelta(f(area)-f(x), Temp=1.5), seq(0,1,0.02))],
       pch=15, cex=0.5)
x = 2.3
rad = 0.5
lines(c(x, x), c(-3, f(x)), lty = 2)
area = seq(x - rad, x+rad, length.out=200)
points(area, rep(f(x), 200),
       col = col_smooth[findInterval(PDelta(f(area)-f(x), Temp=0.15), seq(0,1,0.02))],
       pch=15, cex=0.5)

x = 1.81
rad = 0.5
lines(c(x, x), c(-3, f(x)), lty = 2)
area = seq(x - rad, x+rad, length.out=200)
points(area, rep(f(x), 200),
       col = col_smooth[findInterval(PDelta(f(area)-f(x), Temp=0.15), seq(0,1,0.02))],
       pch=15, cex=0.5)

axis(1, c(2.5, 2.75, 2.3, 1.83), labels = c(expression(paste("x"^"[0]")), expression(paste("x"^"[1]")), expression(paste("x"^"[2]")), expression(paste("x"^"[3]"))),tick=F)

x.all = c(2.5, 2.75, 2.3, 1.83)
text(x = x.all, y= c(f(x.all) + 0.4), labels = c(expression(paste("V(x"^"[0]",")")), expression(paste("V(x"^"[1]",")")),
                                                 expression(paste("V(x"^"[2]",")")), expression(paste("V(x"^"[3]",")"))),
     cex = 0.9)



#legend
legend_image <- as.raster(matrix(col_smooth, ncol=1))
plot(c(0,1),c(0,1), type = 'n', axes = F, xlab = '', ylab = '', main = "")
rasterImage(legend_image, 0, 0, 1, 1)
axis(2, at = seq(0, 1, l = 6), labels = seq(1, 0, l = 6), tick = T, las=1,
     lwd = 0, lwd.ticks = 1)

##################

library("RColorBrewer")
Delta = seq(-5, 50, length.out= 500)

colvec = brewer.pal(5, "Accent")
plot(Delta, PDelta(Delta, 250), type = "l", ylim = c(-0.1,1.3), xlab = expression(Delta[f]), ylab = "P(Acceptance)", col = colvec[1], lwd = 2, yaxt="n")
axis(2, seq(0,1, by=.2))
abline(h=c(0,1), lty = 2)
lines(c(0,0), c(0,1), lty=2)
points(Delta, PDelta(Delta, 125), type = "l", col = colvec[2], lwd = 2)
points(Delta, PDelta(Delta, 50), type = "l", lwd = 2, col = colvec[3])
points(Delta, PDelta(Delta, 10), type = "l", lwd = 2,col = colvec[4])
points(Delta, PDelta(Delta, 2), type = "l", lwd = 2, col = colvec[5])
legend("top", paste("T =", c(250,125,50,10,2)), fill=colvec, ncol=5)


#################

library(ggplot2)
library(mlr)
multimodFun = function(x) {
  x^4 + x^3 - 2 * x^2
  
}

x = seq(-2.5, 2.5, 0.01)
y = multimodFun(x)
multimod.dat = data.frame(x, y)
estimates = sapply(x, function(x) {
  res = optim(x, fn = multimodFun, method = "Nelder-Mead")
  c(res$par, res$value)
})
estimates = as.data.frame(t(estimates))
multimod.dat[, c("par", "value")] = estimates
cols = c("blue", "red")
multimod.dat$col = as.factor(c(rep(cols[1L], times = 250L),
                               rep(cols[2L], times = 251L)))
multimod.dat$sz = rep(.1, times = nrow(multimod.dat))

ggplot(multimod.dat,aes(x = x, y = y, color = col)) + geom_line() +
  geom_point(aes(x, value, size = sz), color = "black") +
  scale_y_continuous(limits = c(-3, 3)) +
  scale_x_continuous(limits = c(-2.5, 2),
                     breaks = c(-2, -1.44, 0, 0.69, 2),
                     labels = c("-2", expression(x[1]), "0", expression(x[2]), "2")) +
  scale_size_continuous(range = c(0.1, 0.2), name = "",
                        label = "optimum found by Nelder-Mead") +
  scale_color_manual(name="basin of attraction paths",
                     values = cols,
                     labels = c(expression(x[1]), expression(x[2]))) +
  guides(size = guide_legend(override.aes = list(size = 3))) + theme_bw()

#################





