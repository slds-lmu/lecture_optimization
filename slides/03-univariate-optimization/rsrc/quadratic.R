# Optimization, WiSe 22/23
# Chapter 3.1 Univariate - brent
# Plots for: Quadratic interpolation


library(ggplot2)
library(latex2exp)
library(grid)
library(gridExtra)


source("./helper.R")

interpolate = function(fun, xl, xr, xb) {
  fl = fun(xl)
  fr = fun(xr)
  fb = fun(xb)

  denom = (xl - xb) * (xl - xr) * (xb - xr)
  A = (xr * (fb - fl) + xb * (fl - fr) + xl * (fr - fb)) / denom
  B = (xr^2 * (fl - fb) + xb^2 * (fr - fl) + xl^2 * (fb - fr)) / denom
  C = (xb * xr * (xb - xr) * fl + xr * xl * (xr - xl) * fb + xl * xb * (xl - xb) * fr) / denom
  xnew = - B / (2 * A)

  return(list(A = A, B = B, C = C, xnew = xnew))
}

gr = (1 + sqrt(5)) / 2 # golden ratio

quadratic.interpolation = function(a, b, fun, max.iter = 10) {

  iter = 0

  xl = a
  xr = b

  xb = 3

  int = interpolate(fun = fun, xl = xl, xr = xr, xb = xb)

  x = c(xl, int$xnew, xb, xr)
  colors = c("blue", "red", "green", "blue")
  ltypes = c(1, 2, 2, 1)
  y = rep(-10, 4)
  d = data.frame(x = x, colors = colors, ltypes = ltypes, y = y)
  rownames(d) = c("xleft", "xnew", "xbest", "xright")

  while (iter < max.iter) {

    fnew = fun(d["xnew", "x"])
    fbest = fun(d["xbest", "x"])

    p1 = plot.gr(d)
    p1 = p1 + stat_function(fun = function(x) int$A * x^2 + int$B * x + int$C, lty = 2)
    p1 = p1 + geom_point(aes(x=d["xnew", "x"], y=fbest), color="blue", size=2.5)
    p1 = p1 + geom_point(aes(x=d["xbest", "x"], y=fnew), color="blue", size=2.5)
    p1 = p1 + ylab("y") + xlab("")
    p1 = p1 + ggtitle(expression(paste("Define ", x[new], " via parabola")))

    if (fnew < fbest) {
      store = d["xbest", "x"]
      d["xbest", "x"] = d["xnew", "x"]
      d["xnew", "x"] = store

      p2 = plot.gr(d)
      p2 = p2 + geom_point(aes(x=d["xnew", "x"], y=fbest), color="blue", size=2.5)
      p2 = p2 + geom_point(aes(x=d["xbest", "x"], y=fnew), color="blue", size=2.5)
      p2 = p2 + ylab("y") + xlab("")
      p2 = p2 + ggtitle(expression(paste("Switch ", x[new], " and " ,x[best])))

    } else {
      p2 = plot.gr(d)
      p2 = p2 + geom_point(aes(x=d["xbest", "x"], y=fbest), color="blue", size=2.5)
      p2 = p2 + ylab("y") + xlab("")
      p2 = p2 + ggtitle(expression(paste("Do not switch ", x[new], " and ", x[best])))
    }

    if (d["xbest", "x"] < d["xnew", "x"]) {

      d["xright", "x"] = d["xnew", "x"]
      d["xright", "colors"] = "blue"

      p3 = plot.gr(d)
      p3 = p3 + geom_vline(xintercept = d["xright", "x"], color = "blue", na.rm = T)
      p3 = p3 + geom_point(mapping=aes(x=d["xright", "x"], y=fbest), color="blue", size=2.5)
      p3 = p3 + ylab("y") + xlab("")
      p3 = p3 + ggtitle(expression(paste(x[new], " is the new right boundary")))
    } else {
      d["xleft", "x"] = d["xnew", "x"]
      d["xleft", "colors"] = "blue"
      d = d[- which(rownames(d) == "xnew"), ]

      p3 = plot.gr(d)
      p3 = p3 + geom_vline(xintercept = d["xleft", "x"], colour = "red", na.rm = T)
      p3 = p3 + geom_point(mapping=aes(x=d["xleft", "x"], y=fbest), color="blue", size=2.5)
      p3 = p3 + ylab("y") + xlab("")
      p3 = p3 + ggtitle(expression(paste(x[new], " is the new left boundary")))
    }

    g = grid_arrange_shared_legend(p1, p2, p3, ncol = 3)
    
    ggsave(paste("figure_man/quadratic", iter, ".pdf", sep = ""), g, width = 9, height = 4)

    int = interpolate(fun = fun, xl = d["xleft", "x"], xr = d["xright", "x"], xb = d["xbest", "x"])

    d["xnew", "x"] = int$xnew

    d["xnew", c("colors", "ltypes", "y")] = c("red", 2, -10)
    d$ltypes = as.numeric(d$ltypes)

    iter = iter + 1
  }
}


fun = function(x) 1/4*(x-3/4)^3-2*x

a = 0.5
b = 5

quadratic.interpolation(a, b, fun, max.iter = 5)

