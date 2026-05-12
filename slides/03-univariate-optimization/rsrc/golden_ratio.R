# ------------------------------------------------------------------------------
# univariate optimization

# FIG: plot golden ratio method process
# ------------------------------------------------------------------------------

library(ggplot2)
library(latex2exp)
library(grid)
library(gridExtra)

source("helper.R")

# ------------------------------------------------------------------------------

gr = (1 + sqrt(5)) / 2 # golden ratio

goldenratio = function(a, b, fun, max.iter = 10) {

  iter = 0

  xl = a
  xr = b

  xb = xr - (xr - xl) / gr
  xn = xl + (xr - xl) / gr

  x = c(xl, xn, xb, xr)
  colors = c("blue", "red", "green", "blue")
  ltypes = c(1, 2, 2, 1)
  y = rep(-10, 4)
  d = data.frame(x = x, colors = colors, ltypes = ltypes, y = y)
  rownames(d) = c("xleft", "xnew", "xbest", "xright")

  while (iter < max.iter) {

    fnew = fun(d["xnew", "x"])
    fbest = fun(d["xbest", "x"])

    p1 = plot.gr(d)
    p1 = p1 + ggtitle(TeX("Define x_{new}"))

    if (fnew < fbest) {
      z = d["xbest", "x"]
      d["xbest", "x"] = d["xnew", "x"]
      d["xnew", "x"] = z

      p2 = plot.gr(d)
      p2 = p2 + ggtitle(TeX("Switch x_{new} and x_{best}"))

    } else {
      p2 = p1 + ggtitle(TeX("Do not switch x_{new} and x_{best}"))
    }

    if (d["xbest", "x"] < d["xnew", "x"]) {

      d["xright", "x"] = d["xnew", "x"]
      d["xright", "colors"] = "blue"

      p3 = plot.gr(d)
      p3 = p3 + geom_vline(xintercept = d["xright", "x"], color = "red", na.rm = T)
      p3 = p3 + ggtitle(TeX("x_{new} is the new right boundary"))
    } else {
      d["xleft", "x"] = d["xnew", "x"]
      d["xleft", "colors"] = "blue"
      d = d[- which(rownames(d) == "xnew"), ]

      p3 = plot.gr(d)
      p3 = p3 + geom_vline(xintercept = d["xleft", "x"], colour = "red", na.rm = T)
      p3 = p3 + ggtitle(TeX("x_{new} is the new left boundary"))
    }

    g = grid_arrange_shared_legend(p1, p2, p3, ncol = 3)
    ggsave(paste("../figure_man/golden_ratio", iter, ".pdf", sep = ""), g, width = 9, height = 4)


    d["xbest", "x"] = d["xright", "x"] - (d["xright", "x"] - d["xleft", "x"]) / gr
    d["xnew", "x"] = d["xleft", "x"] + (d["xright", "x"] - d["xleft", "x"]) / gr
    d["xnew", c("colors", "ltypes", "y")] = c("red", 2, -10)
    d$ltypes = as.numeric(d$ltypes)

    iter = iter + 1
  }
}



fun = function(x) 1/4*(x-2)^3-2*x
a = 3/2
b = 5
#cos(x) - 2 * x^3 + 3 * exp(x)

#goldenratio(a, b, fun, max.iter = 1)
goldenratio(a, b, fun, max.iter = 4)

