# ------------------------------------------------------------------------------
# multicrit

# FIG: Illustrates the hypervolume contributions (HVC) of individual solutions 
#      in a two-objective optimization problem.
# ------------------------------------------------------------------------------

library(ggplot2)

# ------------------------------------------------------------------------------

# ref_p = c(9,9)
dd = data.frame(f1 = c(0, 2, 3, 8, 9), f2 = c(7, 5.5, 4 , 2, 9))
dd$f1_max = dd$f1
dd$f2_max = dd$f2
n = nrow(dd)

oo = order(dd$f1, decreasing = TRUE)
for (j in 2:n)
  dd[oo[j], "f1_max"] = dd[oo[j-1], "f1"]
oo = order(dd$f2, decreasing = TRUE)
for (j in 2:n)
  dd[oo[j], "f2_max"] = dd[oo[j-1], "f2"]


dd$hvc = (dd$f2_max - dd$f2) * (dd$f1_max - dd$f1)
dd$hvc[n] = Inf
j_worst = which.min(dd$hvc)

pl = ggplot(dd, aes(x = f1, y = f2))
pl = pl + geom_point(size = 3)
pl = pl + geom_rect(aes(xmin = f1, xmax = f1_max, ymin = f2, ymax = f2_max), alpha = 0.2)
# pl = pl + geom_point(x = 9, y = 9, size = 3)
pl = pl + xlim(0,10) + ylim(0, 10)
# pl = pl + geom_text(aes(x = 5, y = 7, label = expression(lambda)))
pl = pl + geom_text(data = dd[-n,], aes(x = f1+1, y = f2+0.5, label = paste("HVC =",hvc)), size = 3)
pl = pl +  annotate("text", x = dd$f1[j_worst]-0.5, y = dd$f2[j_worst]-0.5, size = 5, parse = TRUE, label = as.character(expression(tilde(lambda))))

ggsave("../figure_man/hv_contrib.png", pl, width = 3, height = 3)


