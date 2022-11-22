library(ggplot2)

source("rsrc/functions.R")

foo = function(x, y) {
  x^2 - y^2
}

x = y = seq(-1.5, 1.5, length = 50)
z = outer(x, y, foo)
p = c(list(list(1, 0)), optim0(1, 0, FUN = foo, maximum = FALSE, alpha = 0.2))

pdf(file = "figure_man/gradient_descent_branin.pdf",   # The directory you want to save the file in
    width = 5.5, # The width of the plot in inches
    height = 5) # The height of the plot in inches

sd_plot()

# Also get gradient norm 
gradnorm = function(x, y) sqrt((2 * x)^2 + (2 * y)^2)

gnorm = data.frame(grad_norm = unlist(lapply(p, function(pp) {
  gradnorm(pp[[1]], pp[[2]])
})))
gnorm$iter = 1:nrow(gnorm)

p = ggplot(data = gnorm, aes(x = iter, y = grad_norm)) + geom_line() + theme_bw()
p

ggsave(filename = "figure_man/saddle_point_grad_norm.pdf", p, width = 9, height = 2.5)

