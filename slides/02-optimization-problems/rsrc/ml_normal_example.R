# ------------------------------------------------------------------------------
# optimization problems

# FIG: normal density and neg log likelihood for mu.
# ------------------------------------------------------------------------------

library(ggplot2)

# ------------------------------------------------------------------------------

# Artificial data (for plotting)
x <- seq(-1, 6, 0.1)

# Observed data 
xi = rnorm(30, mean=2.5, sd = 1)
n <- length(xi)

y <- dnorm(x, mean=2.5, sd=1)
data <- data.frame(x, y)
p1 <- ggplot(data=data, mapping=aes(x=x, y=y)) + geom_line() +
  ylab(expression(paste('f(x | ',mu,', ',sigma,')'))) +
  ggtitle(expression(paste('Normal density, ',sigma^2,'=1'))) 

p1 = p1 + geom_point(data = data.frame(x = xi, y = 0, Type = "Data"), aes(x = x, y = y, colour = Type))
p1 = p1 + theme_bw()
p1 = p1 + theme(legend.position = c(0.85, 0.85), plot.title = element_text(hjust = 0.5), title = element_blank(), legend.key = element_rect(fill = alpha("white", 0.0)))
p1
ggsave("../figure_man/ml_normal_example_dnorm.pdf", p1, width = 3, height = 3)


# generate data for different mu, sigma^2 is always = 1
mus <- seq(-1, 6, 0.1)
out = lapply(mus, function(mu) {
  (n / 2 * log(2 * pi)) + (1 / 2) * sum((xi - mu)^2)
})
out = unlist(out)

p2 <- ggplot(data=data.frame(x = mus, y = out), mapping=aes(x=x, y=y)) + geom_line() +
  xlab(expression(mu)) + ylab(expression(paste('- l(', mu, ')'))) +
  ggtitle(expression(paste('Min. neg. log. likelihood'))) +
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
p2

ggsave("../figure_man/ml_normal_example_negloglike_nooptim.pdf", p2, width = 3, height = 3)

p2 = p2 + geom_vline(mapping=aes(xintercept=mean(xi)), colour = "#f8766d") 
p2
ggsave("../figure_man/ml_normal_example_negloglike.pdf", p2, width = 3, height = 3)

