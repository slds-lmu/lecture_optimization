library(ggplot2)

x <- seq(-1, 6, 0.1)
n <- length(x)

y <- dnorm(x, mean=2.5, sd=1)
data <- data.frame(x, y)
p1 <- ggplot(data=data, mapping=aes(x=x, y=y)) + geom_line() +
  ylab(expression(paste('f(x | ',mu,', ',sigma,')'))) +
  ggtitle(expression(paste('Normal distribution with ',mu,'=2.5, ',sigma^2,'=1'))) +
  theme(plot.title = element_text(hjust = 0.5))
p1
ggsave("../figure_man/ml_normal_example_dnorm.pdf", p1, width = 3, height = 2)


# generate data for different mu, sigma^2 is always = 1
mus <- seq(-1, 6, 0.1)
df_mu <- data.frame(matrix(nrow=n))
for (mu in mus) {
  df_mu[, ncol(df_mu)+1] <- dnorm(x, mean=mu, sd=1)                
  names(df_mu)[ncol(df_mu)] <- paste0("mu=", mu) 
}
df_mu <- df_mu[-1]
View(df_mu)


# calculate the negative log likelihood for the generated data
df_plot <- data.frame((matrix(data=mus)))
for (y in 1:ncol(df_mu)){
    df_plot[y, 2] <- (n*log(2*pi*1))/2 + (1/2) + sum((df_mu[, y]-2.5)^2)
}
names(df_plot) <- c('mu', 'l')
View(df_plot)

p2 <- ggplot(data=df_plot, mapping=aes(x=mu, y=l)) + geom_line() +
  geom_point(mapping=aes(x=2.5, y=min(df_plot$l)), colour = "blue") +
  xlab(expression(mu)) + ylab('-loglike') +
  ggtitle(expression(paste('Minimal -loglike at ', mu, '=2.5 for ', sigma^2,'=1'))) +
  theme(plot.title = element_text(hjust = 0.5))
p2
ggsave("../figure_man/ml_normal_example_negloglike.pdf", p2, width = 3, height = 2)

