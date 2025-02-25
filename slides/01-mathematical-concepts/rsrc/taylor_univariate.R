# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot taylor polynomial of 0-4 orders for sin(x).
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# DATA -------------------------------------------------------------------------

x <- seq(-15, 15, length.out = 1000)
a <- 2  # Expansion point

# Taylor series approximations
y0 <- rep(sin(a), length(x))
y1 <- y0 + cos(a) * (x - a) / factorial(1)
y2 <- y1 - sin(a) * (x - a)^2 / factorial(2)
y3 <- y2 - cos(a) * (x - a)^3 / factorial(3)
y4 <- y3 + sin(a) * (x - a)^4 / factorial(4)

df <- data.frame(
  x = rep(x, 6),
  y = c(sin(x), y0, y1, y2, y3, y4),
  order = factor(rep(c("sin(x)", "m=0", "m=1", "m=2", "m=3", "m=4"), each = length(x)),
                 levels = c("sin(x)", "m=0", "m=1", "m=2", "m=3", "m=4"))
)

# PLOT -------------------------------------------------------------------------

colors <- c("black", "red", "blue", "orange", "green", "cyan")

plot <- ggplot(df, aes(x = x, y = y, color = order)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  labs(
    title = "Taylor polynomial for various orders at a=2",
    x = "x",
    y = "y",
    color = "Order"
  ) +
  xlim(-6, 10) + ylim(-8, 8) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  )

plot
ggsave("../figure_man/taylor_univariate.png", plot, width = 10, height = 8)
