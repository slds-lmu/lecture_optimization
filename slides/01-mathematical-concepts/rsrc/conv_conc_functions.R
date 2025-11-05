# ------------------------------------------------------------------------------
# mathematical concepts

# FIG: plot functions
#   (1) convex |x|
#   (2) concave log(x)
#   (3) not convex nor concave exp(-x^2)
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

# DATA -------------------------------------------------------------------------

x <- seq(-15, 15, length.out = 1000)

# abs
abs_df <- data.frame(x = x, y = abs(x), func='abs')
abs_points <- data.frame(x = c(-1, 1.5), y = c(1, 1.5))
abs_line <- data.frame(x1 = -1, y1 = 1, x2 = 1.5, y2 = 1.5)

# log
x_log <- x[x > 0]  # log is defined for x > 0
log_df <- data.frame(x = x_log, y = log(x_log), func='log')
log_points <- data.frame(x = c(0.5, 2), y = log(c(0.5, 2)))
log_line <- data.frame(x1 = 0.5, y1 = log(0.5), x2 = 2, y2 = log(2))

# exp(-x^2)
exp_df <- data.frame(x = x, y = exp(-x^2), func='exp(-x^2)')
exp_points <- data.frame(x = c(-2.2, 0.5), y = exp(-c(-2.2, 0.5)^2))
exp_line <- data.frame(x1 = -2.2, y1 = exp(-(-2.2)^2), x2 = 0.5, y2 = exp(-(0.5)^2))

plot_data <- bind_rows(abs_df, log_df, exp_df)
func_colors <- c("abs" = "blue",
                 "log" = "green",
                 "exp(-x^2)" = "red")
# PLOT -------------------------------------------------------------------------

plot <- ggplot() +
  geom_line(data = plot_data, aes(x = x, y = y, color = func), show.legend = TRUE) +
  scale_color_manual(values = func_colors) +
  # abs
  geom_point(data = abs_points, aes(x = x, y = y), color = "blue") +
  geom_segment(data = abs_line, aes(x = x1, y = y1, xend = x2, yend = y2), color = "blue", linetype = "dashed", show.legend = FALSE) +
  
  #log
  geom_point(data = log_points, aes(x = x, y = y), color = "green") +
  geom_segment(data = log_line, aes(x = x1, y = y1, xend = x2, yend = y2), color = "green", linetype = "dashed", show.legend = FALSE) +
  
  # exp(-x^2)
  geom_point(data = exp_points, aes(x = x, y = y), color = "red") +
  geom_segment(data = exp_line, aes(x = x1, y = y1, xend = x2, yend = y2), color = "red", linetype = "dashed", show.legend = FALSE) +
  
  labs(x = "x", y = "y") +
  xlim(-3, 3) + ylim(-1, 2) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, "cm"),
    legend.position = c(0.1, 0.1),
    legend.title = element_blank()
  )


plot
ggsave("../figure_man/conv_conc_functions.png", plot, width = 10, height = 8)
