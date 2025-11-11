# ------------------------------------------------------------------------------
# optimization problems

# FIG: plot |x| and subgradients
# ------------------------------------------------------------------------------

library(ggplot2)

# DATA -------------------------------------------------------------------------

# f(x) = |x|
f <- function(x) abs(x)

# subgradient function
df <- function(x, c) c * x

xv <- seq(-1.5, 1.5, length.out = 1000)
xgrad <- seq(-1, 1, length.out = 100)
cv <- seq(-0.9, 0.9, length.out = 10)

df_f <- data.frame(x = xv, y = f(xv))

labels <- paste0("g[", 1:10, "]")
df_grad <- data.frame()
for (i in seq_along(cv)) {
  df_grad <- rbind(df_grad, data.frame(x = xgrad, y = df(xgrad, cv[i]), c = as.factor(i), label = labels[i], grad_val = -cv[i]))
}

# PLOT -------------------------------------------------------------------------

plot <- ggplot() +
  # Plot f(x) = |x|
  geom_line(data = df_f, aes(x = x, y = y), color = "black", linewidth = 1.5) +
  annotate("text", x = -1.2, y = 1, label = "f(x) == abs(x)", parse = TRUE, size = 8, color = "black") +
  
  # Plot subgradients with unique colors
  geom_line(data = df_grad, aes(x = x, y = y, color = c), linetype = "dashed", linewidth = 1) +
  
  # Annotate subgradients at (1.05, -c)
  geom_text(data = df_grad[df_grad$x == max(xgrad), ], 
            aes(x = 1.1, y = grad_val, label = label), 
            color = "black", size = 5, parse = TRUE) +
  
  # Labels and themes
  labs(x = "x", y = "", color = "g_i") +
  scale_color_manual(values = rainbow(10)) +  # Ensure unique colors
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    legend.position = "none"  # Hide legend to match Python
  )

plot
ggsave("../figure_man/abs.png", plot, width = 8, height = 6)
