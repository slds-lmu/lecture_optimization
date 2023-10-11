library(plotly)

W <- cbind(c(1, 1), c(1, -1)) / sqrt(2)

create_fig <- function(lam_min) {
  L <- diag(c(1, lam_min))
  A <- W %*% L %*% t(W)

  f <- function(x, y) {
    t(c(x, y)) %*% A %*% c(x, y)
  }

  x <- seq(-1, 1, 0.01)
  y <- seq(-1, 1, 0.01)
  z <- outer(x, y, FUN = function(x, y) {
    xy <- mapply(c, x, y)
    apply(xy, 2, function(v) {
      f(v[1], v[2])
    })
  })

  plot_ly(x = x, y = y, z = z, type = "contour", showscale = FALSE) %>%
    layout(
      xaxis = list(
        showticklabels = FALSE,
        ticks = ""
      ),
      yaxis = list(
        showticklabels = FALSE,
        ticks = ""
      )
    )
}

fig1 <- create_fig(lam_min = 1)
fig2 <- create_fig(lam_min = 0.5)
fig3 <- create_fig(lam_min = 0.01)

fig <- subplot(fig1, fig2, fig3) %>%
  layout(
    width = 1200,
    height = 400
  )
