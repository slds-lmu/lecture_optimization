# ------------------------------------------------------------------------------
# constrained

# FIG: plot lagrange contour lines
# ------------------------------------------------------------------------------

# Lagrange contour lines
lagrangeContour = function(col = terrain_hcl, theta = 40, phi = 40, xlab = "x1", ylab = "x2", constraint = F, add.path = F) {
  if ( is.function(col)) col = col(nrow(z) * ncol(z))
    require("colorspace")
    image(x, y, z, col = col, xlab = xlab, ylab = ylab)
    contour(x, y, z, add = TRUE, levels = c(seq(- 2, 1, by = 0.5)))
    if (constraint == T) {
      n = length(h.y)
      for (j in seq_along(h.y)) {
        if (j > 1) {
          lines(c(h.x1[j], h.x1[j-1]), c(h.x2[j], h.x2[j-1]), col = "red")
        } else {
          lines(c(h.x1[j], h.x1[n]), c(h.x2[j], h.x2[n]), col = "red")
        }
      }
    }
    if (add.path == T) {
      lines(x = path$x, y = path$y, col = "darkred")
      points(x = path$x, y = path$y, pch = 1, col = "darkred")
      points(x = p1[1], y = p1[2], pch = 18, cex = 1.5, col = "red")
      points(x = p2[1], y = p2[2], pch = 18, cex = 1.5, col = "red")
      text(x = p2[1] + 0.2, y = p2[2], labels = "Optimum", col = "red")
      text(x = p1[1] + 0.3, y = p1[2] + 0.1, labels = "feasible, not optimal", col = "red")
    }
  invisible(NULL)
}
