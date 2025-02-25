# ------------------------------------------------------------------------------
# univariate optimization

# FUNC: 
#   grid_arrange_shared_legend - Combines multiple ggplot2 plots 
#                                into a single figure.
#   plot.gr - Creates a custom plot that visualizes 
#             a function with vertical lines at key points.
# ------------------------------------------------------------------------------

grid_arrange_shared_legend = function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots = list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] +
  theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
  theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                     legend,ncol = 1,
          heights = unit.c(unit(1, "npc") - lheight, lheight)),
          "right" = arrangeGrob(do.call(arrangeGrob, gl),
          legend, ncol = 2,
          widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  grid.newpage()
  grid.draw(combined)

  # return gtable invisibly
  invisible(combined)
}

plot.gr = function(d) {
  p = ggplot(data = d) + stat_function(fun = fun) + xlim(c(d["xleft", "x"] - 1, d["xright", "x"] + 1)) + geom_vline(data = d[c("xbest", "xnew"), ], aes(xintercept = x, color = rownames(d[c("xbest", "xnew"), ]))) + theme(legend.position = "top") + geom_rect(aes(xmin = d["xleft", "x"], xmax = d["xright", "x"], ymin = - Inf, ymax = Inf), fill = d["xleft", "colors"], alpha = 0.05) + theme_bw() + scale_color_manual(values = c(xbest = "green", xnew = "red"), name = "") + theme(legend.position = "bottom")
  return(p)
}