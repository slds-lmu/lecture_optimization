# Used in: slides-multivar-first-order-4-weaknesses-curvature.tex
#
# Sets up the ill-conditioned quadratic used for the manual surface and contour
# screenshots in figure_man/ill-cond_1.png and figure_man/ill-cond_2.png.

set.seed(1L)

library(vistool)

ill_conditioned_objective = Objective$new(
  id = "ill_cond_quad",
  label = "Ill-conditioned quadratic",
  fun = function(x, Q) {
    drop(t(x) %*% Q %*% x)
  },
  xdim = 2L,
  lower = c(-10, -10),
  upper = c(10, 10),
  minimize = TRUE,
  Q = diag(c(1, 36))
)

ill_conditioned_visualizer = as_visualizer(ill_conditioned_objective, type = "surface")

if (interactive()) {
  ill_conditioned_visualizer$plot(
    flatten = FALSE,
    show_title = FALSE,
    show_legend = FALSE
  )
}
# ../figure_man/ill-cond_1.png is a manual screenshot.

if (interactive()) {
  ill_conditioned_visualizer$plot(
    flatten = TRUE,
    show_title = FALSE
  )
}
# ../figure_man/ill-cond_2.png is a manual screenshot.
