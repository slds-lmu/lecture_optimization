# Used in: slides/05-multivariate-second-order/02-nrconv.tex
#
# The two phases of damped Newton-Raphson (Boyd & Vandenberghe 2004, 9.5.3):
# a damped phase in which backtracking returns alpha < 1 and the error decreases
# by roughly a constant factor, followed by a pure phase in which the full step
# alpha = 1 is always accepted and the error is squared at every step.
#
# Objective: the "overshoot" function already used in slideset 1,
#
#   f(x1, x2) = sqrt(1 + x1^2) + x2^2 / 2,   f* = 1 at the origin
#
# chosen deliberately over Boyd's own 2D test function (9.20): on (9.20) the
# full Newton step satisfies the Armijo rule from every reasonable start, so it
# has no damped phase at all. Boyd's figure showing damped steps (fig. 9.22) is
# for the R^100 problem (9.21), not for the 2D one. Along x1 the curvature of
# this objective decays away from the optimum ((1 + x1^2)^(-3/2) -> 0), so the
# undamped step overshoots badly when |x1| > 1 and backtracking has to damp.
#
# The phase boundary is not computed from eta (which would need m, L and the
# Lipschitz constant of the Hessian): it is read off the accepted step sizes,
# following Boyd's own characterisation of the pure phase as the iterations in
# which a step size of 1 is always chosen.
#
#   ../figure/NR_phases.png

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  suppressWarnings(library(vistool))
})

overshoot = function(x) sqrt(1 + x[1]^2) + x[2]^2 / 2

overshoot_grad = function(x) {
  c(x[1] / sqrt(1 + x[1]^2), x[2])
}

overshoot_hess = function(x) {
  matrix(c((1 + x[1]^2)^(-1.5), 0, 0, 1), nrow = 2L, byrow = TRUE)
}

f_opt = 1 # at the origin

x_start = c(3, 2)
steps_nr = 8L

# Boyd's backtracking parameters for the R^2 example (fig. 9.19); gamma is the
# Armijo constant (his alpha), tau the shrink factor (his beta)
armijo_gamma = 0.1
armijo_tau = 0.7

# the objective is finite everywhere, the box only bounds the search domain
objective_box = 1e3

col_damped = "#4d9de0" # damped phase: backtracking returns alpha < 1
col_pure = "#ff6262" # pure phase: full step alpha = 1 always accepted

make_objective = function() {
  objective = Objective$new(
    id = "overshoot",
    label = "sqrt(1 + x1^2) + x2^2/2",
    fun = overshoot,
    xdim = 2L,
    lower = rep(-objective_box, 2L),
    upper = rep(objective_box, 2L),
    minimize = TRUE
  )
  objective$.__enclos_env__$private$p_gradient = overshoot_grad
  objective$.__enclos_env__$private$p_hessian = overshoot_hess
  objective
}

optimizer = OptimizerNR$new(
  objective = make_objective(),
  x_start = x_start,
  step_size = 1,
  gamma = armijo_gamma,
  tau = armijo_tau,
  id = "Newton-Raphson",
  print_trace = FALSE
)
# the built-in Armijo backtracking of OptimizerNR, starting from alpha = 1
optimizer$optimize(steps = steps_nr)

archive = as.data.table(optimizer$archive)
stopifnot(nrow(archive) == steps_nr)

# alpha[t] is the step size that produced x^[t], so iteration 0 has none
progress = data.table(
  iteration = 0:steps_nr,
  fval = c(archive$fval_in[1L], archive$fval_out),
  alpha = c(NA_real_, archive$step_size)
)
progress[, error := fval - f_opt]

# the pure phase starts at the first iterate from which every following step is
# full; searching from the back makes this robust to a damped step appearing
# after an accidental full one early on
is_full = !is.na(progress$alpha) & progress$alpha > 1 - 1e-9
last_damped = if (any(!is_full[-1L])) max(which(!is_full[-1L])) else 0L
first_pure = last_damped + 1L # index into iteration 0:steps_nr, i.e. iterate number

progress[, phase := factor(
  fifelse(iteration >= first_pure, "pure (alpha = 1)", "damped (alpha < 1)"),
  levels = c("damped (alpha < 1)", "pure (alpha = 1)")
)]

cat(sprintf("f* = %.6f, start = (%.1f, %.1f)\n", f_opt, x_start[1], x_start[2]))
print(progress[, .(iteration, alpha, error, phase)])
cat(sprintf("pure phase starts at iterate %d\n", first_pure))

# --- figure -----------------------------------------------------------------
# the last iterates reach f - f* = 0 exactly in double precision, which a log
# axis cannot show: plot only the strictly positive errors
shown = progress[error > 0]

p = ggplot(shown, aes(x = iteration, y = error)) +
  geom_vline(xintercept = first_pure - 0.5, linetype = "dashed", color = "grey55", linewidth = 0.4) +
  geom_line(linewidth = 0.9, color = "grey70") +
  geom_point(aes(color = phase), size = 2.2) +
  scale_color_manual(values = c("damped (alpha < 1)" = col_damped, "pure (alpha = 1)" = col_pure), name = NULL) +
  scale_x_continuous(breaks = shown$iteration) +
  scale_y_log10(
    breaks = 10^seq(-12, 2, by = 2),
    labels = scales::label_math(10^.x)(seq(-12, 2, by = 2))
  ) +
  # the annotations already name the phases, so the legend would only repeat them
  annotate("text", x = (first_pure - 0.5) / 2, y = 1e-8, parse = TRUE,
           label = "atop('damped phase', alpha^'[t]' < 1)", color = col_damped, size = 4.6) +
  annotate("text", x = (first_pure - 0.5 + max(shown$iteration)) / 2, y = 1e-8, parse = TRUE,
           label = "atop('pure phase', alpha^'[t]' == 1)", color = col_pure, size = 4.6) +
  labs(x = "Steps", y = expression(f(x^"[t]") - f(x^"*"))) +
  # the figure is shown at roughly half width on the slide, so the type has to
  # be oversized here to stay legible after scaling
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")

ggsave(filename = "../figure/NR_phases.png", plot = p, width = 5, height = 3, dpi = 300)