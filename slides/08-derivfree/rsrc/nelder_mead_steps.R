# Used in: slides/08-derivfree/02-neldermead.tex
#
# Schematic frames of one and a half Nelder-Mead iterations on the quadratic
# bowl f(x) = x1^2 + x2^2 (optimum at the origin, grey circles are level sets).
# Replaces the hand-made figure_man/Nelder0*.png series and adds the two steps
# it was missing, contraction and shrinking. All frames share the same axis
# limits so they can be overlaid on consecutive slides without jumping.
#
# Iteration 1 (reflection lands inside the best level set, case 2):
#   1  initial simplex, vertices ordered by f
#   2  centroid of the d best vertices
#   3  reflection point v_r
#   4  new simplex: v_r replaces the worst vertex and becomes v_1 (on the
#      slides this illustrates case 1, although the run is in case 2)
#   5  expansion point v_e (worse than v_r here, so v_r is kept)
# Iteration 2 (reflection overshoots, case 3):
#   6  reflection point v_r is worse than v_d
#   7  contraction point v_c between centroid and worst vertex
#   8  new simplex: v_c replaces the worst vertex and becomes v_1
#   9  shrinking of the whole simplex towards v_1. On this bowl v_c is accepted,
#      so no shrink happens; the frame shows the fallback hypothetically, the
#      slide caption says so
#
#   ../figure/nelder_mead_steps_<k>.png

library(data.table)
library(ggplot2)

set.seed(1L)

f = function(v) v[1L]^2 + v[2L]^2

# Nelder-Mead coefficients (defaults used on the slides)
rho = 1 # reflection
chi = 2 # expansion
gam = 0.5 # contraction
sig = 0.5 # shrinking

# plot window and level-set radii, shared by all frames
x_lim = c(-3.8, 4.6)
y_lim = c(-2.6, 4.7) # top closes the outermost circle
radii = c(1.5, 3, 4.5)

# the figures are shown at roughly half the slide width, so type and markers
# are oversized here to stay legible after scaling
label_size = 8
point_size = 4
# label offsets: just clear of the point marker (text is about 0.4 units high)
above = 0.42
below = -0.46

# --- geometry helpers --------------------------------------------------------

# rows are vertices; returns them sorted by f, i.e. v_1 best, v_(d+1) worst
order_simplex = function(v) v[order(apply(v, 1L, f)), , drop = FALSE]

centroid = function(v) colMeans(v[-nrow(v), , drop = FALSE])

# a labelled point; dx, dy place the plotmath label relative to it
pt = function(v, label, dx = 0, dy = above) {
  data.table(x = v[1L], y = v[2L], label = label, dx = dx, dy = dy)
}

vertex_points = function(v, dy) {
  rbindlist(lapply(seq_len(nrow(v)), function(i) pt(v[i, ], sprintf("v[%d]", i), dy = dy[i])))
}

polygon_points = function(v) data.table(x = v[, 1L], y = v[, 2L])

# --- drawing -----------------------------------------------------------------

circle = function(r, n = 200L) {
  t = seq(0, 2 * pi, length.out = n)
  data.table(x = r * cos(t), y = r * sin(t), r = r)
}
level_sets = rbindlist(lapply(radii, circle))

seg = function(a, b, linetype = "dashed") {
  data.table(x = a[1L], y = a[2L], xend = b[1L], yend = b[2L], linetype = linetype)
}

# points: labelled points (see pt()); simplex: vertex matrix drawn as closed
# polygon; ghost: previous simplex as dotted outline; segments: from seg()
draw_frame = function(points, simplex = NULL, segments = NULL, ghost = NULL, filename) {
  p = ggplot() +
    geom_path(
      data = level_sets, aes(x = x, y = y, group = r),
      color = "grey75", linewidth = 0.5
    ) +
    annotate("point", x = 0, y = 0, shape = 21, size = 3.8, stroke = 1, fill = "white") +
    annotate("text", x = -0.38, y = -0.28, label = "x^'*'", parse = TRUE, size = label_size)


  if (!is.null(ghost)) {
    p = p + geom_polygon(
      data = polygon_points(ghost), aes(x = x, y = y),
      fill = NA, color = "grey60", linewidth = 0.8, linetype = "dotted"
    )
  }
  if (!is.null(simplex)) {
    p = p + geom_polygon(
      data = polygon_points(simplex), aes(x = x, y = y),
      fill = NA, color = "black", linewidth = 0.8
    )
  }
  if (!is.null(segments)) {
    p = p + geom_segment(
      data = segments, aes(x = x, y = y, xend = xend, yend = yend, linetype = linetype),
      linewidth = 0.8, show.legend = FALSE
    ) +
      scale_linetype_identity()
  }
  p = p +
    geom_point(data = points, aes(x = x, y = y), size = point_size) +
    geom_text(
      data = points, aes(x = x + dx, y = y + dy, label = label),
      parse = TRUE, size = label_size
    ) +
    coord_fixed(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(8, 8, 8, 8))

  ggsave(filename = filename, plot = p, width = 6, height = 5.3, dpi = 300)
}

out = function(k) sprintf("../figure/nelder_mead_steps_%d.png", k)

# --- iteration 1: reflection succeeds (case 2) --------------------------------

v = order_simplex(rbind(c(1.6, 1.0), c(1.5, -1.6), c(3.8, -1.6)))
vb = centroid(v)
vr = vb + rho * (vb - v[3L, ])
ve = vb + chi * (vr - vb)
stopifnot(f(vr) < f(v[1L, ]), f(ve) > f(vr)) # case 2, and v_r beats v_e

pts_v = vertex_points(v, dy = c(above, below, below))
# edge v_1 v_2 is vertical and the reflection line passes through the centroid,
# so its label goes to the upper right
pt_vb = pt(vb, "bar(v)", dx = 0.4, dy = 0.2)
pt_vr = pt(vr, "v[r]")
pt_ve = pt(ve, "v[e]")

# 1: ordered vertices, no edges yet
draw_frame(pts_v, filename = out(1L))

# 2: centroid of v_1 and v_2
draw_frame(
  rbind(pts_v, pt_vb), simplex = v,
  segments = seg(v[1L, ], v[2L, ], "solid"),
  filename = out(2L)
)

# 3: reflection of the worst vertex through the centroid
draw_frame(
  rbind(pts_v, pt_vb, pt_vr), simplex = v,
  segments = seg(v[3L, ], vr, "solid"),
  filename = out(3L)
)

# 4: v_r accepted, worst vertex dropped, simplex re-ordered
v_new = order_simplex(rbind(v[1L, ], v[2L, ], vr))
pts_new = vertex_points(v_new, dy = c(above, above, below))
draw_frame(pts_new, simplex = v_new, ghost = v, filename = out(4L))

# 5: expansion candidate beyond v_r
draw_frame(
  rbind(pts_v, pt_vb, pt_vr, pt_ve), simplex = v,
  segments = rbind(seg(v[3L, ], vr, "solid"), seg(vr, ve, "dashed")),
  filename = out(5L)
)

# --- iteration 2: reflection fails (case 3) -----------------------------------

v = v_new
vb = centroid(v)
vr = vb + rho * (vb - v[3L, ])
vc = vb + gam * (v[3L, ] - vb)
stopifnot(f(vr) >= f(v[2L, ]), f(vc) < f(v[3L, ])) # case 3, and v_c is accepted

pts_v = pts_new
# the reflection line leaves the centroid steeply upwards, so label to the right
pt_vb = pt(vb, "bar(v)", dx = 0.38, dy = 0.3)
pt_vr = pt(vr, "v[r]")
# v_c sits on the segment from the centroid to v_3, squeezed between the edges;
# the only pocket wide enough for the label is above-left of the point
pt_vc = pt(vc, "v[c]", dx = -0.53, dy = 0.4)

# 6: reflection point lies far outside, worse than v_2
draw_frame(
  rbind(pts_v, pt_vb, pt_vr), simplex = v,
  segments = seg(v[3L, ], vr, "solid"),
  filename = out(6L)
)

# 7: contraction towards the worst vertex instead
draw_frame(
  rbind(pts_v, pt_vb, pt_vr, pt_vc), simplex = v,
  segments = rbind(seg(v[3L, ], vr, "dashed"), seg(vb, v[3L, ], "solid")),
  filename = out(7L)
)

# 8: v_c accepted, worst vertex dropped, simplex re-ordered (v_c is now best)
v_new = order_simplex(rbind(v[1L, ], v[2L, ], vc))
pts_new = vertex_points(v_new, dy = c(below, above, above))
pts_new[1L, dx := 0.22] # nudged right, between the two dotted edges of the old simplex
draw_frame(pts_new, simplex = v_new, ghost = v, filename = out(8L))

# 9: shrinking, the fallback if v_c were not better than v_3 (hypothetical here,
# see header): every vertex moves halfway towards v_1, the old simplex stays as
# dotted outline
v_shrunk = t(apply(v, 1L, function(vi) v[1L, ] + sig * (vi - v[1L, ])))
pts_shrunk = vertex_points(v_shrunk, dy = c(above, above, below))
pts_shrunk[3L, dx := -0.38] # below-left, clear of the dashed displacement line
draw_frame(
  pts_shrunk, simplex = v_shrunk, ghost = v,
  segments = rbind(seg(v[2L, ], v_shrunk[2L, ]), seg(v[3L, ], v_shrunk[3L, ])),
  filename = out(9L)
)
