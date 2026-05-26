# Used in: slides-multivar-first-order-1-GD.tex
#
# Draws the 2-input, 2-hidden-layer (2 units each), 1-output network diagram.
# Hidden neurons are split into left half (z_in, pre-activation, lightyellow)
# and right half (z_out, post-activation, lightblue) using ggforce semicircles.

library(ggplot2)
library(ggforce)

r = 0.08  # node radius (data coords; coord_equal keeps circles round)

# -------------------------------------------------------------------------
# Node centre positions
# -------------------------------------------------------------------------
input_nodes = data.frame(
  x     = 0,
  y     = c(0.25, 0.75),
  label = c("x[1]", "x[2]"),
  layer = "input"
)

hidden1_nodes = data.frame(
  x     = 0.4,
  y     = c(0.25, 0.75),
  label = c("", ""),
  layer = "hidden1"
)

hidden2_nodes = data.frame(
  x     = 0.8,
  y     = c(0.25, 0.75),
  label = c("", ""),
  layer = "hidden2"
)

output_node = data.frame(
  x     = 1.2,
  y     = 0.5,
  label = "f",
  layer = "output"
)

plain_nodes = rbind(input_nodes, output_node)
hidden_nodes = rbind(
  cbind(hidden1_nodes, layer_idx = 1L),
  cbind(hidden2_nodes, layer_idx = 2L)
)

# -------------------------------------------------------------------------
# Edges
# -------------------------------------------------------------------------
make_edges = function(from, to, layer_idx) {
  do.call(rbind, do.call(c, lapply(seq_len(nrow(from)), function(i) {
    lapply(seq_len(nrow(to)), function(j) {
      mx = (from$x[i] + to$x[j]) / 2
      my = (from$y[i] + to$y[j]) / 2
      nudge = if (from$y[i] > to$y[j]) 0.06 else if (from$y[i] < to$y[j]) -0.06 else 0.06
      data.frame(
        x    = from$x[i], y    = from$y[i],
        xend = to$x[j],   yend = to$y[j],
        lx = mx, ly = my + nudge,
        label = sprintf("theta[%d%d]^{(%d)}", j, i, layer_idx)
      )
    })
  })))
}

edges = rbind(
  make_edges(input_nodes,   hidden1_nodes, 1L),
  make_edges(hidden1_nodes, hidden2_nodes, 2L),
  make_edges(hidden2_nodes, output_node,   3L)
)

# -------------------------------------------------------------------------
# Layer title labels (below nodes)
# -------------------------------------------------------------------------
layer_labels = data.frame(
  x     = c(0,       0.4,          0.8,          1.2),
  label = c("Input", "Hidden\nlayer 1", "Hidden\nlayer 2", "Output")
)

# -------------------------------------------------------------------------
# Build semicircle data for hidden nodes
# -------------------------------------------------------------------------
# ggforce::geom_arc_bar with stat="identity" needs: x0, y0, r, r0, start, end
# Left half  (z_in):  start = pi/2,  end = 3*pi/2  (counter-clockwise)
# Right half (z_out): start = -pi/2, end = pi/2

half_data = do.call(rbind, lapply(seq_len(nrow(hidden_nodes)), function(k) {
  cx = hidden_nodes$x[k]
  cy = hidden_nodes$y[k]
  l  = hidden_nodes$layer_idx[k]
  j  = if (cy < 0.5) 1L else 2L
  data.frame(
    x0         = rep(cx, 2),
    y0         = rep(cy, 2),
    r          = rep(r,  2),
    r0         = 0,
    start      = c( pi / 2, -pi / 2),
    end        = c(3 * pi / 2, pi / 2),
    half       = c("in", "out"),
    fill_col   = c("white", "white"),
    # label positions: slightly inside each half
    lx         = c(cx - r * 0.45, cx + r * 0.45),
    ly         = rep(cy, 2),
    z_label    = c(
      sprintf("z[list(\"in\",%d)]^{(%d)}", j, l),
      sprintf("z[list(\"out\",%d)]^{(%d)}", j, l)
    )
  )
}))

# -------------------------------------------------------------------------
# Divider segments through hidden nodes
# -------------------------------------------------------------------------
dividers = data.frame(
  x    = hidden_nodes$x,
  xend = hidden_nodes$x,
  y    = hidden_nodes$y - r,
  yend = hidden_nodes$y + r
)

# -------------------------------------------------------------------------
# Plot
# -------------------------------------------------------------------------
p = ggplot() +
  # edges first (behind nodes)
  geom_segment(
    data = edges,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "grey60", linewidth = 0.4
  ) +
  geom_text(
    data = edges,
    aes(x = lx, y = ly, label = label),
    parse = TRUE, size = 4, colour = "grey30"
  ) +
  # plain (input / output) nodes
  geom_point(
    data = plain_nodes,
    aes(x = x, y = y),
    shape = 21, fill = "white", colour = "black",
    size = 18, stroke = 1
  ) +
  geom_text(
    data = plain_nodes[plain_nodes$label != "", ],
    aes(x = x, y = y, label = label),
    parse = TRUE, size = 5
  ) +
  # hidden node halves (filled semicircles)
  geom_arc_bar(
    data = half_data,
    aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = start, end = end,
        fill = fill_col),
    colour = NA, show.legend = FALSE
  ) +
  scale_fill_identity() +
  # hidden node outlines
  geom_circle(
    data = hidden_nodes,
    aes(x0 = x, y0 = y, r = r),
    colour = "black", linewidth = 0.6, fill = NA, inherit.aes = FALSE
  ) +
  # divider lines
  geom_segment(
    data = dividers,
    aes(x = x, xend = xend, y = y, yend = yend),
    colour = "black", linewidth = 0.4
  ) +
  # z_in / z_out labels inside hidden nodes
  geom_text(
    data = half_data,
    aes(x = lx, y = ly, label = z_label),
    parse = TRUE, size = 2.8, colour = "grey20"
  ) +
  # layer title labels
  geom_text(
    data = layer_labels,
    aes(x = x, y = 0.06, label = label),
    size = 4, vjust = 1, lineheight = 0.85
  ) +
  coord_equal(xlim = c(-0.15, 1.35), ylim = c(0.0, 1.05)) +
  theme_void()

ggsave(
  filename = "../figure/nn_architecture.pdf",
  plot     = p,
  width    = 8,
  height   = 4.5
)