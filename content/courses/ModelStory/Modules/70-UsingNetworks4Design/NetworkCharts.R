```{r}
#| label: random-animated-graph
#| echo: false
#| eval: false
#| # =========================================================
# Animated Random Graph in R
# =========================================================
# This script builds a random (Erdos-Renyi) graph and animates
# its edges appearing one at a time, with a fixed node layout
# so the animation looks smooth rather than jittery.
#
# Output: random_graph_animation.gif
# =========================================================

# ---- 1. Install packages (run once) ----
required_pkgs <- c("igraph", "ggraph", "tidygraph", "gganimate", "gifski", "ggplot2")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(igraph)
library(ggraph)
library(tidygraph)
library(gganimate)
library(ggplot2)

set.seed(42)

# ---- 2. Create a random graph ----
# Erdos-Renyi random graph: n nodes, probability p of an edge between any pair
n_nodes <- 16
p_edge  <- 0.1

g <- sample_gnp(n = n_nodes, p = p_edge, directed = FALSE)
g <- as_tbl_graph(g)

# ---- 3. Fix a layout so nodes don't jump around between frames ----
layout_coords <- create_layout(g, layout = "fr")  # Fruchterman-Reingold

# ---- 4. Assign each edge a random "appearance time" (frame number) ----
edge_df <- as.data.frame(g %>% activate(edges) %>% as_tibble())
n_edges <- nrow(edge_df)
n_frames <- 40

edge_df$appear_frame <- sample(1:n_frames, n_edges, replace = TRUE)

# Build a long data frame: for each frame, which edges are visible
edge_frames <- do.call(rbind, lapply(1:n_frames, function(f) {
  visible <- edge_df[edge_df$appear_frame <= f, ]
  if (nrow(visible) == 0) return(NULL)
  visible$frame <- f
  visible
}))

# Attach node coordinates to edges (from/to)
node_coords <- layout_coords[, c("x", "y")]
node_coords$name <- 1:nrow(node_coords)

edge_frames$x    <- node_coords$x[edge_frames$from]
edge_frames$y    <- node_coords$y[edge_frames$from]
edge_frames$xend <- node_coords$x[edge_frames$to]
edge_frames$yend <- node_coords$y[edge_frames$to]

# Nodes are visible from frame 1 onward (all present, just edges grow in)
node_frames <- do.call(rbind, lapply(1:n_frames, function(f) {
  df <- node_coords
  df$frame <- f
  df
}))

# ---- 5. Build the animated plot ----
p <- ggplot() +
  geom_segment(
    data = edge_frames,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "steelblue", alpha = 0.6, linewidth = 0.6
  ) +
  geom_point(
    data = node_frames,
    aes(x = x, y = y),
    size = 5, color = "firebrick"
  ) +
  theme_void() +
  labs(title = "Random Graph Growth — Frame {closest_state}") +
  transition_states(frame, transition_length = 1, state_length = 1) +
  ease_aes("linear")

# ---- 6. Render and save as GIF ----
anim <- animate(
  p,
  nframes = n_frames * 2,
  fps = 8,
  width = 700, height = 700,
  renderer = gifski_renderer()
)

anim_save("./images/random_graph_animation.gif", animation = anim)

# ---- Done ----
# Open "random_graph_animation.gif" to view the animation:
# edges are added one by one to a fixed random layout, showing
# the graph "growing" from empty to fully random-connected.
# 
```

```{r}
#| label: fig-random-static
#| fig-cap: Random Graph
#| echo: false
#| eval: false
#| 
# =========================================================
# Static Random Network in R
# =========================================================
# Builds an Erdos-Renyi random graph and plots it once,
# with no animation.
#
# Output: random_graph_static.png
# =========================================================

# set.seed(2026)

# ---- 2. Create a random graph ----
# Erdos-Renyi random graph: n nodes, probability p of an edge between any pair
# n_nodes <- 16
# p_edge  <- 0.08

# g <- sample_gnp(n = n_nodes, p = p_edge, directed = FALSE)
# g <- as_tbl_graph(g)
# layout_coords <- create_layout(g, layout = "fr")  # Fruchterman-Reingold

# Sanity checks (optional, printed to console)
cat("Connected:", is_connected(as.igraph(g)), "\n")
cat("Degree range:", range(degree(as.igraph(g))), "\n")

# ---- 3. Plot with a force-directed layout ----
p <- ggraph(graph =  layout_coords) +
  geom_edge_link(color = "steelblue", alpha = 0.6, width = 0.6) +
  geom_node_point(size = 5, color = "firebrick") +
  theme_void() + coord_fixed() + 
  labs(title = "Random Network (Erdos-Renyi, n = 16, p = 0.10)")

# ---- 4. Save as PNG ----
ggsave("./images/random_graph_static.png", plot = p, width = 7, height = 7, dpi = 150)

```


```{r}
#| label: random-regular-graph
#| echo: false
#| eval: false
#| 
#| 
# =========================================================
# Animated Regular Connected Network in R
# =========================================================
# This builds a k-regular ring lattice (every node has the same
# degree, and the graph is guaranteed connected) and animates its
# edges appearing one at a time on a fixed circular layout.
#
# This is the classic "regular network" used as the starting point
# for Watts-Strogatz small-world models, and pairs naturally with
# the Erdos-Renyi random graph animation.
#
# Output: regular_graph_animation.gif
# =========================================================

# ---- 1. Install packages (run once) ----
required_pkgs <- c("igraph", "ggraph", "tidygraph", "gganimate", "gifski", "ggplot2")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(igraph)
library(ggraph)
library(tidygraph)
library(gganimate)
library(ggplot2)

set.seed(42)

# ---- 2. Create a regular connected graph (ring lattice) ----
# Each node connects to its k nearest neighbors on each side of a ring.
# This is deterministic, always connected, and every node has the
# same degree (2 * neighbors_each_side) -> a "regular" graph.
n_nodes <- 16
neighbors_each_side <- 2   # degree = 4 for every node

g <- make_lattice(length = n_nodes, dim = 1, nei = neighbors_each_side, periodic = TRUE)
g <- as_tbl_graph(g)

# Sanity checks (optional, printed to console)
cat("Connected:", is_connected(as.igraph(g)), "\n")
cat("Degrees:", unique(degree(as.igraph(g))), "\n")

# ---- 3. Fixed circular layout (natural for a ring lattice) ----
layout_coords <- create_layout(g, layout = "circle")

# ---- 4. Assign each edge an appearance order ----
# Order edges by neighbor "distance" so the animation reveals
# the nearest-neighbor connections first, then the next ring out.
edge_df <- as.data.frame(g %>% activate(edges) %>% as_tibble())
n_edges <- nrow(edge_df)
n_frames <- 40

# Circular distance between connected nodes (used just for ordering)
circ_dist <- function(a, b, n) {
  d <- abs(a - b)
  pmin(d, n - d)
}
edge_df$dist <- circ_dist(edge_df$from, edge_df$to, n_nodes)

# Map distance ring -> a frame band, with slight jitter so edges
# in the same ring don't all pop in on the exact same frame
max_dist <- max(edge_df$dist)
edge_df$appear_frame <- round(
  (edge_df$dist / max_dist) * (n_frames * 0.7) +
    runif(n_edges, 1, n_frames * 0.3)
)
edge_df$appear_frame <- pmin(pmax(edge_df$appear_frame, 1), n_frames)

# Build a long data frame: for each frame, which edges are visible
edge_frames <- do.call(rbind, lapply(1:n_frames, function(f) {
  visible <- edge_df[edge_df$appear_frame <= f, ]
  if (nrow(visible) == 0) return(NULL)
  visible$frame <- f
  visible
}))

# Attach node coordinates to edges (from/to)
node_coords <- layout_coords[, c("x", "y")]
node_coords$name <- 1:nrow(node_coords)

edge_frames$x    <- node_coords$x[edge_frames$from]
edge_frames$y    <- node_coords$y[edge_frames$from]
edge_frames$xend <- node_coords$x[edge_frames$to]
edge_frames$yend <- node_coords$y[edge_frames$to]

# Nodes are visible from frame 1 onward
node_frames <- do.call(rbind, lapply(1:n_frames, function(f) {
  df <- node_coords
  df$frame <- f
  df
}))

# ---- 5. Build the animated plot ----
p <- ggplot() +
  geom_segment(
    data = edge_frames,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "darkgreen", alpha = 0.6, linewidth = 0.6
  ) +
  geom_point(
    data = node_frames,
    aes(x = x, y = y),
    size = 5, color = "firebrick"
  ) +
  theme_void() +
  labs(title = "Regular Network Growth — Frame {closest_state}") +
  transition_states(frame, transition_length = 1, state_length = 1) +
  ease_aes("linear")

# ---- 6. Render and save as GIF ----
anim <- animate(
  p,
  nframes = n_frames * 2,
  fps = 8,
  width = 700, height = 700,
  renderer = gifski_renderer()
)

anim_save("./images/regular_graph_animation.gif", animation = anim)

# ---- Done ----
# Open "regular_graph_animation.gif" to view the animation:
# nodes are arranged in a circle, and edges to nearest neighbors
# appear first, followed by next-ring connections, revealing the
# symmetric, fully connected regular lattice.
# 
```


```{r}
#| label: fig-regular-static
#| echo: false
#| fig-cap: \"Regular\" Network
#| eval: false
#| 
# =========================================================
# Static Regular Connected Network in R
# =========================================================
# Builds a k-regular ring lattice (every node has the same
# degree, graph guaranteed connected) and plots it once,
# with no animation.
#
# Output: regular_graph_static.png
# =========================================================

# ---- 1. Install packages (run once) ----
required_pkgs <- c("igraph", "ggraph", "tidygraph", "ggplot2")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)

set.seed(2026)

# ---- 2. Create a regular connected graph (ring lattice) ----
# Each node connects to its k nearest neighbors on each side of a ring.
# Deterministic, always connected, and every node has the same
# degree (2 * neighbors_each_side) -> a "regular" graph.
n_nodes <- 16
neighbors_each_side <- 2   # degree = 4 for every node

g <- make_lattice(length = n_nodes, dim = 1, nei = neighbors_each_side, circular = TRUE)
g <- as_tbl_graph(g)

# Sanity checks (optional, printed to console)
cat("Connected:", is_connected(as.igraph(g)), "\n")
cat("Degrees:", unique(degree(as.igraph(g))), "\n")

# ---- 3. Plot with a circular layout (natural for a ring lattice) ----
p <- ggraph(g, layout = "circle") +
  geom_edge_link(color = "darkgreen", alpha = 0.6, width = 0.6) +
  geom_node_point(size = 5, color = "firebrick") +
  theme_void() + coord_fixed() + 
  labs(title = "Regular Connected Network (Ring Lattice, n = 30, degree = 4)")

# ---- 4. Save as PNG ----
ggsave("./images/regular_graph_static.png", plot = p, width = 7, height = 7, dpi = 150)

```

```{r}
#| label: small-worlds-1
#| fig-cap: Small worlds (Random)
#| layout-ncol: 2
#| echo: false
#| eval: false

knitr::include_graphics("./images/random_graph_animation.gif")

knitr::include_graphics("./images/random_graph_static.png")
```


```{r}
#| label: small-worlds-2
#| fig-cap: ""
#| fig-subcap: 
  #|  - Emergent
  #|  - Final State
#| layout-ncol: 2
#| echo: false
#| eval: false
#| 

knitr::include_graphics("./images/regular_graph_animation.gif")

knitr::include_graphics("./images/regular_graph_static.png")

```


```{r}
#| echo: false
#| eval: false
#| 
# =========================================================
# Reproducing Figure 1 from Watts & Strogatz (1998)
# "Collective dynamics of 'small-world' networks", Nature 393
# =========================================================
# Figure 1 shows the random rewiring procedure that interpolates
# between a regular ring lattice (p = 0) and a random network
# (p = 1), passing through a "small-world" regime at intermediate p.
#
# Per the paper: "For clarity, n = 20 and k = 4 in the schematic
# examples shown here." Each vertex starts connected to its k
# nearest neighbours (k/2 on each side) on a ring; edges are then
# rewired to a uniformly random vertex with probability p (no
# duplicate edges, no self-loops), sweeping around the ring once
# per "distance" of neighbour, for k/2 total laps.
#
# igraph's sample_smallworld() implements exactly this
# Watts-Strogatz rewiring procedure.
#
# Output: watts_strogatz_figure1.png
# =========================================================

# ---- 1. Install packages (run once) ----
required_pkgs <- c("igraph", "ggraph", "tidygraph", "ggplot2", "patchwork")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(patchwork)

set.seed(1)

# ---- 2. Parameters matching the paper's schematic (n = 20, k = 4) ----
n <- 20
k <- 4
nei <- k / 2   # neighbours on each side, as required by sample_smallworld()

p_regular <- 0     # p = 0: unrewired ring lattice
p_small   <- 0.1   # intermediate p: "small-world" regime
p_random  <- 1     # p = 1: fully rewired, random network

# ---- 3. Generate the three graphs via Watts-Strogatz rewiring ----
g_regular <- sample_smallworld(dim = 1, size = n, nei = nei, p = p_regular)
g_small   <- sample_smallworld(dim = 1, size = n, nei = nei, p = p_small)
g_random  <- sample_smallworld(dim = 1, size = n, nei = nei, p = p_random)

# ---- 4. Helper to plot one panel on a circular layout ----
# All three use the same circular vertex arrangement (as in the
# original figure), so only the edges change across panels.
plot_panel <- function(g, title) {
  gt <- as_tbl_graph(g)
  ggraph(gt, layout = "circle") +
    geom_edge_arc(strength = 0.15, color = "black", alpha = 0.8, width = 0.4) +
    geom_node_point(size = 4, color = "black", fill = "white", shape = 21, stroke = 1) +
    coord_fixed() +
    theme_void() +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 13))
}

p1 <- plot_panel(g_regular, "Regular\n(p = 0)")
p2 <- plot_panel(g_small,   "Small-world\n(p = 0.1)")
p3 <- plot_panel(g_random,  "Random\n(p = 1)")

# ---- 5. Combine panels side by side with a caption, as in Fig. 1 ----
combined <- (p1 | p2 | p3) +
  plot_annotation(
    title = "Figure 1 (Watts & Strogatz, 1998): Random rewiring procedure",
    caption = "p = 0                              Increasing randomness                              p = 1",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = element_text(hjust = 0.5, size = 11, face = "italic")
    )
  )

print(combined)

# ---- 6. Save as PNG ----
ggsave("./images/watts_strogatz_figure1.png", plot = combined, width = 12, height = 4.5, dpi = 150)

# ---- Notes ----
# - This reproduces the SCHEMATIC in Fig. 1 (n = 20, k = 4), used to
#   illustrate the rewiring mechanism, not Fig. 2's L(p)/C(p) curves
#   (which use n = 1000, k = 10, averaged over 20 realizations).
# - Because rewiring is stochastic, the "small-world" and "random"
#   panels will differ slightly each run unless you fix set.seed().
#   
```

```{r}
#| echo: false
#| eval: false

# =========================================================
# Reproducing Figure 2 from Watts & Strogatz (1998)
# "Collective dynamics of 'small-world' networks", Nature 393
# =========================================================
# Figure 2 shows the characteristic path length L(p) and clustering
# coefficient C(p) for the family of randomly rewired ring lattices
# from Fig. 1, each normalized by its value at p = 0, plotted
# against p on a log scale.
#
# Per the paper's Fig. 2 legend: "All the graphs have n = 1000
# vertices and an average degree of k = 10 edges per vertex... The
# data shown are averages over 20 random realizations of the
# rewiring process."
#
# L(p): mean shortest-path length (mean_distance in igraph)
# C(p): average LOCAL clustering coefficient (transitivity, type
#       = "average" in igraph -- this matches the paper's Cv-based
#       definition, not the global triangle-to-triple ratio)
#
# NOTE: this involves generating hundreds of 1000-node graphs and
# computing shortest paths / clustering on each, so it can take a
# few minutes to run. Reduce n_realizations, n, or the length of
# p_values below for a quicker (noisier) approximation.
#
# Output: watts_strogatz_figure2.png
# =========================================================

# ---- 1. Install packages (run once) ----
required_pkgs <- c("igraph", "ggplot2", "tidyr", "dplyr")
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)

set.seed(1)

# ---- 2. Parameters matching the paper ----
n <- 1000
k <- 10
nei <- k / 2          # neighbours per side, required by sample_smallworld()
n_realizations <- 20  # averaged over 20 random realizations, as in the paper

# Log-spaced p values from 0.0001 to 1 (paper's x-axis range)
p_values <- 10^seq(log10(0.0001), log10(1), length.out = 20)

# ---- 3. Baseline L(0) and C(0) from the unrewired regular lattice ----
g0 <- sample_smallworld(dim = 1, size = n, nei = nei, p = 0)
L0 <- mean_distance(g0, directed = FALSE)
C0 <- transitivity(g0, type = "average")

cat(sprintf("Baseline regular lattice: L(0) = %.3f, C(0) = %.4f\n", L0, C0))

# ---- 4. Compute L(p) and C(p), averaged over multiple realizations ----
results <- data.frame(p = p_values, L = NA_real_, C = NA_real_)

for (i in seq_along(p_values)) {
  p <- p_values[i]
  
  L_vals <- numeric(n_realizations)
  C_vals <- numeric(n_realizations)
  
  for (r in 1:n_realizations) {
    g <- sample_smallworld(dim = 1, size = n, nei = nei, p = p)
    L_vals[r] <- mean_distance(g, directed = FALSE)
    C_vals[r] <- transitivity(g, type = "average")
  }
  
  results$L[i] <- mean(L_vals)
  results$C[i] <- mean(C_vals)
  
  cat(sprintf("p = %.5f  ->  L = %.3f, C = %.4f\n", p, results$L[i], results$C[i]))
}

# ---- 5. Normalize by the p = 0 baseline, as in the paper ----
results$L_norm <- results$L / L0
results$C_norm <- results$C / C0

# ---- 6. Reshape for plotting both curves together ----
plot_df <- results %>%
  select(p, L_norm, C_norm) %>%
  pivot_longer(cols = c(L_norm, C_norm), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, L_norm = "L(p) / L(0)", C_norm = "C(p) / C(0)"))

# ---- 7. Plot: log-scale x-axis, points + line, matching Fig. 2 style ----
p_fig2 <- ggplot(plot_df, aes(x = p, y = value, color = metric, shape = metric)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  geom_point(size = 2.5) +
  scale_x_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1),
    labels = c("0.0001", "0.001", "0.01", "0.1", "1")
  ) +
  scale_color_manual(values = c("L(p) / L(0)" = "black", "C(p) / C(0)" = "black")) +
  scale_shape_manual(values = c("L(p) / L(0)" = 1, "C(p) / C(0)" = 16)) +
  labs(
    title = "Figure 2 (Watts & Strogatz, 1998): Small-world transition",
    x = "p",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.85, 0.85),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )

print(p_fig2)

# ---- 8. Save as PNG ----
ggsave("./images/watts_strogatz_figure2.png", plot = p_fig2, width = 7, height = 5, dpi = 150)

# ---- Notes ----
# - The paper's key result: over a broad range of intermediate p,
#   L(p) drops rapidly toward the random-graph value while C(p)
#   stays close to its regular-lattice value -- this is the
#   "small-world" regime, and should be clearly visible as a
#   sharp early drop in the open circles (L) while the filled
#   circles (C) remain high for longer.
# - This is computationally heavier than Fig. 1's schematic; if it's
#   too slow, try n = 200-300 and/or n_realizations = 5-10 first to
#   confirm the script works, then scale up for a closer match.
#   
#   
```

```{r}
#| echo: false
#| label: fig-small-worlds
#| layout-ncol: 2
#| fig-align: center
#| fig-subcap: 
  #|  - Increasing Random Links ( L to R )
  #|  - \"Network Distance\" and \"Clustering\"
#| out-width: "100%"
#| out-height: "100%"
#| 

knitr::include_graphics("./images/watts_strogatz_figure1.png")
knitr::include_graphics("./images/watts_strogatz_figure2.png")
```


```{r}
#| echo: false
#| eval: false
#| 
# ============================================================
# Animated reproduction of Figure 1 from:
# Watts, D. J., & Strogatz, S. H. (1998).
# "Collective dynamics of 'small-world' networks." Nature, 393(6684), 440-442.
#
# The original Figure 1 shows three static snapshots of a ring lattice
# as edges are randomly rewired with probability p:
#   p = 0    -> regular ring lattice
#   p small  -> "small-world" network (few long-range shortcuts)
#   p = 1    -> random graph
#
# This script animates that entire process by smoothly sweeping p from
# 0 to 1 and rewiring edges one by one, while keeping node positions
# fixed on a circle (exactly as in the paper's figure) so the viewer
# can see individual edges "jump" from local to long-range connections.
#
# Requires: igraph, gifski
#   install.packages(c("igraph", "gifski"))
# ============================================================

library(igraph)
library(gifski)

set.seed(2026)

# ---------------------------
# Parameters (feel free to tweak)
# ---------------------------
N        <- 20   # number of nodes on the ring
K        <- 4    # each node initially connects to K nearest neighbors (K/2 per side)
n_frames <- 90    # number of animation frames
gif_file <- "watts_strogatz_animation.gif"

# Use a log-ish spacing for p so the interesting transition region
# (small p) gets more frames, similar to how the paper emphasizes
# the log-scale x-axis in Figure 2. Start at exactly 0.
p_values <- c(0, exp(seq(log(1e-3), log(1), length.out = n_frames - 1)))

# ---------------------------
# Build the base ring lattice edge list
# ---------------------------
build_ring_lattice <- function(N, K) {
  edges <- matrix(ncol = 2, nrow = 0)
  for (i in 1:N) {
    for (j in 1:(K / 2)) {
      neighbor <- ((i - 1 + j) %% N) + 1
      edges <- rbind(edges, c(i, neighbor))
    }
  }
  edges
}

base_edges <- build_ring_lattice(N, K)
n_edges    <- nrow(base_edges)

# ---------------------------
# Pre-compute, for every edge, a random "rewiring threshold" and a
# random alternative target node. As p sweeps upward, any edge whose
# threshold is below the current p becomes rewired to its pre-chosen
# random target. This makes the animation deterministic frame-to-frame
# (no flickering) while still reproducing the WS rewiring procedure.
# ---------------------------
thresholds <- runif(n_edges)

rewired_targets <- sapply(base_edges[, 1], function(i) {
  repeat {
    candidate <- sample(setdiff(1:N, i), 1)
    if (candidate != i) break
  }
  candidate
})

# ---------------------------
# Construct the graph corresponding to a given value of p
# ---------------------------
make_graph_at_p <- function(p) {
  el <- base_edges
  rewire_idx <- which(thresholds <= p)
  el[rewire_idx, 2] <- rewired_targets[rewire_idx]
  g <- graph_from_edgelist(el, directed = FALSE)
  simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
}

# Fixed circular layout so nodes never move -- only edges change,
# exactly matching the visual style of the original Figure 1.
layout_coords <- layout_in_circle(graph_from_edgelist(base_edges, directed = FALSE))

# ---------------------------
# Render the animation
# ---------------------------
save_gif({
  for (p in p_values) {
    g <- make_graph_at_p(p)
    
    plot(
      g,
      layout        = layout_coords,
      vertex.size   = 10,
      vertex.color  = "steelblue",
      vertex.frame.color = "white",
      vertex.label  = NA,
      edge.color    = adjustcolor("gray30", alpha.f = 0.7),
      edge.width    = 1.2,
      edge.curved   = 0.15,
      main          = sprintf("Watts-Strogatz rewiring:  p = %.4f", p)
    )
  }
}, gif_file = gif_file, width = 700, height = 700, delay = 1 / 15, loop = TRUE)

cat("Animation saved as:", gif_file, "\n")

```

```{r}
#| label: fig-small-world-animated
#| fig-cap: Small World Construction (Emergent)
#| echo: false
#| out-width: "80%"
#| out-height: "80%"

knitr::include_graphics("./images/watts_strogatz_animation.gif")

```
