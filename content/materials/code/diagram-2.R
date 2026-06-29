library(ggplot2)
library(dplyr)
library(tibble)

roadmap_data <- tribble(
  ~y, ~title, ~desc, ~color, ~icon,
  0, "Welcome", "Introduction to\nStories → Models", "#2c3e50", "🏠",
  -1.7, "Human Behavior", "Game Theory\nHeroes & Cooperation", "#e74c3c", "🦸",
  -3.4, "Social Dynamics", "Networks, Viral Trends\nMemes, Biases & Rumours", "#3498db", "🌐",
  -5.1, "Spatial Patterns", "Proximity, Voronoi\nDelaunay & Kandinsky", "#2ecc71", "📍",
  -6.8, "Randomness", "Random Walks\nHamlet, Probability", "#f1c40f", "🎲",
  -8.5, "Geometry & Emergence", "Fractals\nSymmetry & Iteration", "#9b59b6", "🌿",
  -10.2, "Synthesis & Projects", "Design Experiments\nApply to Art & Design", "#e67e22", "🎨"
)

p <- ggplot(roadmap_data, aes(x = 0, y = y)) +
  geom_segment(aes(xend = 0, yend = y - 1.5),
    color = "#34495e", linewidth = 1.9,
    arrow = arrow(length = unit(0.35, "cm"), type = "closed")
  ) +

  # Even wider boxes
  geom_rect(
    aes(
      xmin = -3.1, xmax = 3.1,
      ymin = y - 0.78, ymax = y + 0.78
    ),
    fill = "white", color = roadmap_data$color, linewidth = 2.4
  ) +

  # Icons (shifted left)
  geom_text(aes(label = icon), size = 20, nudge_x = -2.1, nudge_y = 0.3) +

  # Titles (bold, left-aligned inside box)
  geom_text(aes(label = title),
    size = 5.6, fontface = "bold",
    color = "#2c3e50", nudge_x = -0.8, nudge_y = 0.3, hjust = 0
  ) +

  # Descriptions
  geom_text(aes(label = desc),
    size = 3.95, color = "#2c3e50",
    nudge_x = -0.8, nudge_y = -0.28, hjust = 0, lineheight = 0.95
  ) +
  annotate("text",
    x = 0, y = 1.8,
    label = "Old Tortoise Taught Us:\nThe Model of the Story",
    size = 9, fontface = "bold", color = "#2c3e50", lineheight = 0.9
  ) +
  annotate("text",
    x = 0, y = 0.9,
    label = "Visual Roadmap — Color-Coded Themes",
    size = 5, color = "#7f8c8d"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.margin = margin(60, 60, 70, 60)
  ) +
  coord_fixed(xlim = c(-3.8, 3.8), ylim = c(-11.8, 2.6))

print(p)

ggsave("content/materials/code/enhanced_roadmap_ggplot.png", p, width = 12, height = 15.5, dpi = 300, bg = "#f8f9fa")
