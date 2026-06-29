# Install if needed: install.packages(c("ggplot2", "dplyr", "tibble"))

library(ggplot2)
library(dplyr)
library(tibble)

roadmap_data <- tribble(
  ~stage, ~y, ~title, ~desc,
  1, 0,   "Welcome",          "Introduction\nto Stories → Models\nWhy This Matters",
  2, -1.5,"Human Behavior",   "Game Theory\nHeroes, Cooperation\nPrisoner's Dilemma",
  3, -3,  "Social Dynamics",  "Networks\nViral Trends, Memes\nBiases & Rumours",
  4, -4.5,"Spatial Patterns", "Proximity\nVoronoi / Delaunay\nKandinsky",
  5, -6,  "Randomness",       "Random Walks\nHamlet\nHypothesis Testing",
  6, -7.5,"Geometry & Emergence", "Fractals\nIterated Functions\nSymmetry",
  7, -9,  "Synthesis & Projects","Design Experiments\nML Ideas\nFinal Artifacts"
)

p <- ggplot(roadmap_data, aes(x = 0, y = y)) +
  geom_segment(aes(xend = 0, yend = y - 1.3), 
            color = "#3498db", linewidth = 1.5, 
            arrow = arrow(length = unit(0.35, "cm"), type = "closed")) +
  
  geom_rect(aes(xmin = -2.4, xmax = 2.4, 
                ymin = y - 0.6, ymax = y + 0.6),
            fill = "white", color = "#2c3e50", linewidth = 1.3) +
  
  geom_text(aes(label = title), size = 5.8, fontface = "bold", color = "#2c3e50", vjust = -0.6) +
  geom_text(aes(label = desc), size = 4, color = "#34495e", vjust = 1.2, lineheight = 0.95) +
  
  annotate("text", x=0, y=1.4, 
           label = "Old Tortoise Taught Us:\nThe Model of the Story", 
           size=9, fontface="bold", color="#2c3e50") +
  annotate("text", x=0, y=0.7, 
           label = "Visual Learning Roadmap", size=5, color="#7f8c8d") +
  
  theme_void() +
  theme(plot.margin = margin(40,40,50,40),
        plot.background = element_rect(fill = "#f8f9fa", color=NA)) +
  coord_fixed(xlim=c(-3.2, 3.2), ylim=c(-10.2, 2))

print(p)
ggsave("course_roadmap_ggplot.png", p, width=11, height=14, dpi=300, bg="#f8f9fa")

