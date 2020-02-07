library(mathart)
library(ggart)
library(ggforce)
library(Rcpp)
library(tidyverse)

Seed<-.Random.seed

df <- lissajous(a = runif(1, 0, 2), b = runif(1, 0, 2), A = runif(1, 0, 2), B = runif(1, 0, 2), d = 200) %>%
  sample_n(1001) %>%
  k_nearest_neighbour_graph(40)

ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend), df, size = 0.03,color="red3") +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0) +
  theme(panel.background = element_rect(fill = 'black', colour = 'white')) +
  annotate("text", x = 0.75, y = -1.325, label = "Rafael S. de Souza",color="white")