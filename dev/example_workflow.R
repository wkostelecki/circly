
library(circly)
NROW = 3
NCOL = 2

source('./R/sample_data.R')
data = sample_data(NROW = NROW, NCOL = NCOL)

SHAPES = chord_shapes(data)

# plot --------------------------------------------------------------------

library(ggplot2)
g = ggplot() +
  geom_polygon(data = SHAPES$chords, aes(x, y, group = Chord), alpha = 0.5) +
  geom_polygon(data = SHAPES$crust, aes(x, y, group = ID)) +
  coord_equal() +
  theme_minimal()
print(g)

