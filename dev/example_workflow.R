library(dplyr)
library(circly)
NROW = 10
NCOL = 2

source('./R/sample_data.R')
data = sample_data(NROW = NROW, NCOL = NCOL)

data = expand.grid(FROM = letters[1:NCOL],
                   TO = LETTERS[1:NROW]) %>%
  arrange(FROM)
data$IN = 1:(NCOL * NROW)
data$OUT = (NCOL * NROW):1

SHAPES = chord_shapes(data)

# plot --------------------------------------------------------------------

library(ggplot2)
g = ggplot() +
  geom_polygon(data = SHAPES$chords, aes(x, y, group = Chord), alpha = 0.5) +
  geom_polygon(data = SHAPES$crust, aes(x, y, group = ID)) +
  coord_equal() +
  theme_minimal()
print(g)

