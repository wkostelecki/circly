#----
library(dplyr)
library(circly)
library(ggplot2)
remove(list = ls())

#----
source('./R/sample_data.R')
source('./R/chord_spacing.R')
data = sample_data(NROW = 5, NCOL = 5)

#----
source('./R/outer_spacing.R')
segment = outer_spacing(data)

#----
source('./R/outer_segment.R')
shapes = outer_segments(segment)

#----
library(ggplot2)
g = ggplot(segment) +
  geom_segment(aes(cos(2 * pi * Start), sin(2 * pi * Start),
                   xend = cos(2 * pi * End), yend = sin(2 * pi * End))) +
  geom_path(aes(cos(seq(0, 2 * pi, length.out = 100)), sin(seq(0, 2 * pi, length.out = 100))),
            colour = 'royalblue') + 
  coord_equal()
print(g)

#----
g = ggplot(shapes) +
  geom_polygon(aes(x, y, group = Label)) +
  coord_equal() + 
  theme_minimal()
print(g)

