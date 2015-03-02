#----
library(dplyr)
library(ggplot2)
remove(list = ls())

#----
NROW = 5
NCOL = 5

source('./R/sample_data.R')
data = sample_data(NROW = NROW, NCOL = NCOL)

# SHAPES = chord_shapes(data)

#----
source('./R/chord_spacing.R')
segment_spaces = outer_spacing(data)
# chord_spaces = chord_spacing(data, segment_spaces)


#----
source('./R/outer_segment.R')
outer_shapes = outer_segments(segment_spaces)

source('./R/chord_segment.R')
chord_shapes = chord_segments(data)

#----
library(ggplot2)
g = ggplot(segment_spaces) +
  geom_segment(aes(cos(2 * pi * Start), sin(2 * pi * Start),
                   xend = cos(2 * pi * End), yend = sin(2 * pi * End))) +
  geom_path(aes(cos(seq(0, 2 * pi, length.out = 100)), sin(seq(0, 2 * pi, length.out = 100))),
            colour = 'royalblue') + 
  coord_equal() +
  theme_minimal()
print(g)

#----
g = ggplot(outer_shapes) +
  geom_polygon(aes(x, y, group = Label, fill = Label)) +
  geom_polygon(data = chord_shapes, aes(x, y, group = Chord, fill = FROM), alpha = 0.5) + 
  scale_fill_manual(guide = guide_legend(nrow = 3),
                    values = plot3D::jet.col(NROW + NCOL)) +
  coord_equal() + 
  theme_minimal() +
  theme(legend.position = 'top')
print(g)

