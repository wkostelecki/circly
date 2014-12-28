library(circly)
library(ggplot2)
remove(list = ls())

#----
data = circly:::sample_data(NROW = 5, NCOL = 5)

shapes = chord_shapes(data)



#----

g = ggplot() +
  geom_polygon(data = shapes$outer_segments,
               aes(x, y, group = ID, fill = ID)) +
  coord_equal() +
  theme_minimal()
print(g)