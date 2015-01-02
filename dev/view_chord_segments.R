


out_start = 0
out_end = 0.2
in_start = 0.75
in_end = 0.85

source('./R/chord_segment.R')

data = chord_segment(out_start, out_end, in_start, in_end)


#----
library(ggplot2)
data2 = chord_segment(out_start, 0.35, 0.75, 0.7)
g = ggplot(data2) +
  geom_polygon(aes(x, y), fill = 'red') +
  coord_equal() +
  theme_minimal() +
  ylim(c(-1.1, 1.1)) +
  xlim(c(-1.1, 1.1))

print(g)

# data$z = 1:nrow(data)
# ggplot(data) +
#   geom_point(aes(x, y, size = z)) +
#   coord_equal() +
#   theme_minimal() + ylim(c(-1,1)) +
#   xlim(c(-1,1))

