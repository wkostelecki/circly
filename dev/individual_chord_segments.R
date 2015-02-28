library(ggplot2)
#----
chords = circly:::chord_segment(0.6, 0.601, 0.2, 0.2)

g = ggplot(chords) +
  geom_polygon(aes(x, y), fill = 'royalblue', alpha = 0.5) +
  geom_path(aes(x, y), colour = 'royalblue', alpha = 0.5) +
  coord_equal() +
  ylim(1.1 * c(-1, 1)) +
  xlim(1.1 * c(-1, 1)) + 
  theme_minimal()
print(g)