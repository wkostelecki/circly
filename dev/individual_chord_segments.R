#----
chords = circly:::chord_segment(0.6, 0.7, 0.2, 0.4)
ggplot(chords) + geom_polygon(aes(x, y)) + coord_equal() + ylim(1.1 * c(-1, 1)) + xlim(1.1 * c(-1, 1))
