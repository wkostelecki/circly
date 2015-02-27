
#  ------------------------------------------------------------------------

v1 = 0.999
x1 = cos(2 * pi * v1)
y1 = sin(2 * pi * v1)


v2 = 0.25
x2 = cos(2 * pi * v2)
y2 = sin(2 * pi * v2)


data = circly:::poincare_segment(x1, y1, x2, y2)

ggplot(as.data.frame(data)) + geom_line(aes(x,y)) + coord_equal() + scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-1, 1))
