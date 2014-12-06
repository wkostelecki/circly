remove(list = ls())

NROW = 1
NCOL = 20

#----
data = data.frame(FROM_AMOUNT = 1:(NROW * NCOL),
                  TO_AMOUNT = (NROW * NCOL):1*2,
                  FROM_LABEL = rep(letters[1:NCOL], each = NROW),
                  TO_LABEL = rep(LETTERS[1:NROW], NCOL))
from_label = 'FROM_LABEL'
to_label = 'TO_LABEL'
from_amount = 'FROM_AMOUNT'
to_amount = 'TO_AMOUNT'


shapes = chord_shapes(data, from_amount, to_amount, from_label, to_label)

#----
g = ggplot() +
  geom_polygon(data = shapes$outer_segments,
               aes(x, y, group = ID, fill = ID)) +
  coord_equal() +
  theme_minimal()
print(g)