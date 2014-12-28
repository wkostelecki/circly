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
inner_r = 1.03
outer_r = 1.1

outer_segments = with(segment, lapply(
  1:length(Start),
  function(i){
    outer_segment(Start[i], End[i], inner_r, outer_r) %>%
      mutate(Direction = segment$Direction[i],
             Label = as.character(segment$Label[i]))
  }
)) %>%
  do.call(rbind, .)

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
ggplot(outer_segments) +
  geom_polygon(aes(x, y, group = Label)) +
  coord_equal() + 
  theme_minimal()

#----
source('./R/outer_segment.R')
out = lapply(1:nrow(segment), function(x, ...) {
  print(segment$Label[x])
  with(segment, outer_segment(Start[x], End[x], 1, 1.1))
},
segment)
