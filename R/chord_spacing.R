chord_respacing = function(x, id, direction, gap_width = 0.005, ROI = 1){
  x = (x - min(x)) / (max(x) - min(x)) * (1 - gap_width * length(unique(id)))
  gaps = cumsum(c(0.5, diff(as.numeric(factor(id, levels = unique(id))))))
  if (direction == 'out'){
    x = (x + gaps - 0.5) * (1 / (ROI + 1)) + 0.75
  } else if (direction == 'in'){
    x = -(x + gaps - 0.5) * (ROI / (ROI + 1)) + 0.25
  }

}


x = c(0, sort(runif(8)), 1)
id = c(rep(c('a', 'b', 'c'), each = 3), 'c')

chord_respacing(x, id)
