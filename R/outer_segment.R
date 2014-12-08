#' creates x,y boundaries for outer segments of a circle
outer_segment = function(start, end, inner_r, outer_r){
  
  theta <- 2 * pi * seq(start, end, length = 100)
  
  cos_theta = cos(theta)
  sin_theta = sin(theta)
  
  x = c(inner_r * cos_theta, rev(outer_r * cos_theta))
  y = c(inner_r * sin_theta, rev(outer_r * sin_theta))
  
  data.frame(x = x,
             y = y)
  
}