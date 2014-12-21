
chord_segment = function(out_start, out_end, in_start, in_end){
  
  out_theta <- 2 * pi * seq(out_start, out_end, length = 100)
  in_theta <- 2 * pi * seq(in_start, in_end, length = 100)
  
  cos_out_theta = cos(out_theta)
  sin_out_theta = sin(out_theta)
  
  cos_in_theta = cos(in_theta)
  sin_in_theta = sin(in_theta)
  
  r1 <- circly:::poincare_segment(cos(2 * pi * out_end),
                                  sin(2 * pi * out_end),
                                  cos(2 * pi * in_start),
                                  sin(2 * pi * in_start))
  r2 <- circly:::poincare_segment(cos(2 * pi * in_end),
                                  sin(2 * pi * in_end),
                                  cos(2 * pi * out_start),
                                  sin(2 * pi * out_start))
  
  x = c(cos_out_theta, r1$x, cos_in_theta, r2$x)
  y = c(sin_out_theta, r1$y, sin_in_theta, r2$y)
  
  data.frame(x = x,
             y = y)
  
}