#' @title outer_segment
#' @description Creates x,y boundaries for outer segments of the chord diagram 
#'   circle. \cr start and end arguments are in the interval [0, 1] and are
#'   mapped to [0, 2 * pi]
#' @param start point on circle (value in [0, 1])
#' @param end point on circle (value in [0, 1])
#' @param inner_r inner radius of segment
#' @param outer_r outer radius of segment
outer_segment = function(start, end, inner_r, outer_r){
  
  theta <- 2 * pi * seq(start, end, length = 100)
  
  cos_theta = cos(theta)
  sin_theta = sin(theta)
  
  x = c(inner_r * cos_theta, rev(outer_r * cos_theta))
  y = c(inner_r * sin_theta, rev(outer_r * sin_theta))
  
  data.frame(x = x,
             y = y)
  
}

#' outer_segments
#' calls outer_segment for a data.frame that lists numerous outer segments
#' @param segment A data.frame with Start, End, Label, and Direction columns
outer_segments = function(segment){
  inner_r = 1.03
  outer_r = 1.1
  
  with(segment, lapply(
    1:length(Start),
    function(i){
      outer_segment(Start[i], End[i], inner_r, outer_r) %>%
        mutate(Direction = segment$Direction[i],
               Label = as.character(segment$Label[i]))
    }
  )) %>%
    do.call(rbind, .)
}