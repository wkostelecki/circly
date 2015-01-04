#' @title chord_segment
#' @description Creates x,y boundaries for chord segments of the chord diagram 
#'   circle. \cr start and end arguments are in the interval [0, 1] and are
#'   mapped to [0, 2 * pi]
#' @param out_start point on circle (value in [0, 1])
#' @param out_end point on circle (value in [0, 1])
#' @param in_start point on circle (value in [0, 1])
#' @param in_end point on circle (value in [0, 1])
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

#' @import dplyr
chord_segments = function(data, segment_spaces = NULL){
  
  if (is.null(segment_spaces)){
    segment_spaces = outer_spacing(data)
  }
  
  out = data.frame(FROM = data$FROM,
                   TO = data$TO,
                   OUT_START = NA,
                   OUT_END = NA,
                   IN_START = NA,
                   IN_END = NA)
  
  for (i in 1:nrow(segment_spaces)){
    
    this_segment = segment_spaces[i, ]
    
    
    
    if (this_segment$Direction == 'In'){
      ind = which(data$TO == as.character(this_segment$Label))
      this_seq = subset(data, TO == as.character(this_segment$Label),
                        select = 'IN', drop = TRUE)
    } else if(this_segment$Direction == 'Out'){
      ind = which(data$FROM == as.character(this_segment$Label))
      this_seq = subset(data, FROM == as.character(this_segment$Label),
                        select = 'OUT', drop = TRUE)
    }
    
    size = with(this_segment, End - Start)
    n_seq = c(0, cumsum(this_seq) / sum(this_seq)) * size + this_segment$Start
    
    if (this_segment$Direction == 'In'){
      out$IN_START[ind] = n_seq[1:(length(n_seq) - 1)]
      out$IN_END[ind] = n_seq[2:length(n_seq)]
    } else if(this_segment$Direction == 'Out'){
      out$OUT_END[ind] = n_seq[1:(length(n_seq) - 1)]
      out$OUT_START[ind] = n_seq[2:length(n_seq)]
    }
    
  }
  
  #----
  OUT = vector('list', nrow(out))
  for (i in 1:nrow(data)){
    OUT[[i]] = with(out, chord_segment(OUT_START[i], OUT_END[i],
                                       IN_START[i], IN_END[i])) %>%
      mutate(FROM = as.character(data$FROM[i]),
             TO = as.character(data$TO[i]))
  }
  
  OUT = rbind_all(OUT) %>%
    mutate(Chord = paste(FROM, TO, sep = ':'))
}