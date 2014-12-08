#' @import magrittr dplyr
#' @importFrom reshape2 dcast
#' @export
chord_shapes = function(data, from_amount, to_amount, from_label, to_label){
  gap_width = 0.005
  
  #browser()
  #----
  
  M1 = dcast(data,
             as.formula(paste(to_label, '~', from_label)),
             value.var = from_amount) %>%
    subset(select = 2:ncol(.), drop = FALSE) %>%
    as.matrix %>%
    unname
  
  M2 = dcast(data,
             as.formula(paste(to_label, '~', from_label)),
             value.var = to_amount) %>%
    subset(select = 2:ncol(.), drop = FALSE) %>%
    as.matrix %>%
    unname
  
  ROI = sum(M2) / sum(M1)
  
  NCOL = ncol(M1)
  NROW = nrow(M1)
  NSEGMENT = NCOL + NROW
  
  gap_total = pmin(gap_width * (NSEGMENT), 0.9)
  gap_width = gap_total / (NSEGMENT)
  
  M1 = M1 / sum(M1)
  out_end = cumsum(apply(M1, 2, sum))
  out_start = c(0, out_end[setdiff(0:(length(out_end) - 1), 0)])
  
  M2 = M2 / sum(M2)
  in_end = cumsum(apply(M2, 1, sum))
  in_start = c(0, in_end[setdiff(0:(length(in_end) - 1), 0)])
  
  # ----
  compress = 1 - gap_width * length(out_start) / (1 / (ROI + 1))
  gaps = (gap_width / 2 + (gap_width * 0:(length(out_start) - 1))) / (1 / (ROI + 1))
  
  out_start = (out_start * compress + gaps - 0.5) * (1 / (ROI + 1)) + 0.75
  out_end = (out_end * compress + gaps - 0.5) * (1 / (ROI + 1)) + 0.75
  
  # ----
  compress = 1 - gap_width * length(in_start) / (ROI / (ROI + 1))
  gaps = (gap_width / 2 + (gap_width * 0:(length(in_start) - 1))) / (ROI / (ROI + 1))
  
  in_start = -(in_start * compress + gaps - 0.5) * (ROI / (ROI + 1)) + 0.25
  in_end = -(in_end * compress + gaps - 0.5) * (ROI / (ROI + 1)) + 0.25
  # bottoms segments ----
  
#   r1 = 1.03
#   r2 = 1.1
  
  inner_r = 1.03
  outer_r = 1.1
  
#   outer_segments = data.frame()
#   
#   for (i in 1:length(out_start)) {
#     theta <- 2 * pi * seq(out_start[i], out_end[i], length = 100)
#     
#     x = c(r1 * cos(theta), rev(r2 * cos(theta)))
#     y = c(r1 * sin(theta), rev(r2 * sin(theta)))
#     
#     outer_segments = rbind(outer_segments,
#                            data.frame(ID = paste('Out', i),
#                                       x = x,
#                                       y = y))
#   }
#   
#   # top segments ----
#   for (i in 1:length(in_start)) {
#     theta <- 2 * pi * seq(in_start[i], in_end[i], length = 100)
#     
#     x = c(r1 * cos(theta), rev(r2 * cos(theta)))
#     y = c(r1 * sin(theta), rev(r2 * sin(theta)))
#     
#     outer_segments = rbind(outer_segments,
#                            data.frame(ID = paste('In', i),
#                                       x = x,
#                                       y = y))
#     
#   }
  
  start = c(out_start, in_start)
  end = c(out_end, in_end)
  
  IDS = c(paste('OUT', 1:length(out_start)),
          paste('IN', 1:length(in_start)))
  
  
  outer_segments = lapply(1:length(start),
                          function(i){
                            circly:::outer_segment(start[i], end[i], inner_r, outer_r) %>%
                              mutate(ID = IDS[i])
                          }) %>% do.call(rbind, .) %>%
    mutate(ID = factor(ID, IDS))
  
  return(list(outer_segments = outer_segments,
              chords = data.frame()))
  
}