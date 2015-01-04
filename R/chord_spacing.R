#' Function for spacing chords along the arc of a circle.
#' @param x Numeric vector. Size of element in x corresponds to the width of the
#'   corresponding chord.
#' @param id Character or factor vector indicating how values of x are grouped.
#'   Most notably used to place gaps. Should be arranged so that groups appear
#'   in sequence.
#' @param direction Either 'out' or 'in'.
#' @param gap_width Space between outer segments of chord diagram default is 
#'   0.005
#' @param ROI Rescaling of input or output hemispheres. Default is 1 (both input
#'   and output hemispheres)
respace = function(x, id, direction, gap_width = 0.005, ROI = 1){
  mx = min(x)
  x = (x - mx) / (max(x) - mx) * (1 - gap_width * length(unique(id)))
  gaps = cumsum(c(0.5, diff(as.numeric(factor(id, levels = unique(id)))))) * gap_width
  if (direction == 'out'){
    x = (x + gaps - 0.5) * (1 / (ROI + 1)) + 0.75
  } else if (direction == 'in'){
    x = -(x + gaps - 0.5) * (ROI / (ROI + 1)) + 0.25
  }
  x
}




outer_spacing = function(data, gap_width = 0.005, ROI = 1){
  offset = 0.75
  from_segment = data %>%
    arrange(FROM, TO) %>%
    rename(Label = FROM) %>%
    group_by(Label) %>%
    summarize(Size = sum(OUT)) %>%
    mutate(Direction = 'Out',
           nEnd = cumsum(Size) / sum(Size),
           nStart = c(0, nEnd[1:(n() - 1)]),
           compress = 1 - gap_width * n() / (1 / (ROI + 1)),
           gaps = (gap_width / 2 + (gap_width * 0:(n() - 1))) / (1 / (ROI + 1)),
           Start = (nStart * compress + gaps - 0.5) * (1 / (ROI + 1)) + offset,
           End = (nEnd * compress + gaps - 0.5) * (1 / (ROI + 1)) + offset) %>%
    select(Label, Direction, Start, End)
  
  offset = 0.25
  to_segment = data %>%
    arrange(FROM, TO) %>%
    rename(Label = TO) %>%
    group_by(Label) %>%
    summarize(Size = sum(IN)) %>%
    mutate(Direction = 'In',
           nEnd = cumsum(Size) / sum(Size),
           nStart = c(0, nEnd[1:(n() - 1)]),
           compress = 1 - gap_width * n() / (ROI / (ROI + 1)),
           gaps = (gap_width / 2 + (gap_width * 0:(n() - 1))) / (ROI / (ROI + 1)),
           Start = -(nStart * compress + gaps - 0.5) * (ROI / (ROI + 1)) + offset,
           End = -(nEnd * compress + gaps - 0.5) * (ROI / (ROI + 1)) + offset) %>%
    select(Label, Direction, Start, End)
  
  rbind(from_segment, to_segment)
}