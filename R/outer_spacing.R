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