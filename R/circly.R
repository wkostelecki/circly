#' @export
circly = function (M1,
                   M2 = M1,
                   color_column = rep(rgb(0, 0, 0), ncol(M1)),
                   color_row = rep(rgb(0, 0, 0), nrow(M1)),
                   gap_width = 0.005,
                   cex = 1,
                   label_orientation = 'radial',
                   ROI = 1)
{
  
  #if (getOption('debug_mode')){
  #  browser()
  #}
  
  NCOL = ncol(M1)
  NROW = nrow(M1)
  NSEGMENT = NCOL + NROW
  
  COLNAMES = colnames(M1)
  ROWNAMES = rownames(M1)
  
  if (!all(dim(M1) == dim(M2))) {
    stop("M1 and M2 must have matching dimensions")
  }
  
  if (!identical(COLNAMES, colnames(M2)) || !identical(ROWNAMES, rownames(M2))) {
    warning("Row and column names of M2 assumed to match those from M1")
    colnames(M2) = colnames(M1)
    rownames(M2) = rownames(M1)
  }
  
  gap_total = pmin(gap_width * (NSEGMENT), 0.9)
  gap_width = gap_total / (NSEGMENT)
  
  M1 = M1 / sum(M1)
  out_end = cumsum(apply(M1, 2, sum))
  out_start = c(0, out_end[1:(length(out_end) - 1)])
  
  M2 = M2 / sum(M2)
  in_end = cumsum(apply(M2, 1, sum))
  in_start = c(0, in_end[1:(length(in_end) - 1)])
  
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

  par(mar = c(1, 1, 2, 1), pty = "s")
  extent = 1.6
  
  plot(0, 0, xlim = c(-1, 1) * extent, ylim = c(-1, 1) * extent,
       type = "n", axes = FALSE, xlab = "", ylab = "")
  
  RR = 1.3
  
  # Place chords in random order
  ORDER = expand.grid(i = 1:NCOL,
                      j = 1:NROW)
  set.seed(1)
  ORDER = ORDER[sample(nrow(ORDER)), ]
  for (k in 1:nrow(ORDER)) {
    
    i = ORDER$i[k]
    j = ORDER$j[k]
    
    if (M1[j, i] == 0 | M2[j, i] == 0){
      next
    }
    
    col = paste0(color_column[i], "80")
    p1 = sum(c(0, M1[, i])[1:j]) / sum(M1[, i])
    p2 = sum(M1[, i][1:j]) / sum(M1[, i])
    u = out_start[i] + (out_end[i] - out_start[i]) *
      p1
    v = out_start[i] + (out_end[i] - out_start[i]) *
      p2
    k = NROW + 1 - j
    k = j
    p1 = sum(c(0, M2[k, ])[1:i])/sum(M2[k, ])
    p2 = sum(M2[k, ][1:i])/sum(M2[k, ])
    x = in_end[k] - (in_end[k] - in_start[k]) * p1
    y = in_end[k] - (in_end[k] - in_start[k]) * p2
    if (!is.na(u * v * x * y)) {
      r1 <- poincare_segment(cos(2 * pi * v), sin(2 *
                                                    pi * v), cos(2 * pi * x), sin(2 * pi * x))
      r2 <- poincare_segment(cos(2 * pi * y), sin(2 *
                                                    pi * y), cos(2 * pi * u), sin(2 * pi * u))
      th1 <- 2 * pi * seq(u, v, length = 20)
      th2 <- 2 * pi * seq(x, y, length = 20)
      
      pathx = c(cos(th1), r1$x, (cos(th2)), r2$x)
      pathy = c(sin(th1), r1$y, (sin(th2)), r2$y)
      
      polygon(pathx, pathy,
              col = col,
              border = NA)
      points(pathx, pathy,
             col = paste0(color_column[i], "20"),
             type = 'l')
      
    }
  }
  
  # bottoms segments ----
  for (i in 1:length(out_start)) {
    theta <- 2 * pi * seq(out_start[i], out_end[i], length = 100)
    
    r1 = 1.03
    r2 = 1.1
    polygon(c(r1 * cos(theta), rev(r2 * cos(theta))), c(r1 *
                                                          sin(theta),
                                                        rev(r2 * sin(theta))),
            col = color_column[i],
            border = color_column[i])
    
    thetaText = pi * (out_start[i] + out_end[i])
    if (label_orientation == 'radial'){
      if (thetaText > 3 * pi / 2){
        text(RR * cos(thetaText), RR * sin(thetaText), COLNAMES[i],
             srt = 270 - (pi - thetaText) * 180/pi - 90, cex = cex, adj = c(0, 0.5))
      } else {
        text(RR * cos(thetaText), RR * sin(thetaText), COLNAMES[i],
             srt = 270 - (pi - thetaText) * 180/pi + 90, cex = cex, adj = c(1, 0.5))
      }
    } else if (label_orientation == 'horizontal'){
      text(RR * cos(thetaText), RR * sin(thetaText), COLNAMES[i],
           cex = cex)
    }
  }
  
  # top segments ----
  for (i in 1:length(in_start)) {
    theta <- 2 * pi * seq(in_start[i], in_end[i], length = 100)
    r1 <- 1.03
    r2 <- 1.1
    polygon(c(r1 * cos(theta), rev(r2 * cos(theta))), c(r1 *
                                                          sin(theta), rev(r2 * sin(theta))), col = color_row[i],
            border = color_row[i])
    thetaText = pi * (in_start[i] + in_end[i])
    if (label_orientation == 'radial'){
      text_angle = 90 - (pi - thetaText) * 180/pi
      if (thetaText > pi / 2) {
        text(RR * cos(thetaText), RR * sin(thetaText), ROWNAMES[i],
             srt = text_angle - 90, cex = cex, adj = c(1, 0.5))
      } else {
        text(RR * cos(thetaText), RR * sin(thetaText), ROWNAMES[i],
             srt = text_angle + 90, cex = cex, adj = c(0, 0.5))
        
      }
    } else if(label_orientation == 'horizontal'){
      text(RR * cos(thetaText), RR * sin(thetaText), ROWNAMES[i],
           cex = cex)
    }
  }
  
}
