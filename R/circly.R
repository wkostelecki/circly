circly = function(M1, M2 = M1,
                  color_column = rep(rgb(0, 0, 0), ncol(M1)),
                  color_row = rep(rgb(0, 0, 0), nrow(M1)),
                  gap_width = 0.005,
                  cex = 1){
   # creates chord diagram from two matrix inputs
   
   NCOL = ncol(M1)
   NROW = nrow(M1)
   COLNAMES = colnames(M1)
   ROWNAMES = rownames(M1)
   
   if (!all(dim(M1) == dim(M2))){
      stop('M1 and M2 must have matching dimensions')
   }
   
   if (!all(COLNAMES == colnames(M2)) || !all(ROWNAMES == rownames(M2))){
      warning('Row and column names of M2 being replaced with those from M1')
      colnames(M2) = colnames(M1)
      rownames(M2) = rownames(M1)
   }
   
   M1 = M1 / sum(M1)
   out_end = cumsum(apply(M1, 2, sum)) * 0.5
   out_start = c(0, out_end[1:(length(out_end) - 1)])[1:length(out_end)]
   
   M2 = M2 / sum(M2)
   in_end = cumsum(apply(M2, 1, sum)) * 0.5
   in_start = c(0, in_end[1:(length(in_end) - 1)])[1:length(in_end)]
   
   out_start = out_start * (1 - 2 * gap_width * length(out_start))
   out_end = out_end * (1 - 2 * gap_width * length(out_start))
   out_start = out_start + gap_width / 2
   out_end = out_end + gap_width / 2
   out_start = out_start + gap_width * 0:(length(out_start) - 1) + 0.5
   out_end = out_end + gap_width * 0:(length(out_start) - 1) + 0.5
   
   in_start = in_start * (1 - 2 * gap_width * length(in_start))
   in_end = in_end * (1 - 2 * gap_width * length(in_start))
   in_start = in_start + gap_width / 2
   in_end = in_end + gap_width / 2
   in_start = in_start + gap_width * 0:(length(in_start) - 1)
   in_end = in_end + gap_width * 0:(length(in_start) - 1)
   
   in_start = - in_start + 0.5
   in_end = -in_end + 0.5
   
   par(mar = c(1, 1, 2, 1))
   par(pty = 's')
   plot(0, 0, 
        xlim = c(-1.2, 1.2),
        ylim = c(-1.2, 1.2),
        type = "n",
        axes = FALSE, 
        xlab = "",
        ylab = "")
   
   colcol = color_column
   
   for(i in 1:length(out_start)) {
      theta <- 2*pi*seq(out_start[i], out_end[i], length = 100)
      thetaText = pi * (out_start[i] + out_end[i])
      text(1.22 * cos(thetaText),
           1.25 * sin(thetaText),
           COLNAMES[i],
           srt = 270 - (pi - thetaText) * 180 / pi,
           cex = cex)
      r1 <- 1.03
      r2 <- 1.1
      polygon(
         c( r1*cos(theta), rev(r2*cos(theta)) ),
         c( r1*sin(theta), rev(r2*sin(theta)) ),
         col= colcol[i], border = NA
      )
   }
   
   for(i in 1:length(in_start)) {
      theta <- 2*pi*seq(in_start[i], in_end[i], length = 100)
      thetaText = pi * (in_start[i] + in_end[i])
      text(1.22 * cos(thetaText),
           1.25 * sin(thetaText),
           ROWNAMES[i],
           srt = 90 - (pi - thetaText) * 180 / pi,
           cex = cex)
      r1 <- 1.03
      r2 <- 1.1
      polygon(
         c( r1*cos(theta), rev(r2*cos(theta)) ),
         c( r1*sin(theta), rev(r2*sin(theta)) ),
         col= color_row[i], border = NA
      )
   }
   
   
   for(i in 1:NCOL) {
      col = paste0(colcol[i], '80') # add transparency
      for(j in 1:NROW) {
         p1 = sum(c(0, M1[, i])[1:j]) / sum(M1[, i])
         p2 = sum(M1[, i][1:j]) / sum(M1[, i])
         u = out_start[i] + (out_end[i] - out_start[i]) * p1
         v = out_start[i] + (out_end[i] - out_start[i]) * p2
         
         k = NROW + 1 - j
         k = j
         p1 = sum(c(0, M2[k, ])[1:i]) / sum(M2[k, ])
         p2 = sum(M2[k, ][1:i]) / sum(M2[k, ])
         x = in_end[k] - (in_end[k] - in_start[k]) * p1
         y = in_end[k] - (in_end[k] - in_start[k]) * p2
         
         if(!is.na(u*v*x*y)) {
            r1 <- poincare_segment( cos(2*pi*v), sin(2*pi*v), cos(2*pi*x), sin(2*pi*x) )
            r2 <- poincare_segment( cos(2*pi*y), sin(2*pi*y), cos(2*pi*u), sin(2*pi*u) )
            th1 <- 2*pi*seq(u,v,length=20)
            th2 <- 2*pi*seq(x,y,length=20)
            polygon(
               c( cos(th1), r1$x, rev(cos(th2)), r2$x ),
               c( sin(th1), r1$y, rev(sin(th2)), r2$y ),
               col = col, border=NA
            )
         }
      }
   }
}