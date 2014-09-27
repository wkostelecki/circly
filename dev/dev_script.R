remove(list = ls())
library(circly)
library(circlize)
# source('./circly.R')

NCOL = 2
NROW = 2
# set.seed(1)
M1 = matrix(runif(NCOL * NROW), ncol = NCOL)
M2 = matrix(runif(NCOL * NROW), ncol = NCOL)

# M1 = matrix(1:(NCOL * NROW), ncol = NCOL)
# M2 = matrix((NCOL * NROW):1, ncol = NCOL)

M1 = matrix(sample((1:(NCOL * NROW)) ^ 2), ncol = NCOL)
M2 = matrix(sample(((NCOL * NROW):1) ^ 2), ncol = NCOL)

# colnames = sapply(1:NCOL, function(x) paste0(sample(LETTERS, 8), collapse = ''))
colnames = LETTERS[1:NCOL]
# colnames[2] = 'B\nBC'
rownames = letters[1:NROW]
# rownames[2] = 'safasdf\nsadfsadg'

colnames(M1) = colnames
colnames(M2) = colnames
rownames(M1) = rownames
rownames(M2) = rownames

circly(M1, M2)

#chordDiagram(t(M1)[ncol(M1):1, , drop = F], transparency = 0.5)

# M1[5, 1] = 4



# for(from in 1:NCOL) {
#    col = paste0(colcol[from], '80')
#    for(to in 1:NROW) {
#       if(TRUE) {
#          i = from * NROW + 1 - to
#          j = (to - 1) * ncol(M1) + from
#          k = 0
#          u <- c(0.5, 0.5 + cumsum(sapply(1:NCOL, function(x) rev(M1[, x]))) * 0.5)[i]
#          v <- c(0.5, 0.5 + cumsum(sapply(1:NCOL, function(x) rev(M1[, x]))) * 0.5)[i + 1]
#          x <- c(0, 0 + matrix(cumsum(t(M2)), ncol = NROW) * 0.5)[j]
#          y <- c(0, 0 + matrix(cumsum(t(M2)), ncol = NROW) * 0.5)[j + 1]
#          if(!is.na(u*v*x*y)) {
#             r1 <- poincare_segment( cos(2*pi*v), sin(2*pi*v), cos(2*pi*x), sin(2*pi*x) )
#             r2 <- poincare_segment( cos(2*pi*y), sin(2*pi*y), cos(2*pi*u), sin(2*pi*u) )
#             th1 <- 2*pi*seq(u,v,length=20)
#             th2 <- 2*pi*seq(x,y,length=20)
#             polygon(
#                c( cos(th1), r1$x, rev(cos(th2)), r2$x ),
#                c( sin(th1), r1$y, rev(sin(th2)), r2$y ),
#                col = col, border=NA
#             )
#          }
#       }
#    }
# }