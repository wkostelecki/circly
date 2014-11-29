remove(list = ls())
library(circly)
library(circlize)
# source('./circly.R')

NCOL = 5
NROW = 3
# set.seed(5)
# M1 = matrix(runif(NCOL * NROW), ncol = NCOL)
# M2 = matrix(runif(NCOL * NROW), ncol = NCOL)


options(debug_mode = FALSE)
# options(debug_mode = TRUE)


M1 = matrix(1:(NCOL * NROW), ncol = NCOL)
M2 = matrix((NCOL * NROW):1, ncol = NCOL)

# M1 = matrix(sample((1:(NCOL * NROW)) ^ 2), ncol = NCOL)
# M2 = matrix(sample(((NCOL * NROW):1) ^ 2), ncol = NCOL)

# colnames = sapply(1:NCOL, function(x) paste0(sample(LETTERS, 8), collapse = ''))

# colnames = LETTERS[1:NCOL]
# colnames[2] = 'B\nBC'
# rownames = letters[1:NROW]
colnames = LETTERS[1:NCOL]
# colnames[2] = 'B\nBC'
rownames = letters[1:NROW]
# rownames[2] = 'safasdf\nsadfsadg'

colnames(M1) = colnames
colnames(M2) = colnames
rownames(M1) = rownames
rownames(M2) = rownames

color_column = rgb(c(1,0,1,0,0), c(0,1,0,0,1), c(1,1,0,1,0))
color_row = rgb(c(0,0,0), c(0.5,0.5,0.5), c(0,0,0))

# png('./tmp.png', height = 1000, width = 1000)
circly(M1, M2, gap_width = 0.01, ROI = 1.5, color_column = color_column, color_row = color_row)
# dev.off()

# png('./tmp.png', height = 1000, width = 1000)
# circly(M1, M2, color_column = rgb(c(1,0,0,0.5), c(0,1,0,0), c(0,0,1,0.5)),
#        color_row = rgb(c(0.5,0,0.2,0.8), c(0.5,0.5,0.8,0.2), c(0,0.5,0.2,0.2)))
# dev.off()

#chordDiagram(t(M1)[ncol(M1):1, , drop = F], transparency = 0.5)

# M1[5, 1] = 4
