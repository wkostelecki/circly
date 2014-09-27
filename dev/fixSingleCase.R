remove(list = ls())
library(circly)
library(circlize)
# source('./circly.R')

NCOL = 1
NROW = 4
# set.seed(1)
M1 = matrix(runif(NCOL * NROW), ncol = NCOL)
M2 = matrix(runif(NCOL * NROW), ncol = NCOL)

# M1 = matrix(1:(NCOL * NROW), ncol = NCOL)
# M2 = matrix((NCOL * NROW):1, ncol = NCOL)

M1 = matrix(sample((1:(NCOL * NROW)) ^ 2), ncol = NCOL)
M2 = matrix(sample(((NCOL * NROW):1) ^ 2), ncol = NCOL)

colnames = LETTERS[1:NCOL]
# colnames[2] = 'B\nBC'
rownames = letters[1:NROW]

colnames(M1) = colnames
colnames(M2) = colnames
rownames(M1) = rownames
rownames(M2) = rownames

circly(M1, M2)