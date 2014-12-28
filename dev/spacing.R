remove(list = ls())

source('./R/chord_spacing.R')

x = c(0, sort(runif(8)), 1)
x = 0:9 / 9
x = cumsum(rep(1, 10))
id = c(rep(c('a', 'b', 'c'), each = 3), 'c')

y = respace(x, id, direction = 'out')
print(y)


context('spacing')