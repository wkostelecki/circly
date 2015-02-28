
#  ------------------------------------------------------------------------

context('poincare')


test_that('core function works', {
  v1 = 0
  x1 = cos(2 * pi * v1)
  y1 = sin(2 * pi * v1)
  v2 = 0.51
  x2 = cos(2 * pi * v2)
  y2 = sin(2 * pi * v2)
  data = circly:::poincare_segment(x1, y1, x2, y2)
  expect_equal(length(data$x), 100)
  expect_equal(length(data$y), 100)
  
})


test_that('special case works', {
  v1 = 0
  x1 = cos(2 * pi * v1)
  y1 = sin(2 * pi * v1)
  
  
  v2 = 0.5
  x2 = cos(2 * pi * v2)
  y2 = sin(2 * pi * v2)
  
  
  data = circly:::poincare_segment(x1, y1, x2, y2)
  
  expect_equal(data$x, c(1, -1))
  expect_equal(data$y, c(0, 0))
  
})


test_that('wrapper functions work', {
  v1 = 0
  v2 = 0.25
  data = circly:::unit_poincare(v1, v2)
  expect_equal(with(data, sqrt((x - 1) ^ 2 + (y - 1) ^ 2)),
               rep(1, length(data$x)))
  
  v1 = 0.75
  v2 = 0.5
  data = circly:::unit_poincare(v1, v2)
  expect_equal(with(data, sqrt((x + 1) ^ 2 + (y + 1) ^ 2)),
               rep(1, length(data$x)))
  
})


# ggplot(as.data.frame(data)) + geom_line(aes(x,y)) + coord_equal() + scale_y_continuous(limits = c(-1, 1)) +
#   scale_x_continuous(limits = c(-1, 1))
