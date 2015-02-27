

context('Chord segment')

test_that('Chord segment', {
  
  data = circly:::chord_segment(0, 0, 0, 0)
  expect_true(all(data$x == 1))
  expect_true(all(data$y == 0))
  
  data = circly:::chord_segment(0, 0, 0.5, 0.5)
  expect_true(all(abs(data$y) < 1e-15))
  
})


# data = circly:::chord_segment(0,0.25,0.5,0.75)
# 
# ggplot(data) + geom_polygon(aes(x, y)) + coord_equal()
