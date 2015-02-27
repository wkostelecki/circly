# http://stackoverflow.com/questions/14599150/chord-diagram-in-r

poincare_segment <- function(x1, y1, x2, y2) {
   # Check that the points are sufficiently different
   if( abs(x1-x2) < 1e-6 && abs(y1-y2) < 1e-6 )
      return( list(x=c(x1,x2), y=c(y1,y2)) )
   # Check that we are in the circle
   stopifnot( x1^2 + y1^2 - 1 <= 1e-6 )
   stopifnot( x2^2 + y2^2 - 1 <= 1e-6 )
   # Check it is not a diameter
   if( abs( x1*y2 - y1*x2 ) < 1e-6 )
      return( list(x=c(x1,x2), y=c(y1,y2)) )
   # Equation of the line: x^2 + y^2 + ax + by + 1 = 0 (circles orthogonal to the unit circle)
   a <- ( y1 * (x2^2+y2^2) - y2 * (x1^2+y1^2) + y1 - y2 ) / ( x1*y2 - y1*x2 )
   b <- ( x1 * (x2^2+y2^2) - x2 * (x1^2+y1^2) + x1 - x2 ) / ( y1*x2 - x1*y2 ) # Swap 1's and 2's
   # Center and radius of the circle
   cx <- -a/2
   cy <- -b/2
   radius <- sqrt( (a^2+b^2)/4 - 1 )
   # Which portion of the circle should we draw?
   theta1 <- atan2( y1-cy, x1-cx )
   theta2 <- atan2( y2-cy, x2-cx )
   if( theta2 - theta1 > pi )
      theta2 <- theta2 - 2 * pi
   else if( theta2 - theta1 < - pi )
      theta2 <- theta2 + 2 * pi
   theta <- seq( theta1, theta2, length=100 )
   x <- cx + radius * cos( theta )
   y <- cy + radius * sin( theta )
   list( x=x, y=y )
}


unit_poincare = function(v1, v2){
  x1 = cos(2 * pi * v1)
  y1 = sin(2 * pi * v1)
  x2 = cos(2 * pi * v2)
  y2 = sin(2 * pi * v2)
    
  cartesian_poincare(x1, y1, x2, y2)
}

cartesian_poincare = function(x1, y1, x2, y2){
  poincare_segment(x1, y1, x2, y2)
}