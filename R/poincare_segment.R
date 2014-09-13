poincare_segment <- function(u1, u2, v1, v2) {
   # Check that the points are sufficiently different
   if( abs(u1-v1) < 1e-6 && abs(u2-v2) < 1e-6 )
      return( list(x=c(u1,v1), y=c(u2,v2)) )
   # Check that we are in the circle
   stopifnot( u1^2 + u2^2 - 1 <= 1e-6 )
   stopifnot( v1^2 + v2^2 - 1 <= 1e-6 )
   # Check it is not a diameter
   if( abs( u1*v2 - u2*v1 ) < 1e-6 )
      return( list(x=c(u1,v1), y=c(u2,v2)) )
   # Equation of the line: x^2 + y^2 + ax + by + 1 = 0 (circles orthogonal to the unit circle)
   a <- ( u2 * (v1^2+v2^2) - v2 * (u1^2+u2^2) + u2 - v2 ) / ( u1*v2 - u2*v1 )
   b <- ( u1 * (v1^2+v2^2) - v1 * (u1^2+u2^2) + u1 - v1 ) / ( u2*v1 - u1*v2 ) # Swap 1's and 2's
   # Center and radius of the circle
   cx <- -a/2
   cy <- -b/2
   radius <- sqrt( (a^2+b^2)/4 - 1 )
   # Which portion of the circle should we draw?
   theta1 <- atan2( u2-cy, u1-cx )
   theta2 <- atan2( v2-cy, v1-cx )
   if( theta2 - theta1 > pi )
      theta2 <- theta2 - 2 * pi
   else if( theta2 - theta1 < - pi )
      theta2 <- theta2 + 2 * pi
   theta <- seq( theta1, theta2, length=100 )
   x <- cx + radius * cos( theta )
   y <- cy + radius * sin( theta )
   list( x=x, y=y )
}