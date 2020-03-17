rv = function( den, a, b ){
  rand = runif( 1,0,1)
  sp = sample(den$x[den$y2 > rand],1)
  return( c( r = sp / (a * exp(-b*sp)),
             v = sp))
}
