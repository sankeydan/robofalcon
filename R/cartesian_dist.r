cartesian_dist <- function(x1, y1, x2, y2, hz, method = c("distance","speed" )) {
  # x1 = rep(4,4)
  # x2 = 1:4
  # y1 = 20:23
  # y2 = 3:6
  # hz = 10
  # method = "distance"
  
  if ( method[1] == "speed"){
   x2 = x1[2:length(x1)]
   y2 = y1[2:length(y1)]
   x1 = x1[1:(length(x1)-1)]
   y1 = y1[1:(length(y1)-1)]
  }
  
  dx = abs( x1 - x2)
  dy = abs( y1 - y2)
  
  if ( method[1] == "speed"){
  return( c(NA, sqrt( dy^2 + dx^2) *hz))
  } else {
    return ( sqrt( dy^2 + dx^2))
  }
  
}