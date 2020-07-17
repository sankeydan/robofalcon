get_heading_cartesian = function ( x1 , y1 , x2 = NULL, y2 = NULL, indivs = 1){
  if ( indivs == 1){
    x2 = x1[2:length(x1)]
    y2 = y1[2:length(y1)]
    x1 = x1[1:(length(x1)-1)]
    y1 = y1[1:(length(y1)-1)]
  }
  
  dx = x2 - x1
  dy = y2 - y1
  return ( atan2(dx,dy) ) 

}