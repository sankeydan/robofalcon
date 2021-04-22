ang.diff = function(x,y,fuN = NULL){
  sqrt(((cos(x)+cos(y))/2)^2 +
         ((sin(x)+sin(y))/2)^2)
  if ( fuN == "turning"){
    diff = y-x
    diff = ifelse ( diff > pi, diff-2*pi,diff)
    diff = ifelse ( diff < -pi,diff+2*pi,diff)
    return(diff)
  }
}

