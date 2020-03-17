abs.turn = function( data, hz = 5, abs = T){
  turn = get_heading(data[,"lon"],data[,"lat"])
  turn = (c(diff(turn),NA))
  turn = ifelse( turn >  pi,turn-(2*pi),turn)
  turn = ifelse( turn < -pi,turn+(2*pi),turn)
  if ( abs){
    turn = abs(turn)/(1/hz)
  } else {
    turn =     turn/(1/hz)
  }
  return(turn)
}
