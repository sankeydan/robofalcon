turnang = function ( d){
  head = get_heading(d[,1],d[,2])
  turn.angle = diff(head)
  turn.angle = ifelse ( turn.angle < -pi, abs(abs(turn.angle)- 2*pi ),turn.angle)
  turn.angle = ifelse ( turn.angle >  pi,-abs(abs(turn.angle)- 2*pi ),turn.angle)
  return(c(turn.angle,NA))
}
