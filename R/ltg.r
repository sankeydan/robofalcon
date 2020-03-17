ltg = function(V1,R1,r,sinmeth=T){
  sq = c(0,0.001)
  Ti = 0.2
  while ( sq[length(sq)]-sq[length(sq)-1] > 0){
    t = (Ti * V1)/R1
    obj =ifelse( sinmeth,1-cos(t),(r - r * cos( v*(t/r) ) ))
    sq = c(sq,obj)
    Ti = Ti+0.2
  }
  return(length(sq)-2)
}
