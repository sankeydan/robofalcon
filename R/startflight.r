startflight = function(vec,
                       lag = 100,
                       min = 5,
                       dur = 100,
                       plot = T,
                       outputrunningmean = T){

  run = runningmean(vec,lag)
  vecT = run > min
  co = vector()
  i = 1
  while(length(co)<dur & i < length(run)){
    if(!is.na(run[i])){
      if( vecT[i] == T){
        co = c(co,1)
        fin = i
      } else {
        co = vector()
      } }
    i = i+1
  }
  i = i-dur
  if( plot){
  plot(run)
  abline(v = i)
  }
  if ( outputrunningmean){
    return( list(i,run))
  }
  return(i)
}
