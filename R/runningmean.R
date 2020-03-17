runningmean = function ( x , lag, method = "normal"){

  #x = pressure
  #lag = 200
  # method = "circular"

  # objects
  len = length(x)
  sq = (lag+1):(len-lag)

  # stopifnot
  if( (2*lag) > len){
    stop( "lag is too large")
  }

  vec = rep(NA, len)
  for ( i in sq){
    if ( method == "circular"){
      vec[i] = suppressWarnings( as.numeric(
        circular::mean.circular( x[(i-lag):(i+lag)] , na.rm = T )))
    } else{
    vec[i] = mean(x[(i-lag):(i+lag)] , na.rm = T )
    }
  }
  # for ( i in lag:1){
  #
  #   # i = 2
  #   vec[i] = mean(x[((i-i)+1):(i+(i-1))],na.rm = T)
  #   vec[length(vec)-i] = mean( x[((len-i-i)+1):((len-i)+(i-1))],na.rm = T)
  # }

  return(   vec )
}
