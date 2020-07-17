#' Get pairwise leadership score
#'
#' @export

# leadRship

leadRship= function( p1,
                     p2,
                     window = 11, # for now
                     fission.dist = 100, # for now
                     cor.min = 0, # for now
                     hz = 5,
                     plot.cor=F,
                     loess.method = F, 
                     plot.loess = F,
                     loess.sensitivity = 1000
){
  
  # Objects
  
  # p1 = mcent
  # p2 = mfalc
  # window = 11
  # fission.dist = 50
  # cor.min = 0
  # hz = 1
  # plot.cor = T
  # loess.method = F
  # plot.loess = T
  # loess.sensitivity = 1000
  
  # stop if not
  if ( length(dim(p1)) != 2 ){
    stop( "p1 and p2 must be matrices with x and y columns")
  }
  if ( (window %% 2) == 0){
    stop( "window size must be odd")
  } 
  
  # only output leadership if certain conditions are met
  dist = na.omit(cartesian_dist(p1[,1],p1[,2],
                                p2[,1],p2[,2],method = "distance"))
  if ( length(dist)>0){
    len = length(which ( dist < fission.dist)) /length(dist)
    len = ifelse ( length(len) ==0, 0,len)
  } else {
    len = 0
  }
  
  # transform
  dist = cartesian_dist(p1[,1],p1[,2],
                        p2[,1],p2[,2],method = "distance")
  p1 = p1[which ( dist < fission.dist),]
  p2 = p2[which ( dist < fission.dist),]
  
  # More objects
  len = nrow(p1)
  wn = ceiling(window/2)
  st = wn
  en = len - wn 
  
  # Movement per timestep , dt
  dt1 = matrix(NA, len - 1, 2)
  dt2 = matrix(NA, len - 1, 2)
  dt1[,1] = p1[2:len,1] - p1[1:(len-1),1]
  dt1[,2] = p1[2:len,2] - p1[1:(len-1),2]
  dt2[,1] = p2[2:len,1] - p2[1:(len-1),1]
  dt2[,2] = p2[2:len,2] - p2[1:(len-1),2]
  
  # Normalise
  norm1 = matrix(NA, len-1,2)
  norm2 = matrix(NA, len-1,2)
  norm1[,1] = apply(cbind(dt1[,1], dt1[,2]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
  norm1[,2] = apply(cbind(dt1[,2], dt1[,1]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
  norm2[,1] = apply(cbind(dt2[,1], dt2[,2]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
  norm2[,2] = apply(cbind(dt2[,2], dt2[,1]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
  
  # pairwise correlation
  cors = matrix(NA,len-1, window)
  for ( j in 1:window){
    for ( i in st:en){
      cors[i,j] =  norm1[i,] %*% norm2[(j-wn+i),]
    }
  }
  
  # leader/folower
  coraverage = colMeans( cors[st:en,],na.rm = T)
  if(max( coraverage)< cor.min){
    leadr.colmean = NA
    ss = NA
  } else {
    
    # Using loess method? 
    if ( loess.method){
      x = 1:window
      df = data.frame ( x, y = coraverage)
      p = suppressMessages( ggplot2::qplot(x,y,data=df) + ggplot2::stat_smooth(n = (loess.sensitivity + 1)))
      y = suppressMessages( ggplot2::ggplot_build(p)$data[[2]]$y)
      ss = -(((loess.sensitivity/2)+1) -which.max(y)) / (loess.sensitivity/(window/hz ))
      if( plot.loess ){
        print(p)
      }
    } else {
      ss = NA
    }
    
    # colmeans
    l.f = which.max(coraverage)
    leadr.colmean = -(wn - l.f) / hz
    
    # tau per step
    tau.per.step = apply( cors[st:en,], 1, function(x){
      if( !any(is.na(x)) ){
        x2 = which.max( x)
        return(x2[which.min(abs(x2-wn))])
      }else {
        return(NA)
      }
    }) -wn
    
    #plot
    if(plot.cor){
      timelag = ( (1-wn):(window-wn) )/hz
      plot( coraverage~timelag, xlab = "Time lag (s)", type = "b")
    }
    
  }
  
  # who is leading?
  sl = sign(leadr.colmean)
  print( ifelse ( sl == 1, "p1 is leading",
           ifelse ( sl == 0, "neither are leading", "p2 is leading")))
  
  # return objects
  return (list (colmean.leader = leadr.colmean, 
                loess.leader = ss,
                correlation.matrix = cors,
                tau.per.step = tau.per.step,
                coraverage= coraverage
           
  ))
  
}




