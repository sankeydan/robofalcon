#' Get pairwise leadership score
#'
#' @export

# leadRship


leadRship= function( p1,
                     p2,
                     window = 25,
                     cor.min = 0,
                     sync = T,
                     hz = 5,
                     plot.cor=F,
                     method.each.time = F,
                     ac = 10,
                     auto.cor.test = F,
                     auto.test.length = NULL,
                     message = NULL){

  # Objects

  # p1 = p1
  # p2 = p2
  # window = 25
  # cor.min = 0.9
  # hz = 5
  # sync = T
  # plot.cor = T
  # ac = 10
  # method.each.time = F
  # auto.cor.test = F
  # auto.test.length = NULL

  # stop if not
  if ( length(dim(p1)) != 2 ){
    stop( "p1 and p2 must be matrics with x and y columns")
  }

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

  if(max( coraverage)< cor.min | any(is.na(coraverage)) ){
    leadr.colmean = NA
    leadr.taumean = NA
    leadr.taumed  = NA
  } else {

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
    })
    # # #
    # plot(tau.per.step,main = message)
    # plot(abs(tau.per.step-wn)/hz )
    # abline(h = abs(leadr.colmean))
    #plot(coraverage)
    # # # #
    leadr.taumean = (mean  ( abs(tau.per.step-wn),na.rm=T))/hz
    leadr.taumed =  (median( abs(tau.per.step-wn),na.rm=T))/hz
  }

  # METHOD EACH TIME
  if ( method.each.time){
    l.f = apply( cors[st:en,], 1, function(x) {

      if (! any(is.na(x))){
        if( max(x)[1] > cor.min ){
          round(mean(which (x == max(x))))
        } else {
          NA
        }} else {
          NA
        }
    })
    lead = c( rep(NA, wn-1) ,
              ifelse ( l.f < wn, 1, ifelse( l.f > wn, -1, 0)),  # this is the key part. If below window, then leading
              rep(NA, wn-1))

    ## Autocorrelation test
    if ( auto.cor.test){
      auto.testR = matrix(NA, (len - auto.test.length), auto.test.length)
      for ( j in 1:auto.test.length){
        for ( i in st:(en-auto.test.length)){
          auto.testR[i,j] = lead[i] == lead[i+j]
        }
      }
      at = apply( auto.testR[st:(nrow(auto.testR)-wn),],2, function(x){
        mean(as.numeric(x))})
    }

    # remove duplicates
    for ( i in 1:length(lead)){
      if ( !is.na(lead[i])){
        if( i + ac > length(lead)){
          ac2 = length(lead)-i
        } else {
          ac2 = ac
        }
        for ( j in 1:ac2){
          if ( !is.na(lead[i+j]) ){
            if( lead[i] == lead[i+j]){
              lead[i+j] = NA
            }
          }
        }
      }
    }
    leadr = sum(lead,na.rm = T)
  }

  #plot
  if(plot.cor){
    timelag = ((seq(1,25,1)-wn)/5)
    par(mfrow = c(2,1))
    plot( coraverage)
    plot( coraverage~ timelag, ylim = c(0.5,1))
    abline ( v = leadr.colmean, col = 2)
    abline ( v = leadr.taumean, col = 3)
    abline ( v = leadr.taumed , col = 4)
    abline ( h = cor.min)
  }
  # return objects
  if (auto.cor.test){
    return( list(at, leadr))
  } else {
    return( list( leadr.colmean, leadr.taumean, leadr.taumed))
  }



}
