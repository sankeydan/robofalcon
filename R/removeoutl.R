removeoutl = function(dat, p.val = 0.05, minsplit.dist = 10, maxgroup.dist = 50, const = 1,plot = F){

  # OBJECTS

  # dat.save = dat
  # dat = dat.save
  # p.val = 0.05
  # minsplit.dist = 5
  # const = 2
  # plot = T

  #vars
  dims = dim(dat)
  max.remove = floor( dims[3] / 2)

  #empty objects
  dat.rem = array( NA, dim(dat),dimnames = dimnames(dat))
  dist2cent.rem = array(NA, dims[c(1,3)])

  #objects
  numremoved = 0
  allremoved = F

  while ( !any ( c(  allremoved == F  ,numremoved < max.remove )==F)){

    # dist2cent
    dist2cent = d2c(dat)

    # remove outliers
    remove = apply( dist2cent, 1,function(x){
      # i = 360
      # x = dist2cent[660,]
      # plot( dist2cent[,1])
      # for ( i in 2:ncol(dist2cent)){
      # lines (dist2cent[,i])
      # }
      if ( any(!is.na(x))){
        if( any(na.omit(x) > minsplit.dist)){
          if( any(na.omit(x) > maxgroup.dist)){
            z = which.max(x)
            return(z)
          } else {
          r = fitdistr(na.omit(x), "exponential")
          p = apply(t(x),2,function(y) {
            # y = x[1]
            if( !is.na(y)){
              exp( -r$estimate * y * const )
            } else {
              NA
            }
          })
          z = which.min(p)
          }
          if ( p[z] < p.val){
            return( z)
          } else{
            return( NA)
          }
        } else {
          return( NA)
        }
      } else {
        return( NA)
      }
    })


    if( any( !is.na(remove)) ){
      for ( j in 1:length(remove)){
        # j=400
        if( !is.na(remove[j])){
          dat.rem[j,c("lon","lat"),remove[j]] = dat[j,c("lon","lat"),remove[j]]
          dat[j,c("lon","lat"),remove[j]] = NA
          dist2cent.rem[j,remove[j]] = dist2cent[j,remove[j]]
          dist2cent[j,remove[j]] = NA
        }
      }
      allremoved = F
      numremoved = numremoved+1
    } else{
      allremoved = T
    }

    ## PLOT ##
    if( plot){
      if( numremoved==1 | allremoved ==T){
        yli = max(na.omit(as.vector(dist2cent)))
        yli = ifelse( yli > 600, 600, yli)
      }
      plot( dist2cent[,1],type = "n", ylim=c(0,yli))
      for ( j in 2:ncol(dist2cent)){
        lines (dist2cent[,j])
        lines (dist2cent.rem[,j], col = 2)
      }
    }

  }


  ## RETURN ##
  return( list( dat, dat.rem))
}
