near_nei = function ( data){

  # variables

  #data = dat.all.complete

  #objects
  dims = dim(data)
  dimname = dimnames(data)

  ##### MATRICES

  if( length(dims) == 2 ){

    # empty object
    dist.arr = matrix(NA, dims[1],dims[1], dimnames = list(dimname[[1]],
                                                           dimname[[1]]))

    # loop
    for ( i in 1:dims[1]){
      for ( j in 1:dims[1]){
        if(i!=j){
          dist.arr[i,j] =  get_dist(data[i,"lon"],
                                     data[i,"lat"],
                                     data[j,"lon"],
                                     data[j,"lat"], method = "distance")
        }
      }
    }

    return(dist.arr)
  }

  ##### ARRAYS
  if( length(dims) == 3 ){


    # empty object
    dist.arr = array(NA, c(dims[1],dims[3],dims[3]), dimnames = list(NULL,
                                                                     dimname[[3]],
                                                                     dimname[[3]]))
    # loop
    for ( i in 1:dims[3]){
      for ( j in 1:dims[3]){
        if(i!=j){
          dist.arr[,j,i] =  get_dist(data[,"lon",i],
                                     data[,"lat",i],
                                     data[,"lon",j],
                                     data[,"lat",j], method = "distance")
        }
      }
    }

    # empty object
    nn.mat = matrix(NA, dims[1], dims[3], dimnames = list(NULL,dimname[[3]]))

    # nearest neighbour
    for ( i in 1:dims[3]){
      nn.mat[,i] = apply( dist.arr[,,i], 1, function(x){
        if( any( !is.na(x))){
          min(x, na.rm = T)
        } else {
          NA
        }
      })
    }

    # empty object
    nn.whi = array(NA, c( dims[1], dims[3], dims[3]),  dimnames = list(NULL,NULL,dimname[[3]]))

    # which neighbour
    for ( i in 1:dims[3]){
      #i = 1
      d.a.i = dist.arr[,,i]
      nwhi = t(apply( d.a.i, 1, function(x){
          # x =  d.a.i[3,]
          dimname[[3]][order(x)]
      }))
      nn.whi[,,i] = nwhi
    }

    # RETURN
    return( list ( dist.arr, nn.mat, nn.whi))
  }
}
