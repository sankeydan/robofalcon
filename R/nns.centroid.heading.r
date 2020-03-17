nns.centroid.heading = function(  data,
                                  heads,
                                  data.remain,
                                  heads.remain,
                                  nn.name,
                                  j,
                                  num.nei = 34,
                                  whole.group = T ){

  # VARIABLES

  # num.nei = 34
  # whole.group = T

  # DATA
  dat = data[,,j]
  nn.nam = nn.name[,,j]
  dims = dim(data)
  num.loop = ifelse ( num.nei > (dims[3]-1),(dims[3]-1),num.nei )

  # LIBRARIES
  library(circular)

  # Make main matrix
  if ( whole.group ){
    mat = array ( NA, c( dims[1], (num.nei+1), 2 ))
  } else {
    mat = array ( NA, c( dims[1],  num.nei   , 2 ))
  }

  # for each
  for ( k in 1:num.loop){
    #k=3
    neigbours = nn.nam[,1:k]
    for ( l in 1:dims[1]){
      #l=1
      mat.nei = as.matrix(neigbours)
      nei.lon = mean ( data[l,c("lon"),mat.nei[l,]], na.rm = T)
      nei.lat = mean ( data[l,c("lat"),mat.nei[l,]], na.rm = T)
      ang2neipos = get_heading(  data[l,"lon",j],
                                 data[l,"lat",j],
                                 nei.lon,
                                 nei.lat,indivs = 2)
      turn2neipos = atan2(sin(ang2neipos-heads[l,j]),
                          cos(ang2neipos-heads[l,j]))

      nei.head = mean.circular(circular(heads)[l,mat.nei[l,]])
      turn2neihead = atan2(sin(nei.head-heads[l,j]),
                           cos(nei.head-heads[l,j]))

      # save
      mat[l,k,1] = turn2neipos
      mat[l,k,2] = turn2neihead
    }

  }

  # ADD GLOBAL CENTROID
  if ( whole.group){
    allgroup.pos = data.remain[,,-k]
    allgroup.head= heads.remain[,-k]

    centroid = d2c(allgroup.pos,return_centorid = T)

    ang2neipos = get_heading(  data[,"lon",j],
                               data[,"lat",j],
                               centroid[,"lon"],
                               centroid[,"lat"],indivs = 2)

    meanheads = apply(allgroup.head,1,function(x){
      #x = allgroup.head[1,]
      if ( any( !is.na(x))){
        mean.circular(circular(na.omit(x) ))
      } else {
        NA
      }
    })

    turn2neipos = atan2(sin(ang2neipos-heads[,j]),
                        cos(ang2neipos-heads[,j]))
    turn2neihead = atan2(sin(meanheads-heads[,j]),
                         cos(meanheads-heads[l,j]))

    # save
    mat[,(num.nei+1),1] = turn2neipos
    mat[,(num.nei+1),2] = turn2neihead


  # manipulate
  ret.mat = as.data.frame(cbind( mat[,,1], mat[,,2]))

  # names
  names = c(paste0( "nn", 1:num.nei, "cent"),"allcent",
            paste0( "nn", 1:num.nei, "head"),"allhead")
  names(ret.mat)= names

  # return
  return(ret.mat)

  } else {

    # names
    names = c(paste0( "nn", 1:num.nei, "cent"),
              paste0( "nn", 1:num.nei, "head"))
    ret.mat = as.data.frame(cbind( mat[,,1], mat[,,2]))
    names(ret.mat)= names

    # return
    return(ret.mat)
  }
}
