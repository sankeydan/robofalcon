lead.big = function( gdata, ndata, fdata, leadership = T){

  # OBJECTS

  # leadership = T

  # DATA
  nn.mat = ndata[[3]]
  nn.dist= ndata[[2]]
  all.dist=ndata[[1]]
  fisdata   = fdata[[1]]

  # OBJECTS
  group = as.numeric(spl[i,4])
  names = names.list[[group]]
  namesd= dimnames(gdata)[[3]]

  # EMPTY OBJECTS
  nei.cent.mat = matrix( NA, length(names), 2, dimnames = list(names,c("nei", "cent")))
  pairwise.mat = matrix( NA, length(names), length(names), dimnames =  list(names,names))

  # LOOP PER INDIVIDUAL
  for ( j in 1:length(namesd)){
    # j=1

    # focal data
    p1 = gdata[,c("lon","lat"),j]
    flight.len = length(na.omit(p1[,1]))

    # synchroniscity - neighbour
    {
      nn = nn.mat[,j]
      foo = cbind( gdata[,"lon",], nn)
      nn.lon = apply( foo,1,function(x){ x[x[length(x)]]})
      foo = cbind( gdata[,"lat",], nn)
      nn.lat = apply( foo,1,function(x){x[x[length(x)]]})
      p2 = as.matrix( cbind( lon = nn.lon,
                             lat = nn.lat) )
      nei.cent.mat[namesd[j],"nei"] = leadRship(p1,p2)[[1]]
    }
    # synchronicity - centroid
    {
      p2 = t(apply( fisdata[,c("lon","lat"),-j], 1,function(x){
        #x=data[1,c("lon","lat"),-j]
        rowMeans( x, na.rm = T)
      }))
      nei.cent.mat[namesd[j],"cent"] = leadRship(p1,p2)[[1]]
    }

    # leadership
    if ( leadership){
      for ( k in 1:length(namesd)){
        if( k > j){
          #k=2
          p2 = gdata[,,k]
          lead = leadRship(p1,p2)[[1]]
          pairwise.mat[namesd[j],namesd[k]] = lead
          pairwise.mat[namesd[k],namesd[j]] = -lead
        }
      }
    }
  }

  #return
  if( leadership){
    return( list(nei.cent.mat , pairwise.mat))
  } else {
    return( nei.cent.mat)
  }
}


