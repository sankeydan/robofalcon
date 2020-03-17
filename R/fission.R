fission = function (data,
                    fis_dist = 50,
                    plot = F,
                    cut_radius_home = 1000,
                    return_omitted_individuals = T) {

  #objects

  # data= data
  # fis_dist = 50
  # plot = F
  # cut_radius_home = 1000
  # return_omitted_individuals = T

  # load metadata
  rsc = data.frame (lat = c(51.415368, 51.457547, 51.496972, 51.44844, 51.46375), lon =  c(-0.572615, -0.588399, -0.588914, -0.480168, -0.395744), site =  c("home" ,"N4" ,  "N8" ,  "E4" ,  "E8")  )
  home = rsc[rsc$site == "home",]

  # centroid of whole group regardless of fission
  centroid = data.frame( x = apply(data[,"lon",], 1, function(x){ mean(x,na.rm = T)}),
                         y = apply(data[,"lat",], 1, function(x){ mean(x,na.rm = T)}))

  # distance to centroid
  dist2cent = matrix(NA, dim(data)[1], dim(data)[3] )
  for ( j in 1:nrow(dist2cent)){ # for each timestep
    for ( k in 1:dim(data)[3]){ # for each pigeon
      dist2cent[j,k] = get_dist( data[j,"lon",k], data[j, "lat", k], centroid$x[j], centroid$y[j] , method = "distance") # get the distance between the individual and the centroid
    }
  }

  # Fission
  fis.data = data
  omit.list = list() # a list will store omitted individuals.
  for ( j in 1:nrow(dist2cent)){ # for each timestamp
    #j=1
    vec = vector() # set up a vector to record ommitted individuals
    while( length( which(dist2cent[j,]  > fis_dist)) >0){ # while there is at least one individual over the fission distance threshold
      omit = which(dist2cent[j,]== max(dist2cent[j,], na.rm = T) ) # which individual needs to be removed?
      fis.data[j,c("lat","lon"), omit] = NA # make NA the latitude and the longitude of the furthest individual
      centroid$x[j] = mean(fis.data[j,"lon",], na.rm = T) # calcutate new centroid
      centroid$y[j] = mean(fis.data[j,"lat",], na.rm = T)
      for( k in 1:dim(data)[3]){
        dist2cent[j,k] = get_dist( fis.data[j,"lon",k], fis.data[j, "lat", k], centroid$x[j], centroid$y[j] , method = "distance") # distance to centroid - same as above
      }
    }
    omit.list[[j]] = which(is.na(dist2cent[j,])) # store in the list
  }

  # remove fissioned individuals from the group
  if(return_omitted_individuals){
    omit.mat = matrix(F, dim(data)[1], dim(data)[3], dimnames = list(NULL, dimnames(data)[[3]]))
  }
  for ( i in 1:dim(data)[1]){
    data[i,1:2,][,omit.list[[i]]] = NA
    if(return_omitted_individuals){
      omit.mat[i,omit.list[[i]]] = TRUE
    }
  }

  # plot
  if ( plot){
    par(mfrow = c(2,1))
    par(mar=(c(1,1,1,1)))
    plot(data[,1,1],type="l",ylim = range(as.vector(data[,1,]),na.rm = T))
    for ( i in 1:dim(data)[3]){
      lines(data[,1,i],col=i)
    }

    plot(data[,1:2,1],type="l",ylim = range(as.vector(data[,2,]),na.rm = T),
         xlim = range(as.vector(data[,1,]),na.rm = T))
    for ( i in 1:dim(data)[3]){
      lines(data[,1:2,i],col=i)
    }
  }
  #return
  if ( return_omitted_individuals){
    return(list(data, omit.mat ))
  } else {
  return(data)
  }
}

