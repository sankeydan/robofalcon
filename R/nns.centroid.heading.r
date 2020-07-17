nns.centroid.heading = function(  data,
                                  heads,
                                  data.remain,
                                  heads.remain,
                                  nn.name,
                                  j,
                                  num.nei = 33, 
                                  future.predict=T){
  
  # VARIABLES
  
  # num.nei = 33
  
  # Metadata
  load ( file.path(PROJHOME , "metadata", "site-xy.rda"))
  site.xy = site.xy-0.01
  
  # DATA
  dat = data[,,j]
  nn.nam = nn.name[,,j]
  dims = dim(data)
  num.loop = ifelse ( num.nei > (dims[3]-1),(dims[3]-1),num.nei )
  datm = cbind ( x = get_dist(dat [ , "lon"], site.xy[2],site.xy[1],site.xy[2],method="distance"),
                 y = get_dist(site.xy[1] , dat [ ,"lat"],site.xy[1],site.xy[2],method="distance"))
  
  
  # LIBRARIES
  library(circular)
  
  # Make main matrix
  mat = array ( NA, c( dims[1],  num.nei   , 3 ))
  
  
  # for each
  for ( k in 1:num.loop){
    #k=3
    neigbours = nn.nam[,1:k]
    mat.nei = as.matrix(neigbours)
    rM.lon = rep(NA,dims[1])
    rM.lat = rep(NA,dims[1])
    for ( n in 1:dims[1]){
      #n=1
      rM.lon[n] = mean ( data[n,"lon",mat.nei[n,]],na.rm = T)
      rM.lat[n] = mean ( data[n,"lat",mat.nei[n,]],na.rm = T)
    }
    
    neim = cbind ( x = get_dist(rM.lon     , site.xy[2],site.xy[1],site.xy[2],method="distance"),
                   y = get_dist(site.xy[1] , rM.lat    ,site.xy[1],site.xy[2],method="distance"))
    
    for ( l in 3:dims[1]){
      #l=4
      
      datmi = datm[(l-2):l,]
      neimi = neim[(l-2):l,]
      ta = turnang(neimi,cartesian = T)[1]
      cd = cartesian_dist(neimi[,1],neimi[,2],hz=1,method="speed")[3]
      gh = get_heading_cartesian(neimi[,1],neimi[,2],indivs = 1)[2]
      head = gh+ta
      head = ifelse ( head > pi, head-2*pi,head)
      head = ifelse ( head < -pi,head+2*pi,head)
      if ( !is.na(head)){
        sq = seq(-pi,pi,length.out = 5)
      sq[1]= -pi-.001
        ma = max ( which(head>sq))
      multip = t(matrix( c(-1,-1,1,-1,1,1,-1,1),2))[ma,]
      predicted.lon = neimi[3,1] + abs(cd  * sin ( head))* multip[2]
      predicted.lat = neimi[3,2] + abs(cd  * cos ( head))* multip[1]
      neimi = rbind ( neimi , c(predicted.lon,  predicted.lat))
      
      # Insta centroid
      ang2neipos = get_heading_cartesian(  datm[l,"x"],
                                           datm[l,"y"],
                                           neimi[3,1],
                                           neimi[3,2],indivs = 2)
      turn2neipos = atan2(sin(ang2neipos-heads[l,j]),
                          cos(ang2neipos-heads[l,j]))
      
      # Predicted future centroid
      ang2neipos = get_heading_cartesian(  datm[l,"x"],
                                           datm[l,"y"],
                                           neimi[4,1],
                                           neimi[4,2],indivs = 2)
      turn2neifuture = atan2(sin(ang2neipos-heads[l,j]),
                             cos(ang2neipos-heads[l,j]))
      
      # Alignment
      nei.head = mean.circular(circular(heads)[l,mat.nei[l,]])
      turn2neihead = atan2(sin(nei.head-heads[l,j]),
                           cos(nei.head-heads[l,j]))
      
      # save
      mat[l,k,1] = turn2neipos
      mat[l,k,2] = turn2neihead
      mat[l,k,3] = turn2neifuture
      }
    }
    
  }
  
  
  # names
  names = c(paste0( "nn", 1:num.nei, "cent"),
            paste0( "nn", 1:num.nei, "head"),
            paste0( "nn", 1:num.nei, "futr"))
  ret.mat = as.data.frame(cbind( mat[,,1], mat[,,2],mat[,,3]))
  names(ret.mat)= names
  
  # return
  return(ret.mat)
  
}
