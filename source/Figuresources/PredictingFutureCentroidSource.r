
{ # skip to loop
  
  # housekeeping
  rm(list = ls())
  
  # libraries
  library(robofalcon)
  library(stringr)
  library(plyr)
  library(circular)
  
  
  # objects
  only.small.groups = F
  only.remaining.indivs = F
  
  # files / folders 
  fold = file.path(PROJHOME, "Output", "gGPS" , "site-radius500")
  fold.remain = file.path(PROJHOME, "Output", "DataRemovedOutliers")
  files = list.files(fold)
  files.f=list.files(fold.remain)
  splitstring = str_split_fixed(files, "\\.",12)
  
  # metadata
  load( file.path( PROJHOME ,  "metadata" , "home-coordinates.rda"))
  load(file.path ( PROJHOME ,  "metadata" , "metadata-combinedPredLoft.rda"))
  
  #manip objects 
  splitstring = str_split_fixed(files, "\\." , 9)
  condition = splitstring[,2]
  splitstring = as.data.frame(splitstring)
  names(splitstring) = c("flight.id", as.character(2:ncol(splitstring)))
  if ( only.small.groups){
    whi.big = which(splitstring[,4] == 5)
    files = files[-whi.big]
    condition = condition[-whi.big]
    splitstring = splitstring[-whi.big,]
  }
  
  # empty objects
  aeps = vector()
  bigmatrix = matrix(NA,0,10)
  m = 1
  li = list()
}

i=9
  
  {
    # data
    load( file.path(fold, files[i]))
    data.list[[2]]
    load( file.path(fold.remain, files[i]))
    data.remain = data.list[[1]]
    data.all    = data.list[[2]]
    if( only.remaining.indivs){
      data= data.remain
    } else {
      data = data.all
    }
    
    dims = dim(data)
    
    # near neighbours 
    nn = near_nei(data)
    heads = apply( data, 3, function(x){
      get_heading(x[,"lon"],x[,"lat"])
    })
    heads.remain = apply( data.remain, 3, function(x){
      get_heading(x[,"lon"],x[,"lat"])
    })
    
    # centroid
    centroid = d2c(data,return_centorid = T)
    d2cent = d2c(data)
    
    # metadata
    md   =merge ( splitstring[i,], meta, by = "flight.id")
  }
  
 j=1
    
    # metadata
    temp.data   =md[rep(1, dims[1]),c('condition','small.big','ind.flight','wind.direct.site', 'group.num','wind.speed.site','flight.time','Date')]
    temp.data$pigeon = dimnames(data)[[3]][j]
    
    # support and cross wind
    speed = get_dist(data[,"lon",j],
                     data[,"lat",j], hz = 5, method = "speed")
    temp.data$speed = speed
    if ( i > 8){
      as.s = air_speed(data[,1:2,j],
                       wind.direct = md$wind.direct.site,
                       wind.speed  = md$wind.speed.site ,
                       speed, 
                       return.support.cross = T)
      temp.data$support.wind =  as.s[[2]]
      temp.data$cross.wind   =  as.s[[3]]
    }
    
    # focal heading and turning decisions. autocorrelation
    focal_head = get_heading(data[,"lon",j],data[,"lat",j])
    temp.data$focal_head = focal_head
    temp.data$diff_head = turnang(data[,,j])
    
    # lon lat
    temp.data$lon = data[,"lon",j]
    temp.data$lat = data[,"lat",j]
    
    # dist 2 cent
    temp.data$dist2cent = d2cent[,j]
    
    # turn angle to home
    ang2home = get_heading( data[,"lon",j],
                            data[,"lat",j],
                            home$lon,
                            home$lat,indivs = 2)
    temp.data$turn2home = atan2(sin(ang2home-focal_head), cos(ang2home-focal_head))
    

    
    # distance to home 
    temp.data$dist2home = get_dist(data[,"lon",j],data[,"lat",j],home$lon,home$lat,method = "distance")
    
    # near neighbours 
    nn.dist = nn[[1]] 
    nn.name = nn[[3]]
    
    