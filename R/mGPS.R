#' Manipulate (clean) GPS
#'
#' @export

mGPS = function (
  data,
  plot_traj = T,
  remove_outliers_speed = 100, # more than 100m/s is outlier
  remove_outliers_deg = 2, # more than n degrees (lon/lat) from mean (lon/lat)
  hz =5
){

  #############################

  ## objects

  # data = data
  # plot_traj = T
  # remove_outliers_speed = 100
  # remove_outliers_deg = 2
  # hz = 5

  #libraries
  library(plyr)
  library(groupMassComp)


  # load metadata
  rsc = data.frame (lat = c(51.415368, 51.37112), lon =  c(-0.572615, -0.5979012), site =  c("home" ,"site")  )

  # remove outliers - using speed, and deg
  out.speed = which(get_dist(data$lon, data$lat, hz = 5, method = "speed")>remove_outliers_speed)
  out.lon   = which(abs(data$lon - median(data$lon,na.rm=T)) > remove_outliers_deg)
  out.lat   = which(abs(data$lat  - median(data$lat ,na.rm=T)) > remove_outliers_deg)
  out = unique(c(out.speed, out.lat, out.lon))
  data[out,c("lon","lat")] = NA

  # trim start / fin
  dist2 = data.frame ( site = get_dist(data$lon, data$lat, rep(site$lon,nrow(data)), rep(site$lat,nrow(data)), method = "distance" ),
                       home = get_dist(data$lon, data$lat, rep(home$lon,nrow(data)), rep(home$lat,nrow(data)), method = "distance" ))
  if( na.omit(dist2$site)[1] > cut_radius_site){ # to use if some GPS data was recorded back at the loft. This will trim that part off.
    cut_home = which(dist2$site<cut_radius_site)[1]
    data = data[cut_home+1:nrow(data),]
  }
  data = data[which(dist2$site > cut_radius_site)[1]:
                which(dist2$home < cut_radius_home)[1],]



  # plot
  if( plot_traj ){
    plot(data$lon, data$lat,
         xlim = range ( c( data$lon , site$lon, home$lon), na.rm = T),
         ylim = range ( c( data$lat  , site$lat, home$lat), na.rm = T))
    points( site$lon, site$lat , pch= 19, col = "red")
    points( home$lon, home$lat , pch= 19, col = "blue")
  }


  #save
  return( data)

}

