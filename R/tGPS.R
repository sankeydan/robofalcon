#' Trim and (clean) GPS
#'
#' @export

tGPS = function (
  data,
  plot_traj = T,
  cut_radius_site = 3000,
  cut_radius_home = 2000,
  mins_record_at_home = 0,
  remove_outliers_speed = 100, # more than 100m/s is outlier
  remove_outliers_deg = 2, # more than n degrees (lon/lat) from mean (lon/lat)
  hz =5
){

  #############################

  ## objects

  # data = dat
  # plot_traj = T
  # cut_radius_site = 3000
  # cut_radius_home = 3000
  # mins_record_at_home = 0
  # remove_outliers_speed = 100
  # remove_outliers_deg = 2
  # hz = 5

  #libraries
  library(plyr)
  library(groupMassComp)


  # load metadata
  site = c( lat = 51.37112 , lon = -0.5979012)
  rsc = data.frame (lat = c(51.415368, 51.37112), lon =  c(-0.572615, -0.5979012), site =  c("home" ,"site")  )
  site = rsc[rsc$site == "site",]
  home = rsc[rsc$site == "home",]


  # remove outliers - using speed, and deg
  out.speed = which(get_dist(data$lon, data$lat, hz = 5, method = "speed")>remove_outliers_speed)
  out.lon   = which(abs(data$lon - median(data$lon,na.rm=T)) > remove_outliers_deg)
  out.lat   = which(abs(data$lat  - median(data$lat ,na.rm=T)) > remove_outliers_deg)
  out = unique(c(out.speed, out.lat, out.lon))
  data[out,c("lon","lat")] = NA

  # trim start / fin
  dist2 = data.frame ( site = get_dist(data$lon, data$lat, rep(site$lon,nrow(data)), rep(site$lat,nrow(data)), method = "distance" ),
                       home = get_dist(data$lon, data$lat, rep(home$lon,nrow(data)), rep(home$lat,nrow(data)), method = "distance" ))
  cut_home = ifelse( na.omit(dist2$site)[1] > cut_radius_site,
                     which(dist2$site<cut_radius_site)[1], 1)
  if ( any ( na.omit(dist2$home[(cut_home+1):nrow(dist2)]) < cut_radius_home)){
    t = which(dist2$home[(cut_home+1):nrow(dist2)] < cut_radius_home )[1]+ (hz*60*mins_record_at_home)
    t = ifelse( t > nrow(data), nrow(data),t+(cut_home+1))
  } else {
    t = nrow(data)
  }

  data = data[(cut_home+1):t,]

# plot
if( plot_traj ){
  plot(data$lon, data$lat,
       xlim = range ( c( data$lon , site$lon, home$lon), na.rm = T),
       ylim = range ( c( data$lat  , site$lat, home$lat), na.rm = T))
  points( site$lon, site$lat , pch= 19, col = "red")
  points( home$lon, home$lat , pch= 19, col = "blue")
}


#return
return( data)

}

