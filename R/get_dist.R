#' Get the distance between two GPS vectors
#'
#' Takes two latitudes / longitudes and calculates distance in metres
#' @param lon1 longtude vector for individual 1 or if method is "speed" then t = 1
#' @param lat1 latitude vector for individual 1 or if method is "speed" then t = 1
#' @param lon2 longtude vector for individual 2
#' @param lat2 longtude vector for individual 2
#' @param hz Only when method is "speed". This is how many locations per second
#' @param method When method = speed, no need for lat2 and lon2
#' @return The distance between the input locations in metres
#' @export
#'

get_dist <- function(lon1, lat1, lon2, lat2, hz, method = c("speed" , "distance")) {

  toRAD = pi/180
  R <- 6371 # Earth mean radius [km]

  if( method == "speed"){
    lon2 = lon1[2:length(lon1)]
    lat2 = lat1[2:length(lat1)]
    lon1 = lon1[1:(length(lon1)-1)]
    lat1 = lat1[1:(length(lat1)-1)]
  }
  if (method == "distance"){
    hz = 1
  }


  delta.lon <- (lon2 - lon1) * toRAD
  delta.lat <- (lat2 - lat1) * toRAD
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.lon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))

  if (method == "speed"){
    d = c(NA, R * c)
  }

  if (method == "distance"){
    d= R * c
  }

  return(d*hz*1000)  # Speed in m/s
}
