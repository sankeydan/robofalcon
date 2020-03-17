
air_speed = function( GPS.data ,
                      wind.direct,
                      wind.speed,
                      grnspeed,
                      plot = F,
                      return.support.cross = F){

  # objects

  # GPS.data = data[,1:2,j]
  # wind.direct = md$wind.direct.site
  # wind.speed  = md$wind.speed.site
  # plot = T
  # grnspeed = speed
  # return.support.cross = T

  # objects
  dir  = c("S","SSW","SW","WSW","W","WNW","NW","NNW","N","NNE","NE","ENE","E","ESE","SE","SSE")

  #stopifnot
  if ( ! wind.direct %in% dir){
    stop( "wind dir must be one of the following c('S','SSW','SW','WSW','W','WNW','NW','NNW','N','NNE','NE','ENE', 'E','ESE','SE','SSE')")
  }

  if( ncol(GPS.data) != 2 | dimnames(GPS.data)[[2]][1] != "lon"){
    stop( "GPS data must be a matrix with column names 'lon' and 'lat' ")
  }

  #data
  data = as.data.frame(GPS.data)

  # get_heading
  data$head = get_heading(data[,1], data[,2], indivs = 1)


  dir.circ = data.frame ( circ = seq(-pi,pi-(pi/16),length.out=16),
                          dir)
  wind.dir = dir.circ$circ[which(dir.circ$dir == wind.direct)]


  #Airspeed
  air.speed = rep(NA, nrow(data))
  cross=rep(NA,nrow(data))
  support=rep(NA,nrow(data))
  for (i in 1:nrow(data)){

    diff =wind.dir - data$head[i]
    x = abs(atan2(sin(diff), cos(diff)))
    diff = ifelse ( diff < -pi, diff+2*pi,diff)
    diff = ifelse ( diff > pi , diff-2*pi,diff)

    if(!is.na(x)){
      if ( abs(x) > pi/2){ # if support is negative
        x = abs(x) - (pi/2)
        if ( sign(diff) == 1 ){ # if cross is positive
          cross[i]     = abs(cos(x)) * wind.speed
          support[i]  = -abs(sin(x)) * wind.speed
        } else{ # if cross is negative
          cross[i]   = -abs(cos(x)) * wind.speed
          support[i] = -abs(sin(x)) * wind.speed
        }
      } else{ # if support is positive
        if( sign(diff) == 1){ # if cross is positive
          cross[i]  =  abs(sin(x)) * wind.speed
          support[i]=  abs(cos(x)) * wind.speed
        } else{ # if cross is negative
          cross[i]  = -abs(sin(x)) * wind.speed
          support[i]=  abs(cos(x)) * wind.speed
        }
      }
       air.speed[i] = sqrt ( (grnspeed[i] - support[i])^2 + cross[i]^2)
    } else{
      air.speed[i] = NA
    }
  }

  #plot?
  if(plot){
    plot(grnspeed,type = "l")
    lines(air.speed,col="red")
  }

  # return data
  if( return.support.cross){
    return(list(air.speed,support,cross,wind.dir))
  } else{
    return(air.speed)
  }

}
