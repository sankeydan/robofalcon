metrics = function( data,
                    falc,
                    output = c("pidge",
                               "maxacceleration",
                               "maxspeed",
                               "dist2pred",
                               "tortuosity",
                               "meanspeed",
                               "supportwindsite",
                               "crosswindsite",
                               "centr.acc"),
                    smooth.param.acceleration =10,
                    smooth.param.speed = 10,
                    type = "c" ,# type = "c" , "p" or "t"
                    min.acc.indicates.start.flight = 0.4,
                    wind.direct.loft = NULL,
                    wind.speed.loft = NULL,
                    wind.direct.site = NULL,
                    wind.speed.site = NULL,
                    centr.method = "mean"
){

  # FUNCTION OBJECTS


  # output = c("pidge","maxacceleration","maxspeed","dist2pred","tortuosity","meanspeed","supportwindsite","crosswindsite","centr.acc")  # smooth.param.acceleration =10
  # smooth.param.speed = 10
  # type = condition[i] # type = "c" , "p" or "t"
  # site.radius = 500
  # min.acc.indicates.start.flight = 0.4
  # wind.direct.loft = md$wind.direct.loft
  # wind.direct.site = md$wind.direct.site
  # wind.speed.loft  = md$wind.speed.loft
  # wind.speed.site  = md$wind.speed.site


  # objects/manipulations
  {rsc = data.frame (lat = c(51.415368), lon =  c(-0.572615), site =  c("home")  )
    pidge = dimnames(data)[[3]]}

  #empty objects
  {
    output.mat = matrix(NA,length(pidge),length(output),dimnames = list(pidge,output))
    output.mat[,"pidge"] = pidge
  }

  ###### PIGEON METRICS. AT SITE
  for ( i in 1:(dim(data)[3]) ){
    #i=1

    # data
    dat = data[,,i]

    # calculate speed
    speed = get_dist(dat[,1],dat[,2],method="speed",hz=5)

    # acceleration
    if("maxacceleration" %in% output){
      # remove outliers
      acc1= diff(speed)
      v = which(acc1 < -3.748319 | acc1 > 4.742196 ) # 0.1% and 99.9% quantiles for acceleration across all files - 500 metres from release site, see methods figure "histogram of acceleration"
      speed[v+1] = NA
      acc = runningmean(diff(speed),smooth.param.acceleration)
      #plot(acc,type="l")
      output.mat[i,"maxacceleration"] = max(acc,na.rm = T)
    }

    #max speed
    if("maxspeed" %in% output){
      run = runningmean(speed,smooth.param.acceleration)
      #plot(run)
      output.mat[i,"maxspeed"]= max(run,na.rm = T)
    }

    # tortuosity
    if("tortuosity" %in% output){
      tor = abs.turn(dat)
      output.mat[i,"tortuosity"] = median(tor,na.rm=T)
    }

    # mean speed
    if("meanspeed"  %in% output){
      output.mat[i,"meanspeed"] = mean( speed,na.rm = T)
    }

    # AIRSPEED

    if ("supportwindsite" %in% output ){
      if( !is.na(wind.direct.site)){
        as.s = air_speed(dat[,1:2],wind.direct = wind.direct.site,
                         wind.speed  = wind.speed.site ,speed, return.support.cross = T)
        output.mat[i,"supportwindsite"] = as.s[[3]]
        output.mat[i,"crosswindsite"] = as.s[[2]]
      }
    }

    # Distance to predator
    if ( "dist2pred" %in% output & !is.null(falc)){
      d2p = get_dist(dat[,"lon"],
               dat[,"lat"],
               falc[,"lon"],
               falc[,"lat"],method = "distance")
      output.mat[i,"dist2pred"] =  min ( d2p, na.rm = T)
    }

    # CENTREPETAL ACCELERATION
    if("centr.acc" %in% output){
      r = speed / tor
      if ( centr.method == "mean"){
      output.mat[i,"centr.acc"] = mean (speed^2 / r,na.rm=T)
      }
      if ( centr.method == "median"){
        output.mat[i,"centr.acc"] = median (speed^2 / r,na.rm=T)
      }
    }
  }
  ###### OUTPUTS


  # RETURN
  return( output.mat)
}
