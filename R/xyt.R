xyt = function( data, correct.lon = F, hz = 5, cut.off = 60, plot.traj = T){

  # OBJECTS

  # correct.lon = T
  # hz = 5
  # cut.off = 60
  # plot.traj = T
  # data.save = data
  # data = data.save
{
  # LIBRARIES
  library(stringr)
  library(plyr)

  # rename data columns
  data = as.data.frame(data [, c("LONGITUDE" , "LATITUDE" , "UTC.DATE" , "UTC.TIME", "MS") ])

  # convert lat and lon to numeric
  data$LATITUDE = suppressWarnings(as.numeric(as.character(data$LATITUDE)))
  data$LONGITUDE = suppressWarnings( as.numeric(as.character(data$LONGITUDE)))

  # correct longitude -
  if( correct.lon){
    if(length( sign(data$LONGITUDE) == -1 ) < # sometimes stored as negative, sometimes not, but all studies conducted in negative longitude so -(x) converts all longitude values to negative. Though in some rare cases, the birds flew  past 0 longitude, so see if length is different solves this.
       length( sign(data$LONGITUDE) ==  1 )){
      data$LONGITUDE = -(data$LONGITUDE)
    }
  }

  # correct time
  if( length(which( data$UTC.TIME == "UTC TIME")) != 0){ # fault with logger, sometimes records a time as "UTC TIME" instead of e.g. "00:00:01"
    data$UTC.TIME[data$UTC.TIME == "UTC TIME"] = NA
  }

  # time as posixct
  data = data[complete.cases(data),]
  time.posixct = as.numeric( as.vector(as.POSIXct( paste(data$UTC.DATE,data$UTC.TIME), format = "%Y/%m/%d %H:%M:%S")))
  t1 = time.posixct[1]
  data$time.sec = time.posixct - t1
  tb = table( data$time.sec  )
  data$decimal.seconds= unlist(apply(t(tb),2 , function(x){ (0:(x-1))/hz } ))
  data = data[data$decimal.seconds<0.9,]
  data$time = data$time + data$decimal.seconds


  # remove duplicate timestamps
  dup = which(duplicated(round(data$time,2)))
  if( length(dup > 0)){
    data = data [ -dup,]
  }

  # Correct for when Gps jumps forwards before returning back.
  jumps = which( data$time[2:nrow(data)] - data$time[1:(nrow(data)-1)] < 0 )
  stretch = 1
  if(length(jumps)>0){
    for ( i in 1:length(jumps)){
      jumps2 = 1
      while(length(jumps2)>0){
        lenseq = length(data$time[(jumps[i]-stretch):(jumps[i]+stretch)])
        data$time[(jumps[i]-stretch):(jumps[i]+stretch)] =
          seq( data$time[jumps[i]-stretch],
               data$time[jumps[i]-stretch]+ (((1/hz)*lenseq ) - (1/hz/2) ),(1/hz) )
           jumps2 = which( data$time[(jumps[i]-stretch):(jumps[i]+stretch+1)] -
                          data$time[(jumps[i]-stretch-1):(jumps[i]+stretch)] < 0 )
        stretch = stretch+1
      }
    }
  }

  #Add rows of NA where there are missing timestamps
  tb = table(data$time.sec)
  cumtb = cumsum(c(1,tb))
  tb2 = cbind(cumtb[1:length(cumtb)-1],tb)
  lon = as.vector( apply( tb2  , 1, function(x){
    #x = tb2[1,]
    x[2] = ifelse ( x[2] > 5,5,x[2])
    y = data$LONGITUDE[x[1]:(x[1]+(x[2]-1))]
    t = data$time[x[1]:(x[1]+(x[2]-1))]
    true = round(seq(0,(1-(1/hz)),(1/hz)),2) %in% round(as.numeric(t-floor(t)),2)
    outp = rep(NA,hz)
    outp[true] = y
    return(outp)
  }))
  lat = as.vector( apply( tb2  , 1, function(x){
    #x = tb2[1,]
    x[2] = ifelse ( x[2] > 5,5,x[2])
    y = data$LATITUDE[x[1]:(x[1]+(x[2]-1))]
    t = data$time[x[1]:(x[1]+(x[2]-1))]
    true = round(seq(0,(1-(1/hz)),(1/hz)),2) %in% round(as.numeric(t-floor(t)),2)
    outp = rep(NA,hz)
    outp[true] = y
    return(outp)
  }))
  time = as.vector( apply( tb2  , 1, function(x){
    #x = tb2[1,]
    y = data$time[x[1]:(x[1]+(x[2]-1))]
    outp = round(seq(0,(1-(1/hz)),(1/hz)),2) + floor(y[1])
    return(outp)
  }))


  # missing whole seconds
  t = time[2:length(time)] - time[1:(length(time)-1)]
  missing.secs =  c(0,which(t>0.9 & t< cut.off))
  if( length( missing.secs)>1){
    lzt = list()
    for ( i in 2:length(missing.secs) ){
      #i=2
      extra.time =  seq( (time[missing.secs[i]]+(1/hz)),
                         (time[missing.secs[i]+1]-(1/hz)),
                         (1/hz) )
      time.until = time[(missing.secs[i-1]+1):missing.secs[i]]
      lzt[[i-1]] = as.data.frame(
        cbind( lon = c(lon[(missing.secs[i-1]+1):missing.secs[i]],
                       rep(NA,length(extra.time))),
               lat = c(lat[(missing.secs[i-1]+1):missing.secs[i]],
                       rep(NA,length(extra.time))),
               time= c(time.until,extra.time))  )  }
    lzt[[length(missing.secs)+1]] = as.data.frame(cbind(
      lon = lon [(missing.secs[length(missing.secs)]+1):length(lon)],
      lat = lat [(missing.secs[length(missing.secs)]+1):length(lat)],
      time= time[(missing.secs[length(missing.secs)]+1):length(time)]))
    data = ldply(lzt,data.frame)
  } else {
    data = as.data.frame(cbind(lon,lat,time))
  }

  # plot
  if( plot.traj ){
    plot(data$lon, data$lat)
  }
}
  #time
  data$time = data$time + t1


  #Return
  return(data)

}
