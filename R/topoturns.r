topoturns = function( data,
                      type = NULL,
                      site.radius = 500,
                      min.acc.indicates.start.flight = 0.4,
                      smooth.param.acceleration =10,
                      bin.num = 10){

  # OBJECTS

  # data = data
  # type = condition[i]
  # site.radius = 500
  # smooth.param.acceleration =10
  # min.acc.indicates.start.flight = 0.4
  # bin.num = bin.num

  # objects/manipulations
  {rsc = data.frame (lat = c(51.415368, 51.37112, 51.36264), lon =  c(-0.572615, -0.5979012, -0.573786), site =  c("home" ,"site", "t.site")  )
  if ( type == "t"){
    site = rsc[rsc$site == "t.site",]
  } else {
    site = rsc[rsc$site == "site",]
  }
  if( type =="p"){
    pred = data[,,   dim(data)[3]   ]
    data = data[,,(1:dim(data)[3]-1)]
  } else {
    pred = F
  }
  pidge = dimnames(data)[[3]]}

  # empty objects
  tortuosity = vector()
  speeds = vector()
  dist2pred = vector()

  # loop
    for ( i in 1:length(pidge)){

      # trim data to only site
      {dist2site = get_dist(median(data[1:20,1,],na.rm = T),
                            median(data[1:20,2,],na.rm = T),
                            data[,1,i],
                            data[,2,i],method = "distance")
      end = which(dist2site > site.radius)[1]
      end = ifelse(is.na(end),dim(data)[1],end)
      dat = data[1:end,,i]
      }

      # calculate speed
      speed = get_dist(dat[,1],dat[,2],method="speed",hz=5)

      # remove outliers and Trim data according to start of flight
      acc1= diff(speed)
      v = which(acc1 < -3.748319 | acc1 > 4.742196 ) # 0.1% and 99.9% quantiles for acceleration across all files - 500 metres from release site, see methods figure "histogram of acceleration"
      speed[v+1] = NA
      acc = runningmean(diff(speed),smooth.param.acceleration)
      subset = which(acc>min.acc.indicates.start.flight)[1]:nrow(dat) # subset by start of flight (increase in acceleration) and fixed distance from release point
      sp = speed[subset]
      d = dat[subset,]

      # tortuosity
      head = get_heading(d[,1],d[,2])
      turn.angle = diff(head)
      turn.angle = ifelse ( turn.angle < -pi, abs(abs(turn.angle)- 2*pi ),turn.angle)
      turn.angle = ifelse ( turn.angle >  pi,-abs(abs(turn.angle)- 2*pi ),turn.angle)
      tor =  abs(c(turn.angle,NA))
      #plot(tor)

      # bin by dist 2 site
      vec = cut(dist2site[subset],bin.num)
      tortuosity = c(tortuosity,tapply(tor , vec, function( x) {median(x,na.rm = T)}))
      speeds     = c(speeds,    tapply(sp  , vec, function( x) {median(x,na.rm = T)}))
      }

  # PREDATOR
  if(pred[1] != F){

    #subset
    pre = pred[1:end,]
    pr = pre[subset,]

    # dist2pred
    d2p = get_dist(d[,"lon"],
                   d[,"lat"],
                   pr[,"lon"],
                   pr[,"lat"], method = "distance")

    # bin by dist 2 site
    dist2pred  = c(dist2pred ,tapply(d2p , vec, function( x) {median(x,na.rm = T)}))

    # If no predator
  } else {
    dist2pred= c( dist2pred, rep(NA,bin.num))
  }

  # return
  outp = as.data.frame(cbind(tortuosity,
               speeds,
               dist2pred,
               pidge = rep(pidge, each = bin.num),
               bin = rep(1:bin.num,length(pidge))))

  return(outp)
}
