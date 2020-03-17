#' Group GPS trajectories for group flights
#'
#' @export
#'

gGPS = function(files,
                file_path = NULL,
                plot_speed = F,
                plot_traj = T,
                hz = 5,
                smooth.param.lag = 5, # runningmean function lag
                speed.min = 10,
                site.radius = 700,
                speed.dur = 100,
                pre.flight.section = F,
                pre.flight.section.length = 500

){

  # Objects

  # files = files2
  # file_path = folder.p
  # plot_speed = T
  # plot_traj = T
  # hz = 5
  # smooth.param.for.speed.start = 20
  # smooth.param.lag = 20
  # speed.min = 10
  # pre.flight.section.length = 500
  # speed.dur = 56
  # pre.flight.section = T
  # site.radius = 700

  # defensive coding
  if ( is.null ( file_path)){
    stop( "SPECIFY FILE PATH (what folder are the files in?)")
  }

  # libraries
  library(robofalcon) # DO NOT COPY THIS TO OTHER PACKAGES
  library(chron)

  # objects
  splitstring = str_split_fixed(files,"\\.",12)
  name = splitstring[,2]
  condition = splitstring[1,3]

  # set up  vectors
  start.time = rep(NA,  length(files))
  nrows      = rep(NA,  length(files))

  # LOOP all files to add data to env.
  for ( j in 1:length(files)){
    #j=4

    #load and assign data
    load( file.path( file_path , files[j]))

    # trim files by speed
    speed = get_dist(data$lat, data$lon,method = "speed",hz=5)
    x = speed
    lag = smooth.param.lag
    run = runningmean (x,lag )
    vec = run > speed.min
    co = vector()
    i = 1
    while(length(co)<speed.dur & i < length(run)){
     if(!is.na(run[i])){
       if( vec[i] == T){
        co = c(co,1)
        fin = i
      } else {
        co = vector()
      } }
      i = i+1
    }


    if ( i != length(run)){
    y = fin - which(x[fin:1] == 0)[1]
    y = ifelse( length(y) == 0 , fin, y)
    } else {
      y = 1
    }
    if ( pre.flight.section){
      y = y - pre.flight.section.length
      y = ifelse(y < 1,1,y  )
    }

    #plot(run)
    #plot(data$lon)
    #abline(v = y)

    #print(y)

    # save start time and number of rows (stored from the cut off when the individual reached home)
    start.time[j] =  data$time[y]
    nrows[j] = nrow(data)

    #assign
    assign ( paste0( "P" ,j), data)
  }

  # Find the Min start time and match up the pigeons start times
  min.start = max(start.time, na.rm = T)
  n.rows = rep(NA, length(files)) # for next part of script
  for ( j in 1:length(files)){
    data = get(paste0("P",j))
    # trim data
    #plot(data$time -min.start )
    data = data[which(data$time ==min.start)[1]:nrow(data),]
    n.rows[j] = nrow(data)
    assign( paste0("P",j) , data)
  }


  # find max end time  match up the pigeons end times

  n.rows[n.rows>10000] = NA
  max.fin = max(n.rows,na.rm = T)
  for ( j in 1:length(files)){
    #j=1
    data = get(paste0("P",j))
    if ( nrow(data)< max.fin){
    foo =  matrix (NA, nrow = ( max.fin - nrow(data)), ncol = ncol(data) , dimnames = list(NULL , dimnames(data)[[2]]))
    data = rbind( data, foo)
      } else {
      data = data[1:max.fin,]
      }
    assign( paste0("P",j) , data)
  }

  #plot speed
  if(plot_speed){
    data = get(paste0("P",1))
    speed = get_dist(data$lat, data$lon,method = "speed",hz=5)
    plot(speed, type = "n",col = 1,ylab = "Speed", ylim = c(0,38), main = condition)
    for ( j in 2:length(files)){
      data = get(paste0("P",j))
      speed = get_dist(data$lat, data$lon,method = "speed",hz=5)
      lines(speed, col = j)
    }
  }


  #build array
  assign( "data" , # specific name for each group flight
          array(NA, c( nrow(data), 3, length(files)), # rows, variables, pigeon
                dimnames = list(NULL, # dimnames , null for row
                                c("lon", "lat" , "time" ), # variable names
                                name))) # pigeon names
  for ( j in 1:length(files)){ # for each pigeon in the flock, add the following elements to the array
    data[,1,j] = get(paste0("P", j))$lon # lon / x
    data[,2,j] = get(paste0("P", j))$lat # lat / y
    data[,3,j] = get(paste0("P", j))$time
  }

  # cut by distance to site
  pidge.num = dim(data)[3]
  end = rep(NA,pidge.num)
  for ( j in  1:pidge.num){
    dist2site = get_dist(median(data[1:20,1,j],na.rm=T), # good starting point
                         median(data[1:20,2,j],na.rm=T),
                         data[,1,j],data[,2,j],method = "distance")
    end[j] = which(dist2site > site.radius)[1]
    end[j] = ifelse(is.na(end[j]),dim(data)[1],end[j])
  }
  data = data[1:max(end),,]


  # add corrrect times to each individual
  start = na.omit(data[1,3,])[1]
  end = na.omit(data[dim(data)[[1]],3,])[1]
  sq = seq(start,end,length.out = nrow(data))
  if( round( sq[2] - sq[1] , 4) != 1/hz){
    stop( "distance between timesteps is not 1/hz")
  }
  for( j in 1:dim(data)[[3]]){
    data[,3,j] = sq
  }

  # # # #
  # num.ind = apply( data[,1,], 1, function(x){length(which(!is.na(x)))})
  # data = data[which(num.ind >2 )[1]:(nrow(data)-which(rev(num.ind) >2)[1]),,]
  # num.ind = apply( data[,1,], 1, function(x){length(which(!is.na(x)))})
  # plot(num.ind)
  # # #

  # plot traj
  if( plot_traj){
    plot(data[,1:2,1],type="l",ylim = range(as.vector(data[,2,]),na.rm = T),
         xlim = range(as.vector(data[,1,]),na.rm = T))
    for ( i in 1:dim(data)[3]){
      lines(data[,1:2,i],col=i)
    }
  }

  #return
  return(data)
}





