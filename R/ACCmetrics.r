ACCmetrics = function( acc_data,sampling_freq = 200,
                       print.updates = F,
                       plot.updates  = F,
                       split.val = 100000,
                       axis.of.interest = "z",
                       smooth_type = "standard"){

  # OBJECTS

  # acc_data = ACC
  # sampling_freq = 200
  # print.updates = T
  # plot.updates  = T
  # axis.of.interest = "y"
  # smooth_type = "standard"

  ## Stop if not
  if( smooth_type == "num.flaps" & axis.of.interest != "z"){
    stop( "axis must be 'z' when using number of flaps as smoothing parameter")
  }

  ## Axis
  aoi = which ( names(acc_data) == axis.of.interest)

  ## Filter z-axis using running mean over 5 data
  acc_smth =runningmeanACC( acc_data[,aoi] ,smth =  5)
  if ( print.updates){
    print( "smoothed data")
  }

  ## Find max acceleration peaks (downstroke)
  {
    min = PeakDetect(acc_smth, 0.9)$min
    max = PeakDetect(acc_smth, 0.9)$max
    if( print.updates){
      print( "peaks detected")
    }
  }

  ## Stationary periods
  {
    max1 = max[1:(length(max)-1)]
    max2 = max[2:length(max)]
    max3 = max2 - max1
    max4 = which(max3> sampling_freq)
    if ( length(max4) > 0){
      max5 = max4+1
      vec = vector()
      for ( j in 1:length(max4)){
        vec2 = c(max[max4[j]]:max[max5[j]])
        vec = c(vec, vec2)
      }
      vec3 = unique(vec)
      acc_smth =runningmean( acc_data[,aoi] ,lag =  5)
      min = PeakDetect(acc_smth, 0.9)$min
      max = PeakDetect(acc_smth, 0.9)$max
    }
    if( print.updates){
      print( "found max acceleration peaks")
    }
    if ( plot.updates){
      plot(acc_smth, type = "l")
      points(acc_smth[min]~min , col = "blue")
      points(acc_smth[max]~max , col = "red")
    }
  }

  ## Flap Frequency
  {
    op = options(digits.secs = 3)
    td = acc_data$time[min[2:length(min)]]-
      acc_data$time[min[1:(length(min)-1)]]
    ff = 1/as.numeric(td)
    if(plot.updates){
      plot(ff[ff<30])
    }
  }

  ## Remove gravity
  {
    times = ceiling ( nrow(acc_data)/ split.val) # how many times do we need to split our data?  (Because of super low running speeds and in some cases, impossible to run code with my laptop if too large)
    if ( times >1 ){ # If more than once
      gravity = vector() # set up a vector
      for ( j in 1:times){ # and for each split
        obj = acc_smth[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
        max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector
        if ( smooth_type[1] == "num.flaps" ){
          gravity = c(gravity, runningmeanACC(obj,smth_type = "num.flaps" ,
                                              smth = 15, maxima =  max.loop, split.val = split.val,j = j)
          )# use running mean function, and combine to previous splits
        } else {
          gravity = c(gravity,runningmeanACC(obj, smth_type = "standard" , 401, max.loop))
        }
      }
    } else { # if the data do not need to be split.
      if ( smooth_type[1] == "num.flaps" ){
        gravity = runningmeanACC(acc_smth, smth_type = "num.flaps" , 15, max ) # Do the same
      } else {
        gravity = runningmeanACC(acc_smth, smth_type = "standard" , 401, max )
      }
    }
    ACC_rmean =  acc_smth - gravity[1:length(acc_smth)]

    if(plot.updates){
      plot(ACC_rmean,type = "l")
      points(ACC_rmean[min]~min , col = "blue")
      points(ACC_rmean[max]~max , col = "red")
    }
  }
  ## INTEGRATION
  {
    maxtab = ACC_rmean[max]
    mintab = ACC_rmean[min]
    acc_rmean_ms = rep(NA, nrow(acc_data))
    acc_rmean_ms[!is.na(ACC_rmean)] = na.omit(ACC_rmean) * 9.80665
    foo = str_split_fixed(acc_data$time, "\\.",2)[,1]
    splitstringTIME = str_split_fixed(foo,":",3)
    Time = acc_data$time
    velocity_rmean = rep(NA, nrow(acc_data))
    only = !is.na(acc_rmean_ms) # only non NA
    velocity_rmean[only] =pracma::cumtrapz(Time[only], as.vector(acc_rmean_ms[only]))
    if ( plot.updates ){
      plot(velocity_rmean[seq(1,length(velocity_rmean),length.out = 2000)])
    }
    velocity_rmean = velocity_rmean # and into data frame
    if ( times >1 ){ # If more than once
      rmeanVel = vector() # set up a vector
      for ( j in 1:times){ # and then, for each split
        obj = velocity_rmean[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
        max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector
        if ( smooth_type[1] == "num.flaps" ){
          rmeanVel = c(rmeanVel, runningmeanACC(obj, smth_type = "num.flaps" , 15, max.loop, split.val = split.val , j = j)) # use running mean function, and combine to previous splits
        } else {
          rmeanVel = c(rmeanVel,runningmeanACC(obj, smth_type = "standard" , 401, max.loop))
        }
      }
    } else { # if the data do not need to be split.
      if ( smooth_type[1] == "num.flaps" ){
        rmeanVel = runningmeanACC(velocity_rmean, smth_type = "num.flaps" , 15, max ) # Do the same
      } else {
        rmeanVel = runningmeanACC(velocity_rmean, smth_type = "standard" , 401, max )
      }
    }
    if ( plot.updates) {
      plot(velocity_rmean, type = "l")
      lines(rmeanVel)
    }
    if( print.updates){
      print("calculated dba")
    }
  }
  ## remove running mean velocity ( drift)
  velocity_rmean_removed =  velocity_rmean - rmeanVel[1:length(velocity_rmean)]
  if ( plot.updates ){
    plot(velocity_rmean_removed[sort(sample(1:length(velocity_rmean_removed),2000))])
  }

  ## Second integration (displacement)
  {
    displacement_rmean = rep(NA, nrow(acc_data))
    only = !is.na(velocity_rmean_removed)
    displacement_rmean[only] =pracma::cumtrapz(Time[only], as.vector(velocity_rmean_removed[only]))
    displacement_rmean = displacement_rmean # and into data frame
  }

  ## running mean to remove changes in displacement
  {
    if ( times >1 ){ # If more than once
      rmeanDis = vector() # set up a vector
      for ( j in 1:times){ # and then, for each split
        obj = displacement_rmean[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
        max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector

        if ( smooth_type[1] == "num.flaps" ){
          rmeanDis = c(rmeanDis, runningmeanACC(obj, smth_type = "num.flaps" , 15, max.loop, split.val = split.val , j = j)) # use running mean function, and combine to previous splits
        } else {
          rmeanDis = c(rmeanDis,runningmeanACC(obj, smth_type = "standard" , 401, max.loop))
        }
        #   print(paste ( j , "/" , times))
      }
    } else { # if the data do not need to be split.

      if ( smooth_type[1] == "num.flaps" ){
        rmeanDis = runningmeanACC(displacement_rmean, smth_type = "num.flaps" , 15, max ) # Do the same
      } else {
        rmeanDis = runningmeanACC(displacement_rmean, smth_type = "standard" , 401, max )
      }
    }
    if(print.updates){
      print( "removed changes in displacement")
    }
    if(plot.updates){
      plot(displacement_rmean)
      lines(rmeanDis, col = "red")
    }
    displacement_rmean_removed = displacement_rmean - rmeanDis[1:length(displacement_rmean)]
    if(plot.updates){
      plot(displacement_rmean_removed, type = "l")
    }
  }

  ## BUTTERWORTH FILTER
  {
    but = signal::butter( 4, 2.5/(sampling_freq/2),'high')
    butr = rep(NA, nrow(acc_data))
    only = !is.na(displacement_rmean_removed) # only non NA
    butr[only] = signal::filtfilt(but, displacement_rmean_removed[only])

    data_but =  butr # and into data frame
    if ( plot.updates){
      plot(data_but, type = "l")
    }
    amp_rmean = rep(NA, length(max))
    addn = 11
    start= 8
    fin = length(max) - 6
    for ( i in start:fin) {
      shift = round((200/ff[i])/1/8)
      amp_rmean[i] = max(data_but[max[i]:(max[i+1] +addn)] ) -
        min(data_but[(max[i] - shift):(max[i+1] - shift)])
    }
    if( print.updates){
      print("fin butterworth")
    }
  }

  ## interpolate
  int = signal::interp1(1:length(maxtab), maxtab, 1:length(maxtab))
  p_p_z = int-mintab[1:length(int)]

  # RETURN DATA
  if( length( ff) != length(p_p_z)){
    ff = c(ff,NA)
  }
  ret = cbind( amp = amp_rmean*1000, p_p_z, ff, time = Time[max])
  return( list ( ret ,
                 displacement_rmean,
                 rmeanDis,
                 displacement_rmean_removed,
                 velocity_rmean,
                 rmeanVel,
                 velocity_rmean_removed
  ))
}


