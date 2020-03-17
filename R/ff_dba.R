###### Peak detection and DBA


ff_dba = function ( acc_data, start = NA,
                   fin = NA, sampling_freq = 200,
                   split.val = 100000,
                    smooth_type = c("standard", "num.flaps"),
                   length.meets.gps = F,
                   test = F){


  # OBJECTS

  # acc_data = ACC
  # sampling_freq = 200
  # split.val = 100000
  # smooth_type = "num.flaps"
  # test = T



  ## Filter z-axis using running mean over 5 data

  acc_smth =runningmeanACC( acc_data$z ,smth =  5)

  print( "smoothed data")


  ## Find max acceleration peaks (downstroke)

  min = PeakDetect(acc_smth, 0.9)$min
  max = PeakDetect(acc_smth, 0.9)$max

  print( "peaks detected")

  # stationary periods

  max1 = max[1:(length(max)-1)]
  max2 = max[2:length(max)]
  max3 = max2 - max1
  max4 = which(max3> sampling_freq) # more than a second between flaps rings alarm bell

  if ( length(max4) > 0){
    max5 = max4+1

    vec = vector()
    for ( j in 1:length(max4)){

      vec2 = c(max[max4[j]]:max[max5[j]])
      vec = c(vec, vec2)
    }

    vec3 = unique(vec)

  #  acc_data = acc_data[-vec3,]

    ## Filter z-axis using running mean over 5 data

    acc_smth =runningmeanACC( acc_data$z ,smth =  5)

    ## Find max acceleration peaks (downstroke)

    min = PeakDetect(acc_smth, 0.9)$min
    max = PeakDetect(acc_smth, 0.9)$max

  }

  print( "found max acceleration peaks")
  #plot( acc_data$z[24000:26000])

  ############

  ## RUNNING MEAN

  ############



  # plot

  if ( test){
    plot(acc_smth, type = "l")
    points(acc_smth[min]~min , col = "blue")
    points(acc_smth[max]~max , col = "red")
  }

  ############

  ## FLAP FREQUENCY

  ###########

  op = options(digits.secs = 3)

  # time difference between minima
  td = acc_data$time[min[2:length(min)]]-
       acc_data$time[min[1:(length(min)-1)]]

  # flap frequency
  ff = 1/as.numeric(td)

  if ( test){
    plot(ff)
  }


  ############

  ## REMOVE GRAVITY

  ############



  # Running mean over 15 flaps


  times = ceiling ( nrow(acc_data)/ split.val) # how many times do we need to split our data?  (Because of super low running speeds and in some cases, impossible to run code with my laptop if too large)

  if ( times >1 ){ # If more than once
    gravity = vector() # set up a vector
    for ( j in 1:times){ # and for each split
      obj = acc_smth[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
      max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector

      if ( smooth_type[1] == "num.flaps" ){
        gravity = c(gravity,
                    runningmeanACC(obj,
                                smth_type = "num.flaps" ,
                                smth = 15,
                                maxima =  max.loop,
                                split.val = split.val,
                                j = j)
        )# use running mean function, and combine to previous splits
      } else {
        gravity = c(gravity,runningmeanACC(obj, smth_type = "standard" , 401, max.loop))
      }
      # print(paste ( j , "/" , times))
    }
  } else { # if the data do not need to be split.

    if ( smooth_type[1] == "num.flaps" ){
      gravity = runningmeanACC(acc_smth, smth_type = "num.flaps" , 15, max ) # Do the same
    } else {
      gravity = runningmeanACC(acc_smth, smth_type = "standard" , 401, max )
    }
  }

  print("removed gravity")



  # plot gravity

  if( test ) {
    plot( gravity[ seq ( 1,length(gravity),length.out = 2000)] )
  }

  # Remove gravity

  ACC_rmean =  acc_smth - gravity[1:length(acc_smth)]


  # Plot

  if ( test){
    plot(ACC_rmean,type = "l")
    points(ACC_rmean[min]~min , col = "blue")
    points(ACC_rmean[max]~max , col = "red")
  }

  # Does this figure look ok? Is the axis centred around zero?



  # new max and min values at peaks (maxtab)

  maxtab = ACC_rmean[max]
  mintab = ACC_rmean[min]



  ############

  # CONVERT TO M/S2 FOR DISPLACMENT

  ############

  acc_rmean_ms = rep(NA, nrow(acc_data))

  acc_rmean_ms[!is.na(ACC_rmean)] = na.omit(ACC_rmean) * 9.80665

  if (  test ){
    plot(acc_rmean_ms[seq(1,length(acc_rmean_ms),length.out=2000)])
  }

  ############

  # CALCULATE DISPLACEMENT USING DOUBLE INTEGRATION OF DORSAL MEASUREMENTS

  ###########

  # Calculate velocity, first integration

  # but first time
  foo = str_split_fixed(acc_data$time, "\\.",2)[,1]
  splitstringTIME = str_split_fixed(foo,":",3)
  Time = acc_data$time

  velocity_rmean = rep(NA, nrow(acc_data))

  only = !is.na(acc_rmean_ms) # only non NA

  # integrate

  velocity_rmean[only] =pracma::cumtrapz(Time[only], as.vector(acc_rmean_ms[only]))

  if ( test ){
    plot(velocity_rmean[seq(1,length(velocity_rmean),length.out = 2000)])
  }
  velocity_rmean = data.frame(z = velocity_rmean) # and into data frame

  # running mean


  if ( times >1 ){ # If more than once
    rmeanVel = vector() # set up a vector
    for ( j in 1:times){ # and then, for each split
      obj = velocity_rmean$z[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
      max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector

      if ( smooth_type[1] == "num.flaps" ){
        rmeanVel = c(rmeanVel, runningmeanACC(obj, smth_type = "num.flaps" , 15, max.loop, split.val = split.val , j = j)) # use running mean function, and combine to previous splits
      } else {
        rmeanVel = c(rmeanVel,runningmeanACC(obj, smth_type = "standard" , 401, max.loop))
      }
      #     print(paste ( j , "/" , times))
    }
  } else { # if the data do not need to be split.

    if ( smooth_type[1] == "num.flaps" ){
      rmeanVel = runningmeanACC(velocity_rmean$z, smth_type = "num.flaps" , 15, max ) # Do the same
    } else {
      rmeanVel = runningmeanACC(velocity_rmean$z, smth_type = "standard" , 401, max )
    }
  }

  # plot if test

  if ( test) {
    plot(velocity_rmean[,1], type = "l")
    lines(rmeanVel)

  }

  print("calculated dba")

  # remove running mean velocity ( drift)

  velocity_rmean_removed =  velocity_rmean$z - rmeanVel[1:length(velocity_rmean$z)]

  if ( test ){
    plot(velocity_rmean_removed[sort(sample(1:length(velocity_rmean_removed),2000))])
  }

  # From Lucy's paper:
  # "After the first integration, a running mean over 15
  # wingbeat cycles was removed from velocity to remove drift"


  # Second integration (displacement)


  displacement_rmean = rep(NA, nrow(acc_data))
  only = !is.na(velocity_rmean_removed)
  displacement_rmean[only] =pracma::cumtrapz(Time[only], as.vector(velocity_rmean_removed[only]))
  displacement_rmean = data.frame(z = displacement_rmean) # and into data frame

  # running mean to remove changes in displacement


  if ( times >1 ){ # If more than once
    rmeanDis = vector() # set up a vector
    for ( j in 1:times){ # and then, for each split
      obj = displacement_rmean$z[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
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
      rmeanDis = runningmeanACC(displacement_rmean$z, smth_type = "num.flaps" , 15, max ) # Do the same
    } else {
      rmeanDis = runningmeanACC(displacement_rmean$z, smth_type = "standard" , 401, max )
    }
  }

  print( "removed changes in displacement")

  # plot if test

  if(test){
    plot(displacement_rmean[,1])
    lines(rmeanDis, col = "red")
  }

  # remove running mean displacement

  displacement_rmean_removed = displacement_rmean$z - rmeanDis[1:nrow(displacement_rmean)]

  # plot if test

  if(test){

    plot(displacement_rmean_removed, type = "l")
  }
  ##########################

  ## BUTTERWORTH FILTER

  ##########################

  but = signal::butter( 4, 2.5/(sampling_freq/2),'high')

  but = rep(NA, nrow(acc_data))

  only = !is.na(displacement_rmean_removed) # only non NA

  but[only] = signal::filtfilt(but, displacement_rmean_removed[only])

  data_but = data.frame(z = but) # and into data frame

  # plot if test

  if ( test){
    plot(data_but$z, type = "l")
  }

  # Shift????

  amp_rmean = rep(NA, length(max))

  addn = 11 # ?????
  start= 8
  fin = length(max) - 6

  for ( i in start:fin) {

    shift = round((200/ff[i])/1/8)

    amp_rmean[i] = max(data_but[,1][max[i]:(max[i+1] +addn)] ) -
      min(data_but[,1][(max[i] - shift):(max[i+1] - shift)])
  }

  print("fin butterworth")
  ############

  # INTERPOLATE

  ############

  int = signal::interp1(1:length(maxtab), maxtab, 1:length(maxtab))
  p_p_z = int-mintab[1:length(int)]

  ############

  # RETURN DATA

  ############

  if( length( ff) != length(p_p_z)){
    ff = c(ff,NA)
  }

  ret = cbind( amp = amp_rmean*1000, p_p_z, ff, time = Time[max])

  print( paste( "done with" , filename))
  return( ret)

}

