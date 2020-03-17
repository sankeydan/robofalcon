runningmeanACC = function ( x , smth_type = c("standard" , "num.flaps"), smth, maxima = NULL , split.val = NULL, j = NULL){

  ### Variables

  # x = obj
  # smth_type = "num.flaps"
  # smth = 15
  # maxima = max.loop
  # split.val = split.val

  # # # # # # # #


  if( smth %% 2 == 0 ) stop ( "smth is not an odd number") # is smth not divisible by 2
  lag = floor(smth/2) # lag will be the plus and minus value for every mean



  if( smth_type[1] == "standard"){ # If just a standard running mean

    sequence <- seq(1 + lag, length(x) - lag, 1) # sequence of mid points for means (ends cut off)
    locs <- apply(t(sequence), 2, function(x, lag) (x - lag):(x + lag), lag = lag) # a matrix of indexes (for speed)
    runs = apply(matrix(x[locs], nrow(locs), ncol(locs)),2,mean) # mean the indexed values
    ret = (c( rep(NA, lag), runs,rep(NA, lag))) # add the head and tail of the vector and return
                                                #the values left out by the smooting

  }

  if ( smth_type[1] == "num.flaps"){

    if( length( maxima ) < (1 + smth)) { # If there are not enough peaks (flaps) to smooth across

      ret = rep(NA, length(x))  # return a vector of NA
    } else { # Otherwise

      diffmax = maxima[2:length(maxima)]- maxima[1:(length(maxima)-1)] # difference between maxima. Similar generally
      sequence = seq(1 + lag, length(maxima) - lag-1, 1) # Cant do running mean on all values, need to cut off ends.

      diffseq = diffmax[sequence] # Create sequences to bypass using loops

      bot = apply(data.frame(maxima[sequence-lag],maxima[sequence-lag+1],diffseq), 1, function( z){
        round(seq(z[1], z[2], length.out = z[3]))})

      top = apply(data.frame(maxima[sequence+lag],maxima[sequence+lag+1],diffseq), 1, function( z){
        round(seq(z[1], z[2], length.out = z[3]))})

      df = data.frame( b = unlist(bot), t =  unlist(top))
      df = df - ( (j-1) * split.val )
      bt = apply(df,1, function(x){ x[1]:x[2]})


      btt = lapply(bt, function(z){ mean( x[z]) }) # Essentially the whole above section is a round about way of removing loops

      if ( is.null(j)){ # If the data did not have to be trimmed (and hence we would need a j value)
        ret = c(rep(NA, maxima[sequence[1]]),
                unlist(btt),
                rep(NA, (length(x) - maxima[sequence[length(sequence)]+1]) )) # Then return the vector with the ends as NAs
      } else {
        ret = c(rep(NA, (maxima[sequence[1]] - (split.val * (j-1) ) )),
                unlist(btt),
                rep(NA, (length(x) - (maxima[sequence[length(sequence)]+1] - (split.val * (j-1) ) )))) # Otherwise perform a similar process, but allow for changes in sequence number from the loop (hence the use of j)
      }
    }
  }
  return( ret) # Return vector
}




