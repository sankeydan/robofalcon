xylims = function( data, flight.start.est = 30){
  if ( length(dim(data)) == 3){
  limmax = max( diff( range ( na.omit(data[,"lon",]))),
                diff( range ( na.omit(data[,"lat",]))))/2
  y.start = median( as.vector(data[1:flight.start.est,"lat",]),na.rm = T)
  x.start = median( as.vector(data[1:flight.start.est,"lon",]),na.rm = T)
  ylim = y.start + ( c ( - limmax ,  limmax))
  xlim = x.start + ( c ( - limmax ,  limmax))

  return( list( xlim, ylim ))
  } else {
    limmax = max( diff( range ( na.omit(data[,1]))),
                  diff( range ( na.omit(data[,2]))))/2
    y.start = mean ( range( as.vector(data[,2]),na.rm = T))
    x.start = mean ( range( as.vector(data[,1]),na.rm = T))
    ylim = y.start + ( c ( - limmax ,  limmax))
    xlim = x.start + ( c ( - limmax ,  limmax))

    return( list( xlim, ylim ))
  }
}
