
distance = function( XY){
  #objects
  grp.size = nrow(XY)
# functions
dis = function(p1.x, p2.x, p1.y, p2.y){
  sqrt( (p1.y -p2.y)^2 + (p1.x - p2.x)^2)
}
dis.mat = array ( NA, c(grp.size, grp.size)) # a matrix of distances between each individual
for ( i in 1:grp.size){ # for each focal individual
  for ( j in 1:grp.size){ # for each neighbour
    if ( i != j){ # if focal is not neighbour
      dis.mat[i,j]  =  dis( XY[i,1] , XY[j,1], XY[i,2] , XY[j,2]) # use distance function (classic trig) to calculate distance.
    }
  }
}
return(dis.mat)
}
