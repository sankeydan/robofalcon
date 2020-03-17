intersection = function(x1,x2){
  above<-x1>x2
  # Points always intersect when above=TRUE, then FALSE or reverse
  intersect.points<-which(diff(above)!=0)
  # Find the slopes for each line segment.
  x1.slopes<-x1[intersect.points+1]-x1[intersect.points]
  x2.slopes<-x2[intersect.points+1]-x2[intersect.points]
  # Find the intersection for each segment.
  x.points<-intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes-x2.slopes))
  return(floor(x.points[1]))

}
