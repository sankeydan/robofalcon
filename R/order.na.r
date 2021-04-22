order.na = function(x){
  x2 = order(x)
  x2[is.na(x)] = NA
  x2
}