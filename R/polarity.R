polarity = function(p1,p2){
len = nrow(p1)
# Movement per timestep , dt
dt1 = matrix(NA, len - 1, 2)
dt2 = matrix(NA, len - 1, 2)
dt1[,1] = p1[2:len,1] - p1[1:(len-1),1]
dt1[,2] = p1[2:len,2] - p1[1:(len-1),2]
dt2[,1] = p2[2:len,1] - p2[1:(len-1),1]
dt2[,2] = p2[2:len,2] - p2[1:(len-1),2]

# Normalise
norm1 = matrix(NA, len-1,2)
norm2 = matrix(NA, len-1,2)
norm1[,1] = apply(cbind(dt1[,1], dt1[,2]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
norm1[,2] = apply(cbind(dt1[,2], dt1[,1]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
norm2[,1] = apply(cbind(dt2[,1], dt2[,2]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})
norm2[,2] = apply(cbind(dt2[,2], dt2[,1]), 1, function ( x) { x[1] / sqrt(x[1]^2 + x[2]^2)})

return(
  apply( cbind( norm1, norm2),1,function(x){
    x[1:2] %*% x[3:4]
  })
)
}
