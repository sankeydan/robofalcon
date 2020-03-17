

PeakDetect = function(v,delta){
mn = Inf
mx = -Inf
mnpos = NaN
mxpos = NaN

v[is.na(v)] = mean(v, na.rm = T)

maxtab = list(mx, mxpos)
mintab = list(mn, mnpos)
min = vector()
max = vector()
vmin = rep(NA, length(v))
vmax = rep(NA, length(v))


lookformax = T

for (i in 1:length(v)){
  
  val = v[i]
  
  if (val > mx){
    
    mx = val
    mxpos = i
  }
  
  if (val < mn){
    mn = val
    mnpos = i
  }
  
  
  if (lookformax){
    
    if (val < mx-delta){
      
      min = c(min,mnpos)
      mn = Inf
      lookformax = F
    }
  }else {
    if (val > mn+delta){
      
      max = c(max,mxpos)
      mx = -Inf
      lookformax = T
    }
  }
  
}
  # vmin[!is.na(v)] = min
  # vmax[!is.na(v)] = max
  
  return(list (min = min, max = max) )
}
