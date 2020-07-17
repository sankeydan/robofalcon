unq = unique ( dat$study.flight)
vecP = vector()
for ( i in 1:length(unq)){
  #i=10
  dat2 = dat[dat$study.flight == unq[i],]
  unqP = unique(dat2$pigeon)
  lenP = length(unqP)
  matD2P = matrix( dat2$dist2pred , ncol = lenP,dimnames = list  (NULL, unqP))
  D2Psmallest = t(apply ( matD2P , 1, function ( x){ 
    x[x>=median(x,na.rm = T)] = NA
    x
  }))
  vecP = c ( vecP , as.vector( D2Psmallest))
}
length(vecP)
dat$dist2predNearest = vecP
