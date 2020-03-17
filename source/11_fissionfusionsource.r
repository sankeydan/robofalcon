# data
source(file.path(PROJHOME , "source" , "source.r"))


# split by individual/flight
whi = c(0,which(abs(diff(dat.coh$t)) > 5))
li = list()
li.all = list()
for ( i in 1:(length(whi)-1)){
  #i=2
  li    [[i]] = dat.coh[(whi[i]+1):whi[(i+1)],]
  li.all[[i]] = dat    [(whi[i]+1):whi[(i+1)],]
  d = li.all[[i]]
  print(length(which(is.na(d$lat))))
}

# length of NA without splitting is neglidgibly small ( and smaller than the minimum parameter in the sensitivity analysis)

# empty objects
splitdata = li[[1]][1,]
splitdata$splitornot   = NA
splitdata$failedrejoin = NA
splitdata = splitdata[-
                        which ( is.na(splitdata$splitornot  ) &
                                  is.na(splitdata$failedrejoin) )
                      ,]
#loop
for ( i in 1:length(li)){
  #i=2

  # data
  d = li[[i]]
  d.all = li.all[[i]]
  d.all$split = NA

  # splits and rejoins
  isna = as.numeric(is.na(d[,"lat"]))
  len = rle(isna)
  ioo = len[[2]] # in or out
  len = len[[1]]
  cs  = c(1,cumsum(len)[1:(length(len)-1)])
  son = ioo[1] # split or not
  if ( length(len) > 1){
    for ( k in 2:length(ioo)){
      # k =2
      if ( len[k] > minsplit){
        son[k] = ioo[k]
      } else {
        son[k] = son[k-1]
      }
    }
    son2 = son
    son2[ which(diff(son)==0)+1] = NA
    son2 = ifelse ( son2 == 1, "split", ifelse ( son2 == 0, "rejoin", 0))
    frej = rep(NA, length(len)) # failed rejoin
    frej [which ( ioo == 0 & son == 1)] = "failed rejoin"
    cb = cbind( len , cs,  ioo, son,son2,frej)
    cb2 = cb[2:nrow(cb),]
    cb2
    if ( !is.null(dim( cb2))){
      d2 = d.all[which(d.all$t %in% cb2[,"cs"]),]
      d2$splitornot = cb2[,"son2"]
      d2$failedrejoin = cb2[,"frej"]
    } else {
      d2 = d.all[which(d.all$t %in% cb2["cs"]),]
      d2$splitornot = cb2["son2"]
      d2$failedrejoin = cb2["frej"]
    }
  } else {
    d2 = d.all[1,]
    d2$splitornot = "nosplit"
    d2$failedrejoin = "nosplit"
  }

  splitdata = rbind ( splitdata , d2)
}
splitdata = splitdata[-
                        which ( is.na(splitdata$splitornot  ) &
                                  is.na(splitdata$failedrejoin) )
                      ,]

sd = splitdata[,c("condition", "small.big", "ind.flight", "support.wind", "t", "cross.wind","group.num","Date","pigeon","speed","diff_head","dist2cent","turn2home","turn2falcpos", "turn2falchead","dist2pred","dist2site","dir.rel.ali","centrip","flock.size","splitornot","failedrejoin")]
save( sd, file = file.path ( PROJHOME , "Output", "FissionFusion", paste0( "splitdata-minsplit-" , minsplit, "Hz.rda")))

