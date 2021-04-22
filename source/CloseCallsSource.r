li=list()
for ( i in pred.flights){
  #i=9
  
  # Data  
  load ( file.path (fold , files[i]))
  falc = data.list[[2]]
  load ( file.path (fold.remain , files[i]))
  pidge = data.list[[1]]
  nam = dimnames(pidge)[[3]]
  n.pidge = dim(pidge)[3]
  len = dim(pidge)[1]
  
  # metres
  load ( file.path(PROJHOME , "metadata", "site-xy.rda"))
  site.xy = site.xy-0.01
  pidgem = array (NA, c(len,2,
                        n.pidge))
  dist2falcm = array(NA, c(len,
                           n.pidge))
  falcm = cbind ( x = get_dist(falc[ , "lon"], site.xy[2],site.xy[1],site.xy[2],method="distance"),
                  y = get_dist(site.xy[1] , falc [ ,"lat"],site.xy[1],site.xy[2],method="distance"))
  falc.head = get_heading_cartesian(falcm[,1],falcm[,2])
  for ( j in  1:n.pidge){
    pidgem[,,j] = cbind ( x = get_dist(pidge [ , "lon", j], site.xy[2],site.xy[1],site.xy[2],method="distance"),
                          y = get_dist(site.xy[1] , pidge[ ,"lat",j],site.xy[1],site.xy[2]  ,method="distance"))
    dist2falcm[,j] = cartesian_dist(pidgem [ ,1, j],  pidgem[ ,2,j],falcm[,"x"],falcm[,"y"] ,method="distance")
  }
  
  
  # Closest indivs
  closest = t(apply ( dist2falcm,1,function(x)   order(x)))
  d.closest = array ( NA, c(len,2,n.pidge))
  for ( k in 1:len){
    d.closest[k,1,] = pidgem[k,1,closest[k,]]
    d.closest[k,2,] = pidgem[k,2,closest[k,]]
  }
  

  # Heading with respect to predatpr
  heads = get_heading_cartesian(d.closest[,1,1],d.closest[,2,1]) 
  falc.diff =falc.head - heads
  falc.diff = ifelse ( falc.diff > pi, falc.diff-2*pi,falc.diff)
  falc.diff = ifelse ( falc.diff < -pi,falc.diff+2*pi,falc.diff)
  turn2falcs = sign ( falc.diff)
  
  # save output vars for different decisions
  output.vars = c("x","y","d2p","d2p.normal","d2p.diff", "d2p.prev", "group.d2p","closest")
  turning.decisions = c("awayfromfalc", "towardFutCentroid", "align")
  save.mat = array( NA,c( length(heads), length(output.vars),length(turning.decisions),n.pidge),dimnames = list(NULL,output.vars,turning.decisions,1:n.pidge))
  
  for ( k in 3:(dim(pidge)[1]-1)){
    #k=close.k[16]

    for ( l in 1:dim(pidge)[3]){
      
      closest.heading = heads[k]
      closest.turn = ang.diff(heads[k], heads[k+1], fuN = "turning")
      closest.speed = cartesian_dist(pidgem[k:(k+1),1,closest[k,l]],pidgem[k:(k+1),2,closest[k,l]],hz=1,method="speed")[2]
      
      
      ##### AWAY FROM FALC #####
      ie = ifelse ( turn2falcs[k] == 1 , max.turn.rate, -max.turn.rate)
      ie = ie*-1
      hea = closest.heading+ie
      hea = ifelse ( hea > pi, hea-2*pi,hea)
      hea = ifelse ( hea < -pi,hea+2*pi,hea)
      multip = t(matrix( c(-1,-1,1,-1,1,1,-1,1),2))[max ( which(hea>=seq(-pi,pi+0.001,length.out = 5))),]
      predicted.lon = d.closest[k,1,l] + abs(closest.speed  * sin ( hea))* multip[2]
      predicted.lat = d.closest[k,2,l] + abs(closest.speed  * cos ( hea))* multip[1]
      
      save.mat [k,"x"         ,"awayfromfalc",l]  = predicted.lon
      save.mat [k,"y"         ,"awayfromfalc",l]  = predicted.lat
      save.mat [k,"d2p"       ,"awayfromfalc",l] = cartesian_dist(   save.mat [k,"x","awayfromfalc",l] ,save.mat [k,"y","awayfromfalc",l] ,falcm[k+1,1],falcm[k+1,2],method = "distance")
     save.mat [k,"group.d2p" ,"awayfromfalc",l] = mean ( dist2falcm[k+1,closest[k+1,-l]],na.rm=T)
      
      ## TOWARD CENTROID
      neimi = pidgem[(k-2):(k+0),,-closest[k,l]]
      cent = t(apply ( neimi, 1, function(x) rowMeans(x,na.rm = T)))
      gh = get_heading_cartesian(cent[,1],cent[,2],indivs = 1)
      ta = turnang(cent,cartesian = T)[1]
      cd = cartesian_dist(cent[,1],cent[,2],hz=1,method="speed")[3]
      gh = get_heading_cartesian(cent[,1],cent[,2],indivs = 1)[2]
      head = gh+ta
      head = ifelse ( head > pi, head-2*pi,head)
      head = ifelse ( head < -pi,head+2*pi,head)
      multip = t(matrix( c(-1,-1,1,-1,1,1,-1,1),2))[max ( which(head>=seq(-pi,pi,length.out = 5))),]
      predicted.lon = cent[3,1] + abs(cd  * sin ( head))* multip[2]
      predicted.lat = cent[3,2] + abs(cd  * cos ( head))* multip[1]
      
      # does predicted lon / lat clash with an avoidance response? 
      save.dist = vector()
      for ( j in closest[k,-l]){ # 5 closest neighbours
        #j=3
        nei = pidgem[(k-2):(k+0),,j]
        gh = get_heading_cartesian(nei[,1],nei[,2],indivs = 1)
        ta = turnang(nei,cartesian = T)[1]
        cd = cartesian_dist(nei[,1],nei[,2],hz=1,method="speed")[3]
        gh = get_heading_cartesian(nei[,1],nei[,2],indivs = 1)[2]
        head = gh+ta
        head = ifelse ( head > pi, head-2*pi,head)
        head = ifelse ( head < -pi,head+2*pi,head)
        multip = t(matrix( c(-1,-1,1,-1,1,1,-1,1),2))[max ( which(head>=seq(-pi,pi+0.001,length.out = 5))),]
        nei.lon = nei[3,1] + abs(cd  * sin ( head))* multip[2]
        nei.lat = nei[3,2] + abs(cd  * cos ( head))* multip[1]
        save.dist = c(save.dist,cartesian_dist(predicted.lon,predicted.lat,nei.lon,nei.lat))
      }
      if( !any( na.omit(save.dist) < min.avoid.dist)){
        
        aim2cent = get_heading_cartesian(d.closest[k,1,l],d.closest[k,2,l],predicted.lon, predicted.lat,indivs = 2)
        hea = aim2cent-closest.heading
        hea = ifelse ( hea > pi, hea-2*pi,hea)
        hea = ifelse ( hea < -pi,hea+2*pi,hea)
        hea = ifelse ( abs(hea)>max.turn.rate , sign(hea)*max.turn.rate, hea)
        hea = closest.heading + hea
        hea = ifelse ( hea > pi, hea-2*pi,hea)
        hea = ifelse ( hea < -pi,hea+2*pi,hea)
        multip = t(matrix( c(-1,-1,1,-1,1,1,-1,1),2))[max ( which(hea>=seq(-pi,pi+0.001,length.out = 5))),]
        predicted.lon = d.closest[k,1,l] + abs(closest.speed  * sin ( hea))* multip[2]
        predicted.lat = d.closest[k,2,l] + abs(closest.speed  * cos ( hea))* multip[1]
        
        save.mat [k,"x"         ,"towardFutCentroid",l]  = predicted.lon
        save.mat [k,"y"         ,"towardFutCentroid",l]  = predicted.lat
        save.mat [k,"d2p"       ,"towardFutCentroid",l] = cartesian_dist(   save.mat [k,"x","towardFutCentroid",l] ,save.mat [k,"y","towardFutCentroid",l] ,falcm[k+1,1],falcm[k+1,2],method = "distance")
       save.mat [k,"group.d2p" ,"towardFutCentroid",l] = mean ( dist2falcm[k+1,closest[k+1,-l]],na.rm = T)
      }
      ############ ALIGN
      gh = get_heading_cartesian(cent[,1],cent[,2],indivs = 1)[2]
      hea = gh-closest.heading
      hea = hea+closest.heading
      hea = ifelse ( hea > pi, hea-2*pi,hea)
      hea = ifelse ( hea < -pi,hea+2*pi,hea)
      multip = t(matrix( c(-1,-1,1,-1,1,1,-1,1),2))[max ( which(hea>=seq(-pi,pi+0.001,length.out = 5))),]
      predicted.lon = d.closest[k,1,l] + abs(closest.speed  * sin ( hea))* multip[2]
      predicted.lat = d.closest[k,2,l] + abs(closest.speed  * cos ( hea))* multip[1]
      save.mat [k,"x"         ,"align",l]  = predicted.lon
      save.mat [k,"y"         ,"align",l]  = predicted.lat
      save.mat [k,"d2p"       ,"align",l] = cartesian_dist(   save.mat [k,"x","align",l] ,save.mat [k,"y","align",l] ,falcm[k+1,1],falcm[k+1,2],method = "distance")
       save.mat [k,"group.d2p" ,"align",l] = mean ( dist2falcm[k+1,closest[k+1,-l]],na.rm = T)
      
      
      
      # PLOTs?
      if(plott & l == 1){
        xyss = xylims (cbind ( x =  c(d.closest[k,1,], falcm[k,1],
                                      save.mat[k,"x","align"            ,l],
                                      save.mat[k,"x","towardFutCentroid",l],
                                      save.mat[k,"x","awayfromfalc"     ,l]
        ),
        y =  c(d.closest[k,2,], falcm[k,2],
               save.mat[k,"y","align"            ,1],
               save.mat[k,"y","towardFutCentroid",1],
               save.mat[k,"y","awayfromfalc"     ,1]
        )))
        plot ( d.closest[k,1,1],d.closest[k,2,1], xlim = xyss[[1]], ylim = xyss[[2]],  pch = 19)
        for ( o in 2:n.pidge){
          points( d.closest[k,1,o],d.closest[k,2,o], pch = 19)
        }
        points( falcm[k,1], falcm[k,2], col = "green" , pch = 19)
        points( d.closest[k,1,1],d.closest[k,2,1], col = "red", pch = 19)
        points( save.mat[k,"x","align"            ,l], save.mat[k,"y","align"            ,l],pch=19, col = "blue")
        points( save.mat[k,"x","towardFutCentroid",l], save.mat[k,"y","towardFutCentroid",l],pch=19, col = "orange")
        points( save.mat[k,"x","awayfromfalc"     ,l], save.mat[k,"y","awayfromfalc"     ,l],pch=19, col = "purple")
      }
    }

  }
  li[[ which ( pred.flights == i)]] = save.mat
  
}

