# data
{
  load(file.path(fold, files[i]))
  if( spl[i,2] == "p" ){
    dat = data.list[[1]]
  } else {
    dat = data
  }
  dims = dim(dat)
}

# empty objects
{
  # objects for if( d2site < 500) below
  in.flock.matr = matrix(NA, dims[1],dims[3])
  d2site = 0
  d1 = t(dat[1,1:2,])
  site1 = c( lon = mean(d1[,"lon"],na.rm=T),
             lat = mean(d1[,"lat"],na.rm=T))
  maxdist = vector()
}

# loop
for ( j in 1:dims[1]){
  #j=192

  if( d2site < 500){ #see methods for 500 justification

    # deal with missing data
    dat.all = t(dat[j,1:2,])
    dat.all.complete = dat.all[complete.cases(dat.all),]
    dims.comp = dim(dat.all.complete)
    nam.all      = dimnames(dat.all)[[1]]
    nam.complete = dimnames(dat.all.complete)[[1]]
    whi = which ( nam.all %in% nam.complete)

    # convert to metres for groupDetect
    dat.metres = cbind( x = get_dist( dat.all.complete[,"lon"],
                                      site1["lat"],
                                      site1["lon"],
                                      site1["lat"],method = "distance"),
                        y = get_dist( site1["lon"],
                                      dat.all.complete[,"lat"],
                                      site1["lon"],
                                      site1["lat"],method = "distance"))

    # detect membership
    dist.mat = distance(dat.metres)
    group.mat = matrix(as.numeric(dist.mat<split.dist),nrow(dist.mat))
    group.vec = na.omit(as.vector( group.mat))
    if ( any(group.vec == 1)){
      # use Igraph to group individuals
      diag(group.mat) = 0
      net <- igraph::graph.adjacency(group.mat, mode='undirected',diag=FALSE)
      weights <- igraph::E(net)$weight
      coms <- suppressWarnings( igraph::edge.betweenness.community(net,weights=weights))
      membership = coms$membership
    } else {
      membership = 1:nrow(dat.metres)
    }

    # manipulate and add to matrix
    mainflock = which.max(table(membership))
    in.flock = membership == mainflock
    incl.missing      = rep(NA,dims[3]) # add back in any missing individuals (i.e. NA's)
    incl.missing[whi] = in.flock
    in.flock.matr[j,] = incl.missing

    d2site = get_dist( mean(dat.all.complete[in.flock,"lon"]),
                       mean(dat.all.complete[in.flock,"lat"]),
                       site1["lon"],site1["lat"],method = "distance")

    # takestock
    k = j

    # save maxdist
    whi.memb = which(membership == mainflock)
    foo =  max(na.omit(as.vector(dist.mat[whi.memb,whi.memb]))) /dims[3]
    maxdist = c(maxdist,foo)

    #plot
    # XY = xylims(dat.metres)
    #
    # plot( dat.metres[,1],dat.metres[,2],col = membership, ylim = XY[[2]],xlim = XY[[1]])
    # for ( i in 1:nrow(dat.metres)){
    #   x = dat.metres[i,1]
    #   y = dat.metres[i,2]
    #   draw.circle(x,y,split.dist,border =membership[i])
    # }


  }
}


#save maxdists
foo = max(maxdist)
maxdists[i] = foo

data.mainflock = dat[1:k,,]
for ( j in 1:k){
  # j = 1
  in.f = in.flock.matr[j,]
  data.mainflock[j,,!in.f] = NA
}
data.entireflock = dat[1:k,,]
data.list = list(data.mainflock, data.entireflock)
dm = data.mainflock
de = data.entireflock
