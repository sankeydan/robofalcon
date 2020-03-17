
groupDetect = function (XY ,
                        group.num = NULL ,
                        p.val = 0.05 ,
                        rank.type = c( "interaction", "dist.diff", "nei.2.nei"),
                        method = c("groupDetect" , "gmm"),
                        output = T,
                        plot = T,
                        out.at.one.method = "grubbs",
                        min.sep.dist.type = c( "maxnearestneighbour", "sd.multiply", "choose", "none"),
                        msd.multiplier = -1,
                        min.sep.dist = NULL,
                        max.sep.dist = NULL,
                        output.sim = F,
                        hard.clustering = T

){



  ##### VARIABLES

  # XY = dat.metres
  # group.num = NULL  # data from Danai's function
  # rank.type = "interaction"
  # min.sep.dist.type = "maxnearestneighbour"
  # max.sep.dist = 10
  # plot = T
  # p.val = 0.05
  # msd.multiplier = -1
  # output = T
  # min.sep.dist = 5
  # hard.clustering = T

  ##### OBJECTS

  XY =  as.data.frame(XY)
  grp.size = nrow(XY)

  if (! is.null( min.sep.dist)){
    min.sep.dist.type = "choose"
  }


  ##### LIBRARIES

  library(MASS)
  library(text2vec)

  ##### MATRIX OF DISTANCES

  dis.mat = array ( NA, c(grp.size, grp.size)) # a matrix of distances between each individual
  for ( i in 1:grp.size){ # for each focal individual
    for ( j in 1:grp.size){ # for each neighbour
      if ( i != j){ # if focal is not neighbour
        dis.mat[i,j]  =  dis( XY[i,1] , XY[j,1], XY[i,2] , XY[j,2]) # use distance function (classic trig) to calculate distance.
      }
    }
  }

  ### RANK DISTANCES

  rank_actual_dist = array(NA, c(grp.size, grp.size-1)) # store actual distances to each neighbour, in rank order
  rank = matrix(NA, grp.size , (grp.size)) # store rank order, beginning with focal id

  rankD.dist.diff = array(NA, c(grp.size, grp.size-1)) # matrix to store rank distances, dist.diff (neighbour r+1) minus (neighbour r)
  rankD.n2n = array(NA, c(grp.size, grp.size-1)) # matrix to store rank distances, neighbour ot neoighbour
  rankD.interaction =  array(NA, c(grp.size, grp.size-1)) # matrix to store rank distances, neighbour ot neoighbour

  diag(dis.mat) <- 0

  for ( i in 1:grp.size){
    rank[i,] = c( order(dis.mat[i,])[1:(grp.size)]) # rank order, beginning with focal id
    rank_actual_dist[i,] = dis.mat[i,rank[i,2:ncol(rank)]]

    rankD.dist.diff[i,] = dis.mat[i,rank[i,2:ncol(rank)]] - dis.mat[i,rank[i,1:(ncol(rank)-1)]] # These are the three different detection methods
    rankD.n2n[i,] = dis.mat[ cbind(rank[i,1:(ncol(rank)-1)],rank[i,2:ncol(rank)]) ]
    rankD.interaction[i,] = rankD.dist.diff[i,] * rankD.n2n[i,]
  }

  # Only need one ranking index (or, type). Moving forward, which are we interested in?

  if ( rank.type[1] == "dist.diff"){
    rD = rankD.dist.diff
  } else { if ( rank.type[1] == "nei.2.nei"){
    rD = rankD.n2n
  } else { if ( rank.type[1] == "interaction")
    rD = rankD.interaction}
  }


  #######################

  #### OUTLIER DETECTION

  #######################

  outlier.vec = rep(NA, grp.size) # vector to minimum outlier
  outlier.mat = array(0, c(grp.size, grp.size)) # matrix to store links between individuals
  outlier.threshold = list() # NOT FINISHED want to store all values of


  ###### MINSEP DIST

  #  to exclude outliers within a minimum radius of focal individual

  actual_dist = as.vector(rank_actual_dist)

  if ( min.sep.dist.type[1] == "none" ){
    min.sep.dist  = 0
  }
  if ( min.sep.dist.type[1] == "choose" ){
    min.sep.dist  = min.sep.dist
  }
  if ( min.sep.dist.type[1] == "sd.multiply" ){
    if ( is.null(msd.multiplier) ){
      stop ( "if min.sep.dist.type == 'sd.multiply' then msd.multiplier must be given a numerical value")
    }
    min.sep.dist  = mean(actual_dist) + (sd(actual_dist) * msd.multiplier )
  }
  if ( min.sep.dist.type[1] == "maxnearestneighbour"){
    out.at.one = out(rD[,1], p.val = p.val, out.method =  out.at.one.method) # these are the individuals which have outliers at point 1.
    rem.after.one = c(1:grp.size)[ !(1:grp.size %in% out.at.one)]
    outlier.vec[out.at.one] = 1
    min.sep.dist = max( rank_actual_dist[rem.after.one,1])
  }

  ###########################

  ##### USE while() AND OUTLIER DETECTION TO ESTIMATE EACH INDIVIDUAL'S GROUP SIZE

  ###########################

  # excecute the integrated probability density function,
  # with correction factor to make estimate more conservative.
  # from 2:ncol as one has already been treated


  # ADD LINKS TO A NETWORK, BASED ON LOCAL NEIGHBOUR PREDICTIONS

  for ( i in 1:dim(XY)[1] ){ # for each tracked individual
    #i=5

    # run through from start to detect first outlier
    j = 2
    h = rD[i,1:j]
    #p = rD[i,]
    outl = outliers::chisq.out.test(h)
    TF = F
    while (  length ( which ( c( outl$p.value > p.val, TF == F , j < ncol(rD)) ==F)) <2) {
      j = j + 1
      h = rD[i,1:j]
      outl = outliers::grubbs.test(h,two.sided = F)
      ov = which.max( h)
      #plot(h)
      TF = rank_actual_dist[i,ov] > min.sep.dist
      TF = ifelse  ( j == ncol(rD),T, TF)
      ov = ifelse  ( j == (ncol(rD)),grp.size-1,ov)
    }
    ov = ifelse(  outl$p.value > p.val, grp.size,ov)
    outlier.vec[i] = ov

    if ( ov > 1){
      atm = t(combn(rank[i,1:(ov)], 2)) # then put links between all combinations of individuals we have
      #evidence for being in the focal individual's group
      outlier.mat[atm] = outlier.mat[atm] +1

    }
  }

  # Detection from igraph (doesn't need to be. Can update this soon). Always use hard clustering.
  if ( hard.clustering ){

    outlier.mat[outlier.mat>1] = 1

    # add links if below the minimum/maximum separation distance
    for ( j in 1:nrow(outlier.mat)){
      # j=1
      below.min.sep = dis.mat[j,]<min.sep.dist
      outlier.mat[j,below.min.sep] = 1
      if ( !is.null(max.sep.dist)){
        above.max.sep = dis.mat[j,]>max.sep.dist
        outlier.mat[j,above.max.sep] = 0
      }
    }
    diag(outlier.mat) = 0
  }

  # use Igraph to group individuals
  net <- igraph::graph.adjacency(outlier.mat, mode='undirected',diag=FALSE)
  weights <- igraph::E(net)$weight
  coms <- suppressWarnings( igraph::edge.betweenness.community(net,weights=weights))

  #################

  #### PLOT

  ################

  if ( plot ){

    par(mfrow = c(1,3)) # set up plot

    ## RANK INDEX


    plot(1:(grp.size-1), seq(0,max(na.omit(as.vector(rD))),
                             length.out =  grp.size-1),
         type = "n",
         ylim = c(0, max(na.omit(as.vector(rD)))*1.2),
         ylab = rank.type,
         xlab = "neighbour rank")  #plot a blank

    for ( i in 1:grp.size){
      if ( length( group.num )> 0){
        lines(jitter(rD[i,], 3 ), col = group.num[i] , lwd = 2) # finish the plot
      } else{
        lines(jitter(rD[i,], 3 ), col = i , lwd = 2) # finish the plot
      }
    }

    points(jitter(outlier.vec), jitter(rep(max(na.omit(as.vector(rD)))*1.1, length(outlier.vec))), col = group.num, pch = 19)

    ## PLOT A MAP

    if ( length( group.num )> 0){
      plotmap(XY, group.num, "real")
    } else{
      plotmap(XY, 1:grp.size)
    }
    plotmap(XY, coms$membership, "ourmethod")
  }




  ###################

  ### NETWORK ANALYSIS

  ###################




  ##### METHODS

  if ( method [1] == "gmm")  {
    library(mclust)
    # XY = cbind( c(rnorm(10,5),
    #               rnorm(20,15)),
    #             c(rnorm(10,5),
    #               rnorm(20,15)))
    # plot( XY[,1],XY[,2])
    BIC = mclustBIC(XY)
    mod1 = Mclust(XY,x = BIC)
    mod1$classification
    return(mod1$classification)
  }

  if ( method [1] == "groupDetect")  {

    if ( output){
      return( list(coms$membership, outlier.mat, rank) )
    }
    if ( output.sim){
      if ( is.null(group.num) ){
        stop( "need to input expected group numbers")
      } else {

        sim = test_similarity(group.num = group.num, observed.group.num = coms$membership)

        par(new = F)
        plot.new()
        legend("topright" , legend = paste(
          " pairwise similarity = ", round(sim[1],2), "\n",
          "group.similarity = ", round(sim[2],2), "\n",
          "exp groups = ", length(unique(group.num)), "\n",
          "obs groups =" , length(unique(coms$membership)), "\n",
          "cfm =" , cor.factor.multiplier, "\n",
          "msd =" , msd.multiplier, "\n"),
          cex = 1,
          bty = "n"
        )

        return( list(coms$membership, outlier.mat, rank, sim) )
      }



    }

  }

  if ( method[1] == "dbscan") {
    dbscan::dbscan(dis.mat, eps = 1/max(dis.mat))
  }

  if ( method[1] == "fpc"){
    fpc::calinhara(dis.mat)
  }

}






