#' plot_map
#'
#' A handy and interactive plotter for solo or group GPS tracks. Uses ggmaps to produce images or videos of animal trajectories on a googlemaps background.
#' @param data Solo -data frame or list of data frames with 2 dimensions:
#' longitude & latitude (in that order; columns) For group plots,
#' ensure data frames in the list have the same number of rows
#' Group - 3-dimensional array
#' @param type Solo or Group plots.
#' @param image.or.vid "image" or "video" (sequence of images) to be produced
#' @param tail.size animals represented by current position and the previous n positions (video only)
#' @param frame.size plot every row of data, or every 1/frame size for shorter running speeds and output files (video only)
#' @param zoom zoom = 19 is good for video. Other plot types are worth playing around with, try 13 for approx 5 km square
#' @param lines.or.points On the plotted map, animal trajectory represented by either "lines" or "points"
#' @param maptype "satellite" is default, though check ?get_map in the ggmap package for more options
#' @param wd Specify the working directory that you would like to save the plots into.
#' @param col Colour of plotted trajectory (solo only)
#' @param lwd Thickness of line
#' @param n.indiv Number of individuals. Only use in group context.
#' @param centre Choose your own centre points for the mapform: c(lon, lat)
#' @param file.to.folder TRUE if you want to make videos when happy with other params, but for testing videos set to FALSE
#' @export



plot_map = function( data,  type = c("solo", "group") , data.rem = NULL, falc = NULL, image.or.vid = c( "image", "video"),
                     tail.size = 20, frame.size = 1, wd = getwd(), plot_name = "my_plot" ,
                     lines.or.points = "lines", col = "red" , axis.nums.size = 12, axis.labs.size = 14, n.indiv = NULL,
                     lwd = 2, file.to.folder = T, plot_pred = F, centre = NA , maptype = "satellite", start.at = 0, zoom = 16 ){

  #### CONTENTS

  #1. SOLO

  #1.a image
  #1.b video

  #2. GROUP

  #2.a image
  #2.b video

  #### VARIABLES

  # data = data.list
  # data.rem = data.rem
  # falc = falc
  # type = c("group")
  # image.or.vid = "image"
  # plot_pred = T
  # tail.size = 20
  # frame.size = 1
  # wd = wd
  # plot_name = "my_plot"
  # lines.or.points = "lines"
  # col = c("red","yellow")
  # axis.nums.size = 12
  # axis.labs.size = 14
  # n.indiv = NULL
  # lwd = 2
  # file.to.folder = T
  # centre = NA
  # start.at = 47
  # maptype = "satellite"
  # zoom = 20
  # data = predxy
  # type = "solo"
  # zoom =16
  # centre = site.xy
  # lines.or.points = "points"
  # maptype = "satellite"



  ### HOUSEKEEPING

  dims = dim(data)

  # DEFENSIVE CODING
  # if(  (length(dims) == 3) == (type[1] == "solo")){
  #   stop("Solo data should be list of data.frames or data frame")
  # }
  # if(  (length(dims) != 3) == (type[1] == "group")){
  #   stop("Group data should be 3D array")
  # }


  # libraries
  library(ggmap)

  #### 1. SOLO #####

  if ( type[1] == "solo"){

    if ( class(data) == "data.frame"){
      dat = as.data.frame(data)
      names(dat) = c("lon", "lat")
      num.of.files = 1
      lis = F
    }

    if ( class(data) == "list"){
      num.of.files = length(data)
      if ( length(  dim(data[[1]])) ==3){
        foo = data[[1]]
        dat = as.data.frame(cbind( lon = as.vector(foo[,"lon",]),
                                   lat = as.vector(foo[,"lat",])))
      } else{
        dat = as.data.frame(data[[1]])
        names(dat) = c("lon", "lat")
      }
      lis = T
    }

    ### 1.a image

    if ( image.or.vid == "image"){

      if ( is.na(centre[1]) ){ # if no centre is specified.
        centre = c ( ((max( dat[ ,1],na.rm=T ) + min( dat [ ,1],na.rm=T )) /2 ), ((max( dat[ ,2],na.rm=T ) + min( dat [ ,2],na.rm=T )) /2 ) ) # find centre for map
      }


      map = ggmap::get_map( location = centre,
                            zoom = zoom,
                            maptype = maptype) # load map
      p = ggmap::ggmap(map)

      for ( i in 1:num.of.files){
        #i=2

        if ( lis == T){
          if ( length(  dim(data[[1]])) ==3){
            foo = data[[i]]
            dat = as.data.frame(cbind( lon = as.vector(foo[,"lon",]),
                                       lat = as.vector(foo[,"lat",])))
          } else {
            dat = as.data.frame(data[[i]])
            names(dat) = c("lon", "lat")
          }
        }
        if ( lines.or.points == "lines"){
          p = p + ggplot2::geom_path (data= dat, ggplot2::aes(x=lon, y=lat),
                                      color = col[i], size = lwd)
        } else {
          p = p + ggplot2::geom_point (data= dat, ggplot2::aes(x=lon, y=lat),
                                       color = col[i], size = lwd)
        }
      }

      scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
        xbreaks <- seq(xmin,xmax,step)
        xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
        return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
      }

      scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
        ybreaks <- seq(ymin,ymax,step)
        ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
        return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
      }

      p = p  + ggplot2::xlab("Longitude")+
        ggplot2::ylab("Latitude") +
        ggplot2::theme(axis.text= ggplot2::element_text(size= axis.nums.size),
                       axis.title= ggplot2::element_text(size= axis.labs.size ,face="bold"))
      return(p )
    }

    ##### 1.b video

    if ( image.or.vid == "video"){

      for ( j in seq(tail.size,nrow(dat),frame.size)){ # from the start (restricted by tail size), to the end of the data set, by frame size

        dat2 = dat[(j-tail.size+1):j,]
        centroid = data.frame ( lon = median(data.rem[j,1,],na.rm = T),
                                lat = median(data.rem[j,2,],na.rm = T))# A more advanced centriod algorithm is currently being updated for potential publication, email me and I can send it over though if you're interested.
        names(dat2) = c("lon" , "lat")

        if( file.to.folder){
          png(file = file.path( wd , paste0( plot_name,  j , ".png")))
        }

        map = ggmap::get_map( location = c(centroid$lon,
                                           centroid$lat),
                              zoom = zoom,
                              maptype = maptype)

        p = ggmap::ggmap(map, extent = "device")  +
          ggplot2::theme(axis.line = element_blank(),
                         axis.text  = element_blank(),
                         axis.ticks = element_blank(),
                         plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
          xlab('') +
          ylab('')
        p = p + ggplot2::geom_path (data= dat2, aes(x=lon, y=lat), color= col, size = lwd)
        print(p)

        if(file.to.folder){
          dev.off()
        }
      }
    }
  }

  #### 2. GROUP

  if ( type[1] == "group"){

    ### 2.a image

    if( image.or.vid == "image"){
      centroid = collEcol::d2c( data,return_centorid = T)
      centroid = as.data.frame(centroid)

      if ( is.na(centre[1]) ){ # if no centre is specified.
        centre = c ( (max(   centroid[ ,1],na.rm = T ) +
                        min(   centroid[ ,1],na.rm = T) ) /2 ,
                     (max(   centroid[ ,2],na.rm = T ) +
                        min(   centroid[ ,2],na.rm = T) ) /2 ) # find centre for map
      }

      # zoom = 12
      {
        map = ggmap::get_map( location = centre,
                              zoom = zoom,
                              maptype = maptype)
        p = ggmap::ggmap(map, extent = "device")  +
          ggplot2::theme(axis.line = element_blank(),
                         axis.text  = element_blank(),
                         axis.ticks = element_blank(),
                         plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
          xlab('') +
          ylab('')
        rain = rainbow(dims[3])
        for ( k in 1:dims[3]){
          p = p + ggplot2::geom_path (data= as.data.frame(data[,c("lon","lat"),k]),
                                      aes(x=lon, y=lat), color= rain[k], size = 2)
        }
      }
      return(print(suppressWarnings(p)))
    }

    ### 2.b video

    if ( image.or.vid == "video"){


      data = data[[1]]
      dims = dim(data)


      # loop
      for ( j in seq(tail.size+start.at,dims[1],frame.size)){ # from the start (restricted by tail size), to the end of the data set, by frame size
        # j = 20

        # subset
        data2 = data[(j-tail.size+1):j,,]
        falc2 = falc[(j-tail.size+1):j,]
        data3 = plyr::adply(data2,3)
        centroid = data.frame ( lon = mean(data.rem[j,1,],na.rm = T),
                                lat = mean(data.rem[j,2,],na.rm = T))# A more advanced centriod algorithm is currently being updated for potential publication, email me and I can send it over though if you're interested.


        # ggmap
        map = ggmap::get_map( location = c(centroid$lon,
                                           centroid$lat),
                              zoom = zoom,
                              maptype = maptype)
        p = ggmap::ggmap(map, extent = "device")  +
          ggplot2::theme(axis.line = element_blank(),
                         axis.text  = element_blank(),
                         axis.ticks = element_blank(),
                         plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
          xlab('') +
          ylab('')

        fc <- colorRampPalette(c("red", "darkred"))
        col.hue = fc(dims[3])
        for ( k in 1:dims[3]){
          # k = 1
          d = as.data.frame(data2[,,k])
          p = p + ggplot2::geom_path (data= d , aes(x=lon, y=lat),color= col.hue[k], size = 1)

        }
        p = p + ggplot2::geom_path (data= falc2, aes(x=lon, y=lat), color= col[2], size = 2)

        # save as
        if( file.to.folder){
          png(file = file.path( wd , paste0( plot_name,  j , ".png")))
          print(suppressWarnings(p))
          dev.off()
        } else {
          print(suppressWarnings(p))
        }

        #take stock
        print( paste ( j , "/" , dims[1] ))

      }
    }
  }
}






