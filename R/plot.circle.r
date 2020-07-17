#rm(list=ls())

plot.circle = function( x0 = 10,
                        y0 = 10,
                        r,
                        len = 100001,
                        mean,
                        cim,
                        cip,
                        plot.new = F,
                        plot.segments = F,
                        lwds = 1,
                        cexs = 1,
                        plot.dashed = T,
                        pdlwd = 1,
                        colchoose = "red"){

  # variables

  # i=2
  # x0 = 10
  # y0 = 10
  # r  = rs[i]
  # mean = sqm[vars[i],"mean"]
  # cim  = sqm[vars[i],"ci1"]
  # cip  = sqm[vars[i],"ci2"]
  # plot.new  = F
  # plot.segments = T
  # lwds = 1
  # cexs = 1
  # len = 100001
  # plot.dashed = T
  # pdlwd = 1



  # plot blank?
  if(plot.new){
    xblank=x0 + cos(2*pi*c(1:len)/len)
    yblank=y0 + sin(2*pi*c(1:len)/len)
    x2 = rev(xblank[1:ceiling(len/2)])
    y2 = rev(yblank[1:ceiling(len/2)])
    sq = seq( -1, 1, length.out =  (len/2) )
    sq4 = sq[((len/4)+1):(len/2)]
    sq2 = c(-rev((1-rev(sqrt(abs(sq4))) )),0,(1-rev(sqrt(abs(sq4)))))
    plot(yblank~xblank,type = "n")
    lines( x2,y2)
    # x = rev(x)
    # y = rev(y)
    sq3 = seq(-1, 1, length.out =  21)
    for ( j in 1:length(sq3)){
      #j=13
      w.mi = which.min( abs( sq3[j] - sq2))
      points( x2[w.mi],
              y2[w.mi],col="red")
    }
  }

  # xy
  x=x0 + r*cos(2*pi*c(1:len)/len)
  y=y0 + r*sin(2*pi*c(1:len)/len)
  sq = seq( -1, 1, length.out =  (len/2) )
  sq2 = c(-rev((1-rev(sqrt(abs(sq))) )),0,(1-rev(sqrt(abs(sq)))))

  # set up circle
  x2 = rev(x[1:ceiling(len/2)])
  y2 = rev(y[1:ceiling(len/2)])
  sq = seq( -1, 1, length.out =  (len/2) )
  sq4 = sq[((len/4)+1):(len/2)]
  sq2 = c(-rev((1-rev(sqrt(abs(sq4))) )),0,(1-rev(sqrt(abs(sq4)))))
  
  # plot dashed
  if ( plot.dashed){
    lines( x2,y2,lwd = pdlwd, lty = 2)
  }
  
  # plot point and line
  sq3 = which.min( abs(cim- sq2)):
    which.min( abs(cip- sq2))
  mid = which.min( abs(mean-sq2))
  lines( x2[sq3],y2[sq3],lwd =lwds,col = colchoose)
  points(x2[mid],y2[mid],cex =cexs)

  # if plot segments
  if ( plot.segments){
    xblank=x0 + cos(2*pi*c(1:len)/len)
    yblank=y0 + sin(2*pi*c(1:len)/len)
    x2 = rev(xblank[1:ceiling(len/2)])
    y2 = rev(yblank[1:ceiling(len/2)])
    sq = seq( -1, 1, length.out =  (len/2) )
    sq4 = sq[((len/4)+1):(len/2)]
    sq2 = c(-rev((1-rev(sqrt(abs(sq4))) )),0,(1-rev(sqrt(abs(sq4)))))
    w.mi = which.min( abs( cim - sq2))
    segments( 10, 10, x2[w.mi],
              y2[w.mi],col = colchoose)
    w.mi = which.min( abs( cip - sq2))
    segments( 10, 10, x2[w.mi],
              y2[w.mi],col = colchoose)
  }
}



