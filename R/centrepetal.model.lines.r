centrepetal.model.lines = function(dat, speed.cut.off, length_out, max.speed = 40, min.turn = 0.001, plot = F){

  # max.speed = 40
  # min.turn = 0.001

  dat = as.data.frame(dat)
  spe = dat$speed
  tur = dat$turn
  spe[spe<speed.cut.off] = NA
  spe[spe>max.speed] = NA
  tur[tur<min.turn] = NA
  #plot(dat$turn ~ dat$speed)

  m<-nls(tur~a*exp(-b*spe),start=list(a=a_start,b=b_start),control = nlc)
  if(plot){
  plot(tur~spe, xlim = c(speed.cut.off,max.speed))
  }
  params = coef(summary(m))[,1]
  a =  params[1]
  b =  params[2]
  x = seq( speed.cut.off,30, length.out = length_out)
  y = a*exp(-b*x)
  r = x/y
  ca = (x^2)/r
  dat = as.data.frame(cbind(x,y,r,ca))
  return(list(dat, a,b,tur,spe))
}

