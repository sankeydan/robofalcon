save.mixed.model = function( model , filename, mumin= F){

  if ( mumin){
    options(na.action = "na.fail")
    m = MuMIn::dredge(model)
    options(na.action = "na.omit")
    save(m , file= file.path(PROJHOME , "Output",
                             "Statistics", "mumin", filename))
  }else {
    sum1 = summary(model)
    t = sum1$tTable
    write.csv( t , file = file.path (PROJHOME , "Output", "Statistics" , "Tables", filename))
    save ( model , file = file.path (PROJHOME , "Output", "Statistics" , "Models", filename))
  }
}
