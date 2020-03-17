register_ggmap = function(install = T){
  PROJHOME = PROJHOME
  load( file.path("~//APIcode.rda"))
  if ( install){
  devtools::install_github("dkahle/ggmap")
  }
  library(ggmap)
  print(register_google( key = key.code))
  return(key.code)
}
