as.numeric.data = function(dat){

dat.num = dat
for ( i in 1:ncol(dat)){
  dat.num[,i] = suppressWarnings(as.numeric(as.character(dat[,i])))
  if ( length(which(!is.na(dat.num[,i]))) == 0){
    dat.num[,i] = as.character(dat[,i])
  }
}
return(dat.num)

}
