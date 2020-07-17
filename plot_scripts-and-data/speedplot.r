
{ # skip to loop
  
  # housekeeping
  rm(list = ls())
  
  # libraries
  library(robofalcon)
  library(stringr)
  library(lme4)
  library(nlme)
  
  # files / folders 
  fold = file.path(PROJHOME, "Output", "dataremovedoutliers")
  files = list.files(fold)
  spl = str_split_fixed(files, "\\.",12)
  
}

for ( i in 1:length(files)){
# data
load( file.path( fold, files[i]))
dat = data.list[[1]]
mat = apply( dat, 3, function( x){
  #x=dat[,,1]
  get_dist(x[,1],x[,2],hz=5,method = "speed")
})

plot( mat[,1], type = "l",ylim=c(0,40))
for ( i in 2:ncol(mat)){
  lines ( mat[,i],col = i)
}
}
