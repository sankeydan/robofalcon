folderDS = function  ( vec){
  
  for ( i in 1:length(vec)){
    
    vec.path = file.path(PROJHOME , paste(vec[1:i], collapse = "/"))
    
    if ( ! dir.exists ( vec.path)){
      dir.create ( vec.path)
    }
  }
}