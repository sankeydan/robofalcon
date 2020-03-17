printpercent = function ( i, tothisnum){
if ( i %in% round( seq( 1, tothisnum, (tothisnum/100) ))){
  print ( paste ( round(i/ (tothisnum/100)) , "%"))
}
}
