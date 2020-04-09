save.mixed.model = function( model , data, filename){
  # model= lmm9
  # data = d.con
  
  sum1 = summary(model)
  t =  sum1$tTable
  cohens.d =  c(NA, EMAtools::lme.dscore(model,data,"nlme")$d)
  CIs = as.data.frame ( intervals(model, which = "fixed")$fixed[,c(1,3)])
  names( CIs) = c("CI-2.5%", "CI-97.5%")
  t = cbind( t, CIs, cohens.d)
  t = t[,c("Value","CI-2.5%", "CI-97.5%", "t-value", "cohens.d", "p-value")]
  
  write.csv( t , file = file.path (PROJHOME , "Output", "Statistics" , "Tables", filename))
  save ( model , file = file.path (PROJHOME , "Output", "Statistics" , "Models", filename))
  
}
