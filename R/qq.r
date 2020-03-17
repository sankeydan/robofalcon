qq = function(model){
  qqnorm(residuals(model))
  qqline(residuals(model))
}
