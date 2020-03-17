diagnostics.plot <- function(model, breaks = 100){
  old.par = par(no.readonly = TRUE)
  par(mfrow=c(2, 2))
  par(mar=c(3, 3, 0.5, 0.5))
  hist(residuals(model), probability=T, xlab="", ylab="", main="", breaks = breaks)
  x=seq(min(residuals(model)), max(residuals(model)), length.out=100)
  lines(x, dnorm(x, mean=0, sd=sd(residuals(model))))
  qqnorm(residuals(model), main="")
  qqline(residuals(model))
  plot(fitted(model), residuals(model), pch=19)
  abline(h=0, lty=2)
  par(old.par)
}
