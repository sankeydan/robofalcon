# polar results plot
rm(list = ls())

# libraries
library(robofalcon)

# objects
plot.pdf = F


# Data
dat = read.csv( file.path ( PROJHOME, "Output", "Statistics" , "combinedstatstables.csv"))

# variables
vars = c("align", "align", "futatt", "turn2home", "turn2falchead")
model = c(2,1,1,1,3)
modelnum = as.numeric(  stringr::str_split_fixed ( stringr::str_split_fixed ( dat$name, "\\." , 4)[,1],"_",2)[,1])
vec = vector()
for (i in 1:length(vars)){
  vec = c(vec , which ( dat$X == vars[i] & modelnum == model[i]))
}
dat[vec,]
dat$Value = dat$Value *5 # convert into radians per second. 5 GPS hits per second
dat$CI.2.5. = dat$CI.2.5. *5
dat$CI.97.5. = dat$CI.97.5. *5

# confidence intervals
names(dat)[3:5] = c( "mean" , "ci1" , "ci2")
sqm = dat[vec,3:5]
sqm = sqm[rev(1:nrow(sqm)),]
########### PLOTSSSS
rs = rev(seq( 0.2, 1, length.out =  (length(vec)+1)))
rs = rs[2:length(rs)]

# PLOT PDF
if( plot.pdf){
  pdf ( file = file.path (
    PROJHOME , "Figures" , "Results" ,
    "polar-vars-results.pdf"), useDingbats =F)
}

  {
  #margins
  par(mar = c(4,2,2,2.5) )

  # plot NO PDF
  i.c = 4
  {
    for ( i in 1:length(vec)){
      # i = 2
      if ( i == i.c){
        p.s = T
      } else {
        p.s = F
      }
      if ( i == 1){
        p.n = T
      } else {
        p.n = F
      }
      plot.circle(r  = rs[i],
                  mean = sqm[i,"mean"],
                  cim  = sqm[i,"ci1" ],
                  cip  = sqm[i,"ci2" ],
                  plot.new = p.n,
                  lwds = 3.8,
                  len = 10001,
                  plot.segments = p.s,
                  cexs = 1.1)
    }
    points(10,10)
  }
  if (plot.pdf){
  dev.off()
  }
}

mat = t(matrix ( c(
"small.bigs:align" ,3,
"futatt:small.bigs" ,3,
"conditionp:align" ,2,
"conditionp:futatt" ,2,
"conditionp:align" ,1,
"conditionp:futatt" ,1),2))
vars = mat[,1]
model = as.numeric( mat[,2])
vec = vector()
for (i in 1:length(vars)){
  vec = c(vec , which ( dat$X == vars[i] & modelnum == model[i]))
}
sqm = dat[rev(vec),]
par(mfrow =c(2,1),mar = c(4,4,4,20))
plot ( c(1:nrow(sqm)), sqm$mean ,ylim = range( sqm[,3:5]),ylab = "Effect size")
for ( i in 1:nrow(sqm)){
  segments( i, sqm$ci1[i],i,sqm$ci2[i])
}
abline ( h = 0, lty= 2)

