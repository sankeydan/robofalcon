rm(list=ls())
load( file.path(PROJHOME, "gitData","abmData.rda"))
source( file.path(PROJHOME , "source", "source.r"))

control = data.frame ( y= as.numeric ( dat.coh$dir.rel.ali[dat.coh$condition=="c"]), x="Control")
predator = data.frame ( y = as.numeric ( dat.coh$dir.rel.ali[dat.coh$condition=="p"]),x= "Predator")
abm = data.frame(y = d2$selfishs, x="ABM")

dat = rbind ( control, predator, abm)
ggplot(dat,aes(x,y))+
  geom_violin()+
  ylim(c(-1,1))+
  ylab("Instantaneous centroid attraction")+
  xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(  size=24))+
  theme(axis.text.y = element_text(  size=24))+
  theme(axis.title.y = element_text( size=24))+

  geom_hline(yintercept = 0, lty= 2)
  

mod =  lm(dat[,1]~dat[,2])
an = aov(mod)
TukeyHSD(an)
summary(mod)
