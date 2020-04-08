 #descriptives

### how many trajectories in each condition
# fold = file.path(PROJHOME , "Output" , "rawdata2")
# files= list.files(fold)
# spl = stringr::str_split_fixed(files, "\\.",12)
# table( spl[,3],spl[,6])

### how many rows removed for turning angle
source( file.path(PROJHOME , "source", "source.r"))
1-length(which(!is.na(dat$diff_head_sharp))) /
length(which(!is.na(dat$diff_head)))

nas = which(is.na(dat$diff_head_sharp))
no.nas =which(!is.na(dat$diff_head_sharp))
median ( dat$dist2pred [nas], na.rm = T)
median ( dat$dist2pred [no.nas], na.rm = T)
median ( dat$dist2site[nas] , na.rm = T)
median ( dat$dist2site[no.nas],na.rm=T)


# how many rows removed for speed
dat$speed[dat$speed>100] = NA
hist(dat$speed)
quantile ( na.omit(dat$speed), .99)

# what was mass and relation to logger mass
load( file.path(PROJHOME,  "metadata" , "mass" , "mass-data.rda"))
vec= tapply( massdata$Mass, massdata$Pigeon , function  (x) {mean(x,na.rm = T )})
vec2 = 20.5/vec > 0.05
length(which ( vec2==T))

# median distance to centroid - small large
median ( dat.coh$dist2cent [ dat$small.big == "s"], na.rm = T)
median ( dat.coh$dist2cent [ dat$small.big == "b"], na.rm = T)
