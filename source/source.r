
library(robofalcon)
vars = c( "log_diff_head",
          "diff_head",
          "diff_head_sharp",
          "condition",
          "cross.wind",
          "dist2cent",
          "dist2home",
          "turn2falchead",
          "dir.rel.ali",
          "unique.flight",
          "turn2falcpos",
          "align",
          "attract",
          "small.big",
          "turn2home",
          "pigeon",
          "Date",
          "group.num")
vars.nofalc = c( "log_diff_head",
                 "diff_head",
                 "diff_head_sharp",
                 "condition",
                 "cross.wind",
                 "dist2cent",
                 "dist2home",
                 "dir.rel.ali",
                 "unique.flight",
                 "align",
                 "attract",
                 "centrip",
                 "support.wind",
                 "speed",
                 "small.big",
                 "dist2site",
                 "turn2home",
                 "pigeon",
                 "Date",
                 "group.num")
# libraries
library(stringr)
library(lme4)
library(nlme)

# files/folders
fold =file.path(PROJHOME , "Output" , "selfish" )
files = list.files(fold)
spl = str_split_fixed(files,"-",8)
whi = spl[,5] == FALSE

files.all = files[whi]
files.cohesive = files[!whi]

# data

{
  load( file.path(fold, files.cohesive[length(files.cohesive)]) )
  dir.rel.ali = dat$align - dat$diff_head
  dat$sig.cent = sign( dat$attract)
  dat$sig.head = sign( dat$align)
  dat$conflict = ifelse( dat$sig.head != dat$sig.cent, 1,-1)
  dat$dir.rel.ali = dir.rel.ali * dat$conflict
  dat$sign.diff = sign(dat$diff_head)
  dat$log_diff_head = dat$sign.diff * sqrt(abs(dat$diff_head))
  dat$pigeon = as.character(dat$pigeon)
  dat$group.num = as.character(dat$group.num)
  dat$unique.flight = as.character(dat$unique.flight)
  dat.coh = dat
}

{
  load( file.path(fold, files.all[length(files.all)]) )

  dir.rel.ali = dat$align - dat$diff_head
  dat$sig.cent = sign( dat$attract)
  dat$sig.head = sign( dat$align)
  dat$conflict = ifelse( dat$sig.head != dat$sig.cent, 1,-1)
  dat$dir.rel.ali = dir.rel.ali * dat$conflict
  dat$sign.diff = sign(dat$diff_head)
  dat$log_diff_head = dat$sign.diff * sqrt(abs(dat$diff_head))
  dat$pigeon = as.character(dat$pigeon)
  dat$group.num = as.character(dat$group.num)
  dat$unique.flight = as.character(dat$unique.flight)
  dat$speed = get_dist(dat$lat,dat$lon, method = "speed" , hz = 5)
  dat$turn.radius = dat$speed / abs(dat$diff_head)
  dat$centrip = dat$speed^2 / dat$turn.radius
  dat.coh$centrip = dat$centrip
}

# CONFLICT
dat$sign.diff = sign(dat$diff_head)
dat$diff_head[ abs(dat$diff_head) > 0.3] = NA
dat$dir.rel.ali[ abs(dat$dir.rel.ali)  > 0.3] = NA
dat$diff_head_sharp = dat$diff_head
dat$dir.rel.ali_sharp = dat$dir.rel.ali
dat$diff_head_sharp[ abs(dat$diff_head_sharp) < 0.02] = NA
dat$dir.rel.ali_sharp[ abs(dat$dir.rel.ali_sharp) < 0.02] = NA
dat.coh$diff_head_sharp = dat$diff_head_sharp
dat$log_diff_head = dat$sign.diff *
  #sqrt(
    sqrt(abs(dat$diff_head))
    #)

# clockwise vs anticlockwise
dat$sig.cent = sign( dat$attract)
dat$sig.head = sign( -dat$turn2falchead)
dat$sig.turn = sign( dat$diff_head)

# conflict?
dat$conflict = ( dat$sig.head != dat$sig.cent)
con = which(dat$conflict == T)
dat.con = dat[con,]
dat.nocon = dat[-con,]
  dat.con$biased.cent=   dat.con$sig.turn ==   dat.con$sig.cent
dat.nocon$biased.cent= dat.nocon$sig.turn == dat.nocon$sig.cent
# samples
sam9 = seq( 1,nrow(dat), 9 )
sam24 = seq( 1,nrow(dat), 24 )
sam1 = seq( 1,nrow(dat), 1 )
sam6 = seq( 1,nrow(dat), 6 )
sam27 = seq( 1,nrow(dat), 27 )
sam26 = seq( 1,nrow(dat), 26 )

# flocksize
load( file.path( PROJHOME, "Output" , "Statistics" , "Data" , "flock.sizes.rda" ))
dat.coh$flock.size = flock.sizes
dat$flock.size = flock.sizes

# time
load( file.path( PROJHOME, "Output", "Statistics" , "Data" , "ts.rda"))
dat.coh$t = ts
dat$t = ts

