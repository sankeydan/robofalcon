
load(file.path ( PROJHOME , "Output", "FinalDataSet", "ALLrobofalcon-data.rda"))
load(file.path ( PROJHOME , "Output", "FinalDataSet", "REMrobofalcon-data.rda"))
vars = c( "sqrt_diff_head",
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
vars.nofalc = c( "sqrt_diff_head",
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
# samples
sam9 = seq( 1,nrow(dat), 9 )
sam24 = seq( 1,nrow(dat), 24 )
sam1 = seq( 1,nrow(dat), 1 )
sam6 = seq( 1,nrow(dat), 6 )
sam27 = seq( 1,nrow(dat), 27 )
sam26 = seq( 1,nrow(dat), 26 )
sam16 = seq( 1,nrow(dat),16)

