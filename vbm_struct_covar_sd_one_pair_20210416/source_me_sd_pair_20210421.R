gm.df <- read.csv("gm_table_aal_run01_and_02_20210305.csv")
demo.df <- read.csv("motion_detection_demographics_20210309.csv")
bvol.df <- read.csv("struct_covar_gmvols_20210309.csv")

# add brain vols to demo.df
for (i in 1:dim(bvol.df)[1]) {
  bvol.df$subject[i] <- strsplit(strsplit(bvol.df$fname[i], split = "_")[[1]][1], split = "-")[[1]][2]
}

demo.df <- merge(demo.df, bvol.df, by = "subject")
demo.df$fname <- NULL

for (i in 1:dim(gm.df)[1]) {
  subject <- strsplit(strsplit(strsplit(gm.df$fileNames[i], split = "/")[[1]][3], split = "_")[[1]][1], split = "-")[[1]][2]
  subjectIndex <- grep(pattern = subject, demo.df$subject)
  gm.df$subject[i] <- demo.df$subject[subjectIndex]
  gm.df$age[i] <- demo.df$age[subjectIndex]
  gm.df$sex[i] <- demo.df$sex[subjectIndex]
  gm.df$totalvol_gm[i] <- demo.df$totalvol_gm[subjectIndex]
  gm.df$run[i] <- strsplit(strsplit(gm.df$fileNames[i], split = "/")[[1]][3], split = "_")[[1]][4]
}

noMotion.df <- gm.df[ gm.df$run == "run-01", ]
motion.df <- gm.df[ gm.df$run == "run-02", ]

# set.seed(2021)
# #i <- 2
make_motion_affected_df <- function(numMotionAffected = 0) {
  i <- numMotionAffected
  myUniqueSubjects <- unique(gm.df$subject)
  if (numMotionAffected == 0) {
    my.df <- noMotion.df
  } else {
    myMotionSubjects <- sample(myUniqueSubjects, i)
    myMotionIndex <- match(myMotionSubjects, myUniqueSubjects)
    myNoMotionSubjects <- myUniqueSubjects[ -myMotionIndex ]
    # get motion scans
    motionIndex <- match(myMotionSubjects,motion.df$subject)
    # get nomotion scans
    noMotionIndex <- match(myNoMotionSubjects, noMotion.df$subject)
    my.df <- rbind(motion.df[motionIndex,],noMotion.df[noMotionIndex,])
  }
  my.df
}

temp_n3.df <- make_motion_affected_df(3)
nMotion <- 5
nAlready <- length(which(temp_n3.df$run == "run-02"))
nNew <- nMotion - nAlready
newSample <- sample(x = temp_n3.df$subject[ temp_n3.df$run == "run-01" ], size = nNew)
motionSubs <- c(temp_n3.df$subject[ temp_n3.df$run == "run-02" ], newSample)
allSubs <- temp_n3.df$subject
noMotionSubs <- allSubs[ ! allSubs %in% motionSubs ]
motionIndices <- (gm.df$subject %in% motionSubs) & (gm.df$run == "run-02")
noMotionIndices <- (gm.df$subject %in% noMotionSubs) & (gm.df$run == "run-01")
out_temp.df <- gm.df[ motionIndices | noMotionIndices , ]

makeMotionDf <- function(input_df, numMotionAffected = 0) {
  nAlready <- length(which(input_df$run == "run-02"))
  nNew <- numMotionAffected - nAlready
  newSample <- sample(x = input_df$subject[ input_df$run == "run-01" ], size = nNew)
  motionSubs <- c(input_df$subject[ input_df$run == "run-02" ], newSample)
  allSubs <- input_df$subject
  noMotionSubs <- allSubs[ ! allSubs %in% motionSubs ]
  motionIndices <- (gm.df$subject %in% motionSubs) & (gm.df$run == "run-02")
  noMotionIndices <- (gm.df$subject %in% noMotionSubs) & (gm.df$run == "run-01")
  out_temp.df <- gm.df[ motionIndices | noMotionIndices , ]
  out_temp.df
}
#
# make_motion_affected_prior_df <- function(input_df, numMotionAffected = 0) {
#   alreadyMotionAffected <- which(input_df$run == "run-02")
#   numAlreadyAffected <- length(which(input_df$run == "run-02"))
#   myUniqueSubjects <- unique(gm.df$subject)
#   if (numMotionAffected == numAlreadyAffected) {
#     my.df <- input_df
#   } else {
#     if (numMotionAffected < numAlreadyAffected) {
#       # remove motion affected scans from input_df
#     } else {
#       # add motion affected scans to input_df
#
#     }
#   }
#   my.df
# }
#
# temp.df <- make_motion_affected_df()
