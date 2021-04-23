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
  gm.df$age[i] <- demo.df$age[subjectIndex]
  gm.df$sex[i] <- demo.df$sex[subjectIndex]
  gm.df$totalvol_gm[i] <- demo.df$totalvol_gm[subjectIndex]
  gm.df$run[i] <- strsplit(strsplit(gm.df$fileNames[i], split = "/")[[1]][3], split = "_")[[1]][4]
}

noMotion.df <- gm.df[ gm.df$run == "run-01", ]
motion.df <- gm.df[ gm.df$run == "run-02", ]



# myNames <- names(gm.df)[1:116]
#
# calcR <- function(input.df = noMotion.df) {
#   connectionName <- c()
#   connectionR <- c()
#   #connectionXSD <- c()
#   #connectionYSD <- c()
#
#   for (i in 1:length(myNames)) {
#     for (j in (i + 1):length(myNames)) {
#       if ((i + 1) <= length(myNames)) {
#         connectionName <- c(connectionName,paste(myNames[i],myNames[j], sep="-"))
#         connectionR <- c(connectionR, cor(input.df[[ myNames[i] ]], input.df[[ myNames[j] ]]))
#         #connectionXSD <- c(connectionXSD, sd(noMotion.df[[ myNames[i] ]]))
#         #connectionYSD <- c(connectionYSD, sd(noMotion.df[[ myNames[j] ]]))
#       } else break
#     }
#   }
#   data.frame(connectionName, connectionR)
#
# }
#
# #nomotionR.df <- calcR(noMotion.df)
# #motionR.df <- calcR(motion.df)
nRuns <- 5000
nMotionAffected <- seq(from = 0, to = 29, by = 2)
#sdMatrix <- matrix(nrow = nRuns, ncol = length(nMotionAffected))
sdArray <- array(dim = c(nRuns,length(nMotionAffected),90))
noMotionIndices <- which(gm.df$run == "run-01")
motionIndices <- which(gm.df$run == "run-02")
pb <- txtProgressBar(min = 0, max = length(nMotionAffected), style = 3)
for (i in 1:length(nMotionAffected)) {
  for (j in 1:nRuns) {
    motionSample <- sample(motionIndices, nMotionAffected[i], replace = TRUE)
    nNoMotion <- length(noMotionIndices) - nMotionAffected[i]
    noMotionSample <- sample(noMotionIndices, nNoMotion, replace = TRUE)
    mySample <- c(motionSample,noMotionSample)
    my.df <- gm.df[mySample,]
    #myCor.df <- calcR(my.df)
    #sdMatrix[j,i] <- sd(my.df$Precentral_L)
    for (k in 1:90) {
      sdArray[j,i,k] <- sd(my.df[,k])
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)
#
# #save(corMatrix, file = "myCorMatrix_20210330.RData")
#
# # cross_pvals.df <- data.frame(nodes = out, pval = outVals)
# # cross_pvals_reordered.df <- cross_pvals.df[ order(cross_pvals.df$pval), ]
# # cross_pvals_reordered.df$rank <- 1:dim(cross_pvals_reordered.df)[1]
