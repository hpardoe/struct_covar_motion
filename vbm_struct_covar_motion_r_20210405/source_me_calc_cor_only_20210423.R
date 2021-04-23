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
# out <- c()
# outVals <- c()
# for (i in 1:length(myNames)) {
#   for (j in (i + 1):length(myNames)) {
#     if ((i + 1) <= length(myNames)) {
#     out <- c(out,paste(myNames[i],myNames[j], sep="-"))
#     myoutValue <- summary(lm(gm.df[[ names(gm.df)[i] ]] ~ run*gm.df[[ names(gm.df)[j] ]], data = gm.df))$coefficients[4,4]
#     outVals <- c(outVals, myoutValue)
#   } else break
#   }
# }
myNames <- names(gm.df)[1:90]

calcR <- function(input.df = noMotion.df) {
  connectionName <- c()
  connectionR <- c()
  #connection_lmR_nomotioncovar <- c()
  #connection_lmR_with_motioncovar <- c()
  #connectionXSD <- c()
  #connectionYSD <- c()

  # for (i in 1:length(myNames)) {
  #   for (j in (i + 1):length(myNames)) {
  #     if ((i + 1) <= length(myNames)) {
  #       connectionName <- c(connectionName,paste(myNames[i],myNames[j], sep="-"))
  #       cor_only <- cor(input.df[[ myNames[i] ]], input.df[[ myNames[j] ]])
  #       #cor_lm_nomotioncovar <- lm(input.df[[ myNames[i] ]] ~ input.df[[ myNames[j] ]] + input.df$age + input.df$sex)$coefficients[2]
  #       #cor_lm_motioncovar <- lm(input.df[[ myNames[i] ]] ~ input.df[[ myNames[j] ]] + input.df$age + input.df$sex + input.df$run)$coefficients[2]
  #       connectionR <- c(connectionR, cor_only)
  #       #connection_lmR_nomotioncovar <- c(connection_lmR_nomotioncovar, cor_lm_nomotioncovar)
  #       #connection_lmR_with_motioncovar <- c(connection_lmR_with_motioncovar, cor_lm_motioncovar)
  #
  #       #connectionXSD <- c(connectionXSD, sd(noMotion.df[[ myNames[i] ]]))
  #       #connectionYSD <- c(connectionYSD, sd(noMotion.df[[ myNames[j] ]]))
  #     } else break
  #   }
  # }
  # data.frame(connectionName, connectionR)

}

#nomotionR.df <- calcR(noMotion.df)
#motionR.df <- calcR(motion.df)
nRuns <- 5000
nMotionAffected <- seq(from = 1, to = 29, by = 2)
#corMatrix <- matrix(nrow = nRuns, ncol = length(nMotionAffected))
corArray <- array(dim = c(90,90,nRuns, length(nMotionAffected)))
noMotionIndices <- which(gm.df$run == "run-01")
motionIndices <- which(gm.df$run == "run-02")
pb <- txtProgressBar(min = 0, max = length(nMotionAffected), style = 3)
for (j in 1:length(nMotionAffected)) {
  setTxtProgressBar(pb, j)
  for (i in 1:nRuns) {
    motionSample <- sample(motionIndices, nMotionAffected[j], replace = TRUE)
    nNoMotion <- length(noMotionIndices) - nMotionAffected[j]
    noMotionSample <- sample(noMotionIndices, nNoMotion, replace = TRUE)
    mySample <- c(motionSample,noMotionSample)
    #mySample <- sample(noMotionIndices, length(noMotionIndices), replace = TRUE)
    my.df <- gm.df[mySample,1:90]
    corArray[,,i,j] <- cor(my.df)
  }
}
close(pb)

myMeanSummary <- vector(length = 5000)
myMeanMatrix <- matrix(nrow = dim(corArray)[3], ncol = dim(corArray)[4])

for (k in 1:dim(corArray)[4]) {
  for (j in 1:dim(corArray)[3]) {
    myVec <- vector(length = dim(ind)[1])

    for (i in 1:length(myVec)) {
    myVec[i] <- corArray[ind[i,1],ind[i,2],j,k]
    }

    #myMeanSummary[j] <- mean(myVec)
    myMeanMatrix[j,k] <- mean(myVec)
  }
}


# pb <- txtProgressBar(min = 0, max = length(nMotionAffected), style = 3)
# for (i in 1:length(nMotionAffected)) {
#   setTxtProgressBar(pb, i)
#   for (j in 1:nRuns) {
#     motionSample <- sample(motionIndices, nMotionAffected[i], replace = TRUE)
#     nNoMotion <- length(noMotionIndices) - nMotionAffected[i]
#     noMotionSample <- sample(noMotionIndices, nNoMotion, replace = TRUE)
#     mySample <- c(motionSample,noMotionSample)
#     my.df <- gm.df[mySample,]
#
#     myCor.df <- calcR(my.df)
#     corMatrix[j,i] <- mean(myCor.df$connectionR)
#     #corArray[j,i,1] <- mean(myCor.df$connectionR)
#     #corArray[j,i,2] <- mean(myCor.df$connection_lmR_nomotioncovar)
#     #corArray[j,i,3] <- mean(myCor.df$connection_lmR_with_motioncovar)
#   }
# }
# close(pb)
#
# corList = list(corArray = corArray, nMotionAffected = nMotionAffected)

#save(corList, file = "myCorList_20210405.RData")

# cross_pvals.df <- data.frame(nodes = out, pval = outVals)
# cross_pvals_reordered.df <- cross_pvals.df[ order(cross_pvals.df$pval), ]
# cross_pvals_reordered.df$rank <- 1:dim(cross_pvals_reordered.df)[1]
