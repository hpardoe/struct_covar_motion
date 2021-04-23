# The aim of this analysis is to calculate how the r threshold for constructing
# structural covariance network graphs is affected by the number of motion affected
# scans in the sample


source("threshold2Density.R")
mypal <- brewer.pal("Set2", n = 3)
aal_gm.df <- read.csv("gm_table_aal_run01_and_02_20210305.csv", stringsAsFactors=FALSE)

for (i in 1:dim(aal_gm.df)[1]) {
  myfile <- strsplit(aal_gm.df$fileNames[i], split = "/")[[1]][3]
  aal_gm.df$run[i] <- strsplit(myfile, split = "_")[[1]][4]
  aal_gm.df$subject[i] <- strsplit(myfile, split = "_")[[1]][1]
}

aal_gm.df$run <- as.factor(aal_gm.df$run)
aal_gm.df$subject <- as.factor(aal_gm.df$subject)

resample_df <- function(input_df = aal_gm.df, nrows_out = 20, n_motion = 0) {
  nomotion_indices <- which(input_df$run == "run-01")
  motion_indices <- which(input_df$run == "run-02")
  motion_sample <- sample(motion_indices,n_motion, replace=TRUE)
  nomotion_sample <- sample(nomotion_indices, (nrows_out - n_motion) , replace=TRUE)
  input_df[c(motion_sample,nomotion_sample),]
}

calc_cor <- function(input_df = temp) {
  mymatrix <- as.matrix(input_df[,1:90])
  mycor_matrix <- cor(mymatrix)
  mycor_vector <- as.vector(mycor_matrix[ upper.tri(mycor_matrix)])
  list(cor_matrix = mycor_matrix, cor_vector = mycor_vector)
}

n_reps <- 5000
n_motion_subs <- seq(from = 1, to = 29, by = 3)
percent_motion_subjects <- 100*(n_motion_subs/29)
#thresholds <- c(0.89,0.9,0.91)
densities <- c(0.1,0.15,0.2)
myDensity <- 0.1
#sensitivity_specificity_array <- array(dim = c(n_reps,length(n_motion_subs),2,length(densities)))
threshold_array <- array(dim = c(n_reps,length(n_motion_subs),length(densities)))
pb <- txtProgressBar(min = 0, max = n_reps, style = 3)
for (i in 1:n_reps) {
  for (j in 1:length(n_motion_subs)) {
    # tempCor <- calc_cor(resample_df(nrows_out=29, n_motion=n_motion_subs[j]))
    # threshold <- density2Threshold(tempCor$cor_matrix, density = myDensity)
    # threshold_matrix[i,j] <- threshold
      for (k in 1:length(densities)) {
        tempCor <- calc_cor(resample_df(nrows_out=29, n_motion=n_motion_subs[j]))
        #threshold <- thresholds[k]
        threshold <- density2Threshold(tempCor$cor_matrix, density = densities[k])
        threshold_array[i,j,k] <- threshold
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

#plot(x = n_motion_subs, y = colMeans(threshold_array[,,1]), type = "l", col = mypal[1])
