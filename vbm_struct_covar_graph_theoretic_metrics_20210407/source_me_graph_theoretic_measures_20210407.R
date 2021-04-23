# The aim of this analysis is to estimate how commonly used
# graph theoretic summary measures are affected by in-scanner head motion


source("threshold2Density.R")
library(brainGraph)
library(igraph)
library(qgraph)

#mypal <- brewer.pal("Set2", n = 3)
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

#temp.df <- resample_df(nrows_out=29, n_motion=0)
#temp.df <- aal_gm.df[ aal_gm.df$run == "run-01", ]
nRuns <- 5000
nMotion_affected <- seq(from = 0, to = 29, by = 2)
nMotion_affected_percentage <- 100*(nMotion_affected/29)
densities <- c(0.1,0.15,0.2)
nGraphMeasures <- 4
graph_theoretic_array <- array(dim = c(nRuns,length(nMotion_affected),length(densities), nGraphMeasures))
pb <- txtProgressBar(min = 0, max = nRuns, style = 2)
for (i in 1:nRuns) {

  for (j in 1:length(nMotion_affected)) {
    for (k in 1:length(densities)) {
      temp.df <- resample_df(nrows_out=29, n_motion=nMotion_affected[j])
      tempCor <- calc_cor(temp.df)
      threshold <- density2Threshold(tempCor$cor_matrix, density = densities[k])
      temp_adj_matrix <- tempCor$cor_matrix > threshold
      tempAdjGraph <- graph_from_adjacency_matrix(temp_adj_matrix, mode = "undirected", weighted=NULL)

      # graph theoretic measures
      # transitivity/global clustering coefficient
      graph_theoretic_array[i,j,k,1] <- transitivity(tempAdjGraph)
      # modularity
      myCommunity <- cluster_fast_greedy(tempAdjGraph)
      graph_theoretic_array[i,j,k,2] <- modularity(myCommunity)
      # efficiency
      graph_theoretic_array[i,j,k,3] <- efficiency(tempAdjGraph, type = "global")
      # small world index
      graph_theoretic_array[i,j,k,4] <- smallworldIndex(tempAdjGraph)$index
    }
  }
  setTxtProgressBar(pb, i)
}

close(pb)
