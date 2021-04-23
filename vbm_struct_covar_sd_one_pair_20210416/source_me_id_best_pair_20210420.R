# identify pair of brain regions to use to demonstrate how r changes as the number of motion affected scans increases
# source("source_me_sd_pair_20210416.R")

smallCorArray <- array(dim = c(90,90,3))
smallCorArray[,,1] <- cor(n0.df[,1:90])
smallCorArray[,,2] <- cor(n3.df[,1:90])
smallCorArray[,,3] <- cor(n6.df[,1:90])

# look at results of below to see biggest change in r
smallCorArray[,,3] - smallCorArray[,,1]
