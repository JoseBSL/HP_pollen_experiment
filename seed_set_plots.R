
#In this script I'm going to represent the seed set of the different species 

#First species Petunia integrifolia, CODE=PEIN, N=10 for each treatment, just represented the ones that we have obtained seed set


library(reshape2)

seed_set <- read.csv("PEIN_seed_set.csv", sep=";")

seed_set <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = seed_set, na.rm= TRUE)

plot(seed_set, cex.axis=0.5)





