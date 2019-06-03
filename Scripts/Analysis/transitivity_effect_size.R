#Analysis of transitivity of pollen effect

library(statnet)
library(reshape2)

#In this script I'm going to plot the transitivity of the effect oh HP pollen with effect sizes

effect <- readRDS("Data/matrix_effect_size.RData")
effect[effect>0]<-0
effect=abs(effect)
