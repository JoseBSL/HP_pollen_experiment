#Script for analysis of correlations between effect size matrix and traits

#LOAD LIBRARIES
library(vegan)

#LOAD DATA
matrix_effect_size <- readRDS("Data/matrix_effect_size.RData")
traits_all <- read.csv("Data/traits_scinames.csv")
traits_all=traits_all[,-c(1:4,10,14)]
#Scale traits
traits_all_scaled=scale(traits_all)

#ANALYSIS
#First all the traits
mantel(matrix_effect_size,dist(traits_all_scaled))
#Now trait by trait
#Selfing rate
mantel(matrix_effect_size,dist(traits_all_scaled[,1]))
#Pollen size
mantel(matrix_effect_size,dist(traits_all_scaled[,2]))
#Pollen per anther
mantel(matrix_effect_size,dist(traits_all_scaled[,3]))
#Ovules
mantel(matrix_effect_size,dist(traits_all_scaled[,4]))
#Pollen ovule ratio
mantel(matrix_effect_size,dist(traits_all_scaled[,5]))
#Stigmatic area
mantel(matrix_effect_size,dist(traits_all_scaled[,6]))
#Stigma length
mantel(matrix_effect_size,dist(traits_all_scaled[,7]))
#Stigma width
mantel(matrix_effect_size,dist(traits_all_scaled[,8]))
#Style length
mantel(matrix_effect_size,dist(traits_all_scaled[,9]))
#Style width
mantel(matrix_effect_size,dist(traits_all_scaled[,10]))
#Ovary width
mantel(matrix_effect_size,dist(traits_all_scaled[,11]))
#Ovary length
mantel(matrix_effect_size,dist(traits_all_scaled[,12]))
#SI index
mantel(matrix_effect_size,dist(traits_all_scaled[,13]))

