#Script for analysis of correlations between effect size matrix and traits

#LOAD LIBRARIES
library(vegan)

#
##LOAD DATA
#
#load effect sizes
matrix_effect_size <- readRDS("Data/matrix_effect_size.RData")
#load traits
traits_all <- read.csv("Data/traits_scinames.csv")
traits_all=traits_all[,-c(1:4,10,14)]
#Scale traits
traits_all_scaled=scale(traits_all)
#load phylogenetic distance
evo_distance_its <- readRDS("Data/evo_distance_its.RDS")
evo_distance_rbcl <- readRDS("Data/evo_distance_rbcl.RDS")

#
##ANALYSIS
#

#Effect size~traits

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

#Effect size~phylogenetic distance
mantel(matrix_effect_size,evo_distance_its)
mantel(matrix_effect_size,evo_distance_rbcl)
#Now by family 
matrix_soly=matrix_effect_size[c(3,7,9,10),c(3,7,9,10)]
traits_soly=traits_all_scaled[c(3,7,9,10),]

#SOLANACEAE
mantel(matrix_soly,traits_soly)
#Selfing rate
mantel(matrix_soly,dist(traits_soly[,1]))
#Pollen size
mantel(matrix_soly,dist(traits_soly[,2]))
#Pollen per anther
mantel(matrix_soly,dist(traits_soly[,3]))
#Ovules
mantel(matrix_soly,dist(traits_soly[,4]))
#Pollen ovule ratio
mantel(matrix_soly,dist(traits_soly[,5]))
#Stigmatic area
mantel(matrix_soly,dist(traits_soly[,6]))
#Stigma length
mantel(matrix_soly,dist(traits_soly[,7]))
#Stigma width
mantel(matrix_soly,dist(traits_soly[,8]))
#Style length
mantel(matrix_soly,dist(traits_soly[,9]))
#Style width
mantel(matrix_soly,dist(traits_soly[,10]))
#Ovary width
mantel(matrix_soly,dist(traits_soly[,11]))
#Ovary length
mantel(matrix_soly,dist(traits_soly[,12]))
#SI index
mantel(matrix_soly,dist(traits_soly[,13]))  

#BRASSICACEAE
matrix_brra=matrix_effect_size[c(1,2,4,8),c(1,2,4,8)]
matrix_brra_its=evo_distance_its[c(1,2,4,8),c(1,2,4,8)]
traits_brra=traits_all_scaled[c(1,2,4,8),]

mantel(matrix_brra,traits_brra) 
mantel(matrix_brra,matrix_brra_its) 

#Selfing rate
mantel(matrix_brra,dist(traits_brra[,1]))
#Pollen size
mantel(matrix_brra,dist(traits_brra[,2]))
#Pollen per anther
mantel(matrix_brra,dist(traits_brra[,3]))
#Ovules
mantel(matrix_brra,dist(traits_brra[,4]))
#Pollen ovule ratio
mantel(matrix_brra,dist(traits_brra[,5]))
#Stigmatic area
mantel(matrix_brra,dist(traits_brra[,6]))
#Stigma length
mantel(matrix_brra,dist(traits_brra[,7]))
#Stigma width
mantel(matrix_brra,dist(traits_brra[,8]))
#Style length
mantel(matrix_brra,dist(traits_brra[,9]))
#Style width
mantel(matrix_brra,dist(traits_brra[,10]))
#Ovary width
mantel(matrix_brra,dist(traits_brra[,11]))
#Ovary length
mantel(matrix_brra,dist(traits_brra[,12]))
#SI index
mantel(matrix_brra,dist(traits_brra[,13]))


#
#
#PROTEST
#
#
#
##ANALYSIS
#

#Effect size~traits

#First all the traits
protest(matrix_effect_size,dist(traits_all_scaled))
#Now trait by trait
#Selfing rate
protest(matrix_effect_size,dist(traits_all_scaled[,1]))
#Pollen size
protest(matrix_effect_size,dist(traits_all_scaled[,2]))
#Pollen per anther
protest(matrix_effect_size,dist(traits_all_scaled[,3]))
#Ovules
protest(matrix_effect_size,dist(traits_all_scaled[,4]))
#Pollen ovule ratio
protest(matrix_effect_size,dist(traits_all_scaled[,5]))
#Stigmatic area
protest(matrix_effect_size,dist(traits_all_scaled[,6]))
#Stigma length
protest(matrix_effect_size,dist(traits_all_scaled[,7]))
#Stigma width
protest(matrix_effect_size,dist(traits_all_scaled[,8]))
#Style length
protest(matrix_effect_size,dist(traits_all_scaled[,9]))
#Style width
protest(matrix_effect_size,dist(traits_all_scaled[,10]))
#Ovary width
protest(matrix_effect_size,dist(traits_all_scaled[,11]))
#Ovary length
protest(matrix_effect_size,dist(traits_all_scaled[,12]))
#SI index
protest(matrix_effect_size,dist(traits_all_scaled[,13]))

#Effect size~phylogenetic distance
protest(matrix_effect_size,evo_distance_its)
protest(matrix_effect_size,evo_distance_rbcl)



#SOLANACEAE
protest(matrix_soly,traits_soly)
#Selfing rate
protest(matrix_soly,dist(traits_soly[,1]))
#Pollen size
protest(matrix_soly,dist(traits_soly[,2]))
#Pollen per anther
protest(matrix_soly,dist(traits_soly[,3]))
#Ovules
protest(matrix_soly,dist(traits_soly[,4]))
#Pollen ovule ratio
protest(matrix_soly,dist(traits_soly[,5]))
#Stigmatic area
protest(matrix_soly,dist(traits_soly[,6]))
#Stigma length
protest(matrix_soly,dist(traits_soly[,7]))
#Stigma width
protest(matrix_soly,dist(traits_soly[,8]))
#Style length
protest(matrix_soly,dist(traits_soly[,9]))
#Style width
protest(matrix_soly,dist(traits_soly[,10]))
#Ovary width
protest(matrix_soly,dist(traits_soly[,11]))
#Ovary length
protest(matrix_soly,dist(traits_soly[,12]))
#SI index
protest(matrix_soly,dist(traits_soly[,13]))  

#BRASSICACEAE
matrix_brra=matrix_effect_size[c(1,2,4,8),c(1,2,4,8)]
matrix_brra_its=evo_distance_its[c(1,2,4,8),c(1,2,4,8)]
traits_brra=traits_all_scaled[c(1,2,4,8),]

protest(matrix_brra,traits_brra) 
protest(matrix_brra,matrix_brra_its) 

#Selfing rate
protest(matrix_brra,dist(traits_brra[,1]))
#Pollen size
protest(matrix_brra,dist(traits_brra[,2]))
#Pollen per anther
protest(matrix_brra,dist(traits_brra[,3]))
#Ovules
protest(matrix_brra,dist(traits_brra[,4]))
#Pollen ovule ratio
protest(matrix_brra,dist(traits_brra[,5]))
#Stigmatic area
protest(matrix_brra,dist(traits_brra[,6]))
#Stigma length
protest(matrix_brra,dist(traits_brra[,7]))
#Stigma width
protest(matrix_brra,dist(traits_brra[,8]))
#Style length
protest(matrix_brra,dist(traits_brra[,9]))
#Style width
protest(matrix_brra,dist(traits_brra[,10]))
#Ovary width
protest(matrix_brra,dist(traits_brra[,11]))
#Ovary length
protest(matrix_brra,dist(traits_brra[,12]))
#SI index
protest(matrix_brra,dist(traits_brra[,13]))

