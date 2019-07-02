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

combined_distance<- evo_distance_its+evo_distance_rbcl
mean_distance<-combined_distance/2



#
##ANALYSIS
#

#Effect size~traits
#First all the traits
mantel(matrix_effect_size,dist(traits_all_scaled))
#Now trait by trait
#Selfing rate
selfing <- mantel(matrix_effect_size,dist(traits_all_scaled[,1]))

selfing[4]
#Pollen size
pollen_size <- mantel(matrix_effect_size,dist(traits_all_scaled[,2]))
#Pollen per anther
mean_pollen_anther<-mantel(matrix_effect_size,dist(traits_all_scaled[,3]))
#Ovules
mean_ovules <- mantel(matrix_effect_size,dist(traits_all_scaled[,4]))
#Pollen ovule ratio
pollen_ovule_ratio <- mantel(matrix_effect_size,dist(traits_all_scaled[,5]))
#Stigmatic area
stigma_area <- mantel(matrix_effect_size,dist(traits_all_scaled[,6]))
#Stigma length
stigma_length <- mantel(matrix_effect_size,dist(traits_all_scaled[,7]))
#Stigma width
stigma_width <- mantel(matrix_effect_size,dist(traits_all_scaled[,8]))
#Style length
style_length <- mantel(matrix_effect_size,dist(traits_all_scaled[,9]))
#Style width
style_width <- mantel(matrix_effect_size,dist(traits_all_scaled[,10]))
#Ovary width
ovary_width <- mantel(matrix_effect_size,dist(traits_all_scaled[,11]))
#Ovary length
ovary_length <- mantel(matrix_effect_size,dist(traits_all_scaled[,12]))
#SI index
si_index <- mantel(matrix_effect_size,dist(traits_all_scaled[,13]))

save.image("Manuscript_draft/Data/table_s4.RData")


#Effect size~phylogenetic distance
mantel(matrix_effect_size,evo_distance_its)
mantel(matrix_effect_size,evo_distance_rbcl)
mantel(matrix_effect_size,mean_distance)
#Now by family 
matrix_soly=matrix_effect_size[c(3,7,9,10),c(3,7,9,10)]
traits_soly=traits_all_scaled[c(3,7,9,10),]

#SOLANACEAE
mantel(matrix_soly,traits_soly)
#Selfing rate
selfing_soly <- mantel(matrix_soly,dist(traits_soly[,1]))
#Pollen size
pollen_size_soly <- mantel(matrix_soly,dist(traits_soly[,2]))
#Pollen per anther
mean_pollen_anther_soly <- mantel(matrix_soly,dist(traits_soly[,3]))
#Ovules
mean_ovules_soly <- mantel(matrix_soly,dist(traits_soly[,4]))
#Pollen ovule ratio
pollen_ovule_ratio_soly <- mantel(matrix_soly,dist(traits_soly[,5]))
#Stigmatic area
stigma_area_soly <- mantel(matrix_soly,dist(traits_soly[,6]))
#Stigma length
stigma_length_soly <- mantel(matrix_soly,dist(traits_soly[,7]))
#Stigma width
stigma_width_soly <- mantel(matrix_soly,dist(traits_soly[,8]))
#Style length
style_length_soly <- mantel(matrix_soly,dist(traits_soly[,9]))
#Style width
style_width_soly <- mantel(matrix_soly,dist(traits_soly[,10]))
#Ovary width
ovary_width_soly <- mantel(matrix_soly,dist(traits_soly[,11]))
#Ovary length
ovary_length_soly <- mantel(matrix_soly,dist(traits_soly[,12]))
#SI index
si_index_soly <- mantel(matrix_soly,dist(traits_soly[,13]))  

#BRASSICACEAE
matrix_brra=matrix_effect_size[c(1,2,4,8),c(1,2,4,8)]
matrix_brra_its=evo_distance_its[c(1,2,4,8),c(1,2,4,8)]
traits_brra=traits_all_scaled[c(1,2,4,8),]

mantel(matrix_brra,traits_brra) 
mantel(matrix_brra,matrix_brra_its) 

#Selfing rate
selfing_bras <- mantel(matrix_brra,dist(traits_brra[,1]))
#Pollen size
pollen_size_bras <- mantel(matrix_brra,dist(traits_brra[,2]))
#Pollen per anther
mean_pollen_anther_bras <- mantel(matrix_brra,dist(traits_brra[,3]))
#Ovules
mean_ovules_bras <- mantel(matrix_brra,dist(traits_brra[,4]))
#Pollen ovule ratio
pollen_ovule_ratio_bras <- mantel(matrix_brra,dist(traits_brra[,5]))
#Stigmatic area
stigma_area_bras <- mantel(matrix_brra,dist(traits_brra[,6]))
#Stigma length
stigma_length_bras <- mantel(matrix_brra,dist(traits_brra[,7]))
#Stigma width
stigma_width_bras <- mantel(matrix_brra,dist(traits_brra[,8]))
#Style length
style_length_bras <- mantel(matrix_brra,dist(traits_brra[,9]))
#Style width
style_width_bras <- mantel(matrix_brra,dist(traits_brra[,10]))
#Ovary width
ovary_width_bras <- mantel(matrix_brra,dist(traits_brra[,11]))
#Ovary length
ovary_length_bras <- mantel(matrix_brra,dist(traits_brra[,12]))
#SI index
si_index_bras <- mantel(matrix_brra,dist(traits_brra[,13]))

save.image("Manuscript_draft/Data/table_s5.RData")

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
cor(matrix_brra,dist(traits_brra[,13]), method="pearson" ) 
 protest(matrix_brra, dist(traits_brra[,13]),  symmetric = FALSE)
 a<- protest(matrix_brra, dist(traits_brra[,13]))
 
plot(a, kind = "2")
summary(a)
residuals(a)
