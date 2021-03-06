#Script for analysis of correlations between effect size matrix and traits

#LOAD LIBRARIES
library(vegan)

#
##LOAD DATA
#
#load effect sizes
matrix_effect_size <- readRDS("Data/Rdata/matrix_effect_size.RData")
#load traits
traits_all <- read.csv("Data/Csv/traits_scinames.csv")
traits_all=traits_all[,-c(1:4,10,14)]
#Scale traits
traits_all_scaled=scale(traits_all)
#load phylogenetic distance
evo_distance_its <- readRDS("Data/RDS/evo_distance_its.RDS")
evo_distance_rbcl <- readRDS("Data/RDS/evo_distance_rbcl.RDS")

combined_distance<- evo_distance_its+evo_distance_rbcl
mean_distance<-combined_distance/2



#
##ANALYSIS
#

#Effect size~traits
#First all the traits
mantel(matrix_effect_size,dist(traits_all_scaled))
protest(matrix_effect_size,dist(traits_all_scaled))
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

#save.image("Manuscript_draft/Data/table_s4.RData")


#Effect size~phylogenetic distance
mantel(matrix_effect_size,sqrt(evo_distance_its))
mantel(matrix_effect_size,sqrt(evo_distance_rbcl))
mantel(matrix_effect_size,sqrt(mean_distance))
protest(matrix_effect_size,sqrt(mean_distance))
#Now by family 
matrix_soly=matrix_effect_size[c(3,7,9,10),c(3,7,9,10)]
traits_soly=traits_all_scaled[c(3,7,9,10),]

#SOLANACEAE
mantel(matrix_soly,dist(traits_soly))
protest(matrix_soly,dist(traits_soly))

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

mantel(matrix_brra,dist(traits_brra)) 
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

#save.image("Manuscript_draft/Data/table_s5.RData")

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
selfing_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,1]))
#Pollen size
pollen_size_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,2]))
#Pollen per anther
mean_pollen_anther_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,3]))
#Ovules
mean_ovules_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,4]))
#Pollen ovule ratio
pollen_ovule_ratio_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,5]))
#Stigmatic area
stigma_area_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,6]))
#Stigma length
stigma_length_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,7]))
#Stigma width
stigma_width_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,8]))
#Style length
style_length_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,9]))
#Style width
style_width_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,10]))
#Ovary width
ovary_width_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,11]))
#Ovary length
ovary_length_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,12]))
#SI index
si_index_pro <- protest(matrix_effect_size,dist(traits_all_scaled[,13]))
si_index_pro[3]#sum squares
si_index_pro[6]#correlation
si_index_pro[13]#significance
#Effect size~phylogenetic distance
protest(matrix_effect_size,evo_distance_its)
protest(matrix_effect_size,evo_distance_rbcl)

protest(matrix_effect_size,dist((traits_all_scaled[,4]/traits_all_scaled[,6])))
protest(matrix_effect_size,dist((traits_all_scaled[,9]/traits_all_scaled[,6])))
protest(matrix_effect_size,dist((traits_all_scaled[,6]/traits_all_scaled[,9])))




#SOLANACEAE
protest(matrix_soly,traits_soly)
#Selfing rate
selfing_pro_sol <- protest(matrix_soly,dist(traits_soly[,1]))
#Pollen size
pollen_size_pro_sol <- protest(matrix_soly,dist(traits_soly[,2]))
#Pollen per anther
mean_pollen_anther_pro_sol <- protest(matrix_soly,dist(traits_soly[,3]))
#Ovules
mean_ovules_pro_sol <- protest(matrix_soly,dist(traits_soly[,4]))
#Pollen ovule ratio
pollen_ovule_ratio_pro_sol <- protest(matrix_soly,dist(traits_soly[,5]))
#Stigmatic area
stigma_area_pro_sol <- protest(matrix_soly,dist(traits_soly[,6]))
#Stigma length
stigma_length_pro_sol <- protest(matrix_soly,dist(traits_soly[,7]))
#Stigma width
stigma_width_pro_sol <- protest(matrix_soly,dist(traits_soly[,8]))
#Style length
style_length_pro_sol <- protest(matrix_soly,dist(traits_soly[,9]))
#Style width
style_width_pro_sol <- protest(matrix_soly,dist(traits_soly[,10]))
#Ovary width
ovary_width_pro_sol <- protest(matrix_soly,dist(traits_soly[,11]))
#Ovary length
ovary_length_pro_sol <- protest(matrix_soly,dist(traits_soly[,12]))
#SI index
si_index_pro_sol <- protest(matrix_soly,dist(traits_soly[,13]))  


protest(matrix_soly,dist(scale(traits_soly[,4]/traits_soly[,6])))
mantel(matrix_soly,dist(scale(traits_soly[,4]/traits_soly[,6])))
protest(matrix_soly,dist(scale(traits_soly[,4]/traits_soly[,9])))
protest(matrix_soly,dist((traits_soly[,6]/traits_soly[,9])))


#BRASSICACEAE
matrix_brra=matrix_effect_size[c(1,2,4,8),c(1,2,4,8)]
matrix_brra_its=evo_distance_its[c(1,2,4,8),c(1,2,4,8)]
traits_brra=traits_all_scaled[c(1,2,4,8),]

protest(matrix_brra,traits_brra) 
protest(matrix_brra,matrix_brra_its) 

#Selfing rate
selfing_pro_bra <- protest(matrix_brra,dist(traits_brra[,1]))
#Pollen size
pollen_size_pro_bra <- protest(matrix_brra,dist(traits_brra[,2]))
#Pollen per anther
mean_pollen_anther_pro_bra <- protest(matrix_brra,dist(traits_brra[,3]))
#Ovules
mean_ovules_pro_bra <- protest(matrix_brra,dist(traits_brra[,4]))
#Pollen ovule ratio
pollen_ovule_ratio_pro_bra <- protest(matrix_brra,dist(traits_brra[,5]))
#Stigmatic area
stigma_area_pro_bra <- protest(matrix_brra,dist(traits_brra[,6]))
#Stigma length
stigma_length_pro_bra <- protest(matrix_brra,dist(traits_brra[,7]))
#Stigma width
stigma_width_pro_bra <- protest(matrix_brra,dist(traits_brra[,8]))
#Style length
style_length_pro_bra <- protest(matrix_brra,traits_brra[,9])
plot(style_length_pro_bra)
#Style width
style_width_pro_bra <- protest(matrix_brra,dist(traits_brra[,10]))
#Ovary width
ovary_width_pro_bra <- protest(matrix_brra,dist(traits_brra[,11]))
#Ovary length
ovary_length_pro_bra <- protest(matrix_brra,dist(traits_brra[,12]))
#SI index
si_index_pro_bra <- protest(matrix_brra,dist(traits_brra[,13]))
#save.image("Manuscript_draft/Data/table_s4.RData")

mantel(matrix_brra,dist(scale(traits_brra[,4]/traits_brra[,9])))
mantel(matrix_brra,dist(scale(traits_brra[,6]/traits_brra[,9])))

protest(matrix_brra,dist(scale(traits_brra[,4]/traits_brra[,9])))
protest(matrix_effect_size,dist((traits_all[,9]/traits_all[,6])))

