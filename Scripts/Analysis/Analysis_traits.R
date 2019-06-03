#Analysis with traits
library(vegan)

matrix_scale_effect <- readRDS("Data/matrix_scale_effect.Rda")

traits_all <- read.csv("Data/traits_all.csv", sep=",")
#si_index <- readRDS("Data/si_index.RData")
si_index <- readRDS("Data/si_index_1.RData")

traits_all$si_index <- si_index
traits_all <- traits_all[,-c(1,2)]

traits_all_dist <- dist(traits_all)
mantel(matrix_scale_effect, traits_all_dist)

#Here I'm scaling all... We obtain a similar result but the r coefficient decrease
#and p values increase
traits_all_scaled <- scale(traits_all)
traits_all_dist <- dist(traits_all_scaled)
mantel(matrix_scale_effect, traits_all_dist)



traits_all_scaled <- (traits_all[,16])
traits_all_dist <- dist(traits_all_scaled)
mantel(matrix_scale_effect, traits_all_dist)


#stigma_type
mantel(matrix_scale_effect, dist(traits_all[1]))
mantel(matrix_scale_effect, dist(scale(traits_all[1])))
#Selfing_rate
mantel(matrix_scale_effect, dist(traits_all[2]))
mantel(matrix_scale_effect, dist(scale(traits_all[2])))
#pollen_size
mantel(matrix_scale_effect, dist(traits_all[3]))
mantel(matrix_scale_effect, dist(scale(traits_all[3])))
#mean_pollen_anther
mantel(matrix_scale_effect, dist(traits_all[4]))
mantel(matrix_scale_effect, dist(scale(traits_all[4])))
#mean_ovules
mantel(matrix_scale_effect, dist(traits_all[5]))
mantel(matrix_scale_effect, dist(scale(traits_all[5])))
#pollen_ovule_ratio
mantel(matrix_scale_effect, dist(traits_all[6]))
mantel(matrix_scale_effect, dist(scale(traits_all[6])))
#anthers
mantel(matrix_scale_effect, dist(traits_all[7]))
mantel(matrix_scale_effect, dist(scale(traits_all[7])))
#stigma_area
mantel(matrix_scale_effect, dist(traits_all[8]))
mantel(matrix_scale_effect, dist(scale(traits_all[2])))
#stigma_length
mantel(matrix_scale_effect, dist(traits_all[9]))
mantel(matrix_scale_effect, dist(scale(traits_all[9])))
#stigma_width
mantel(matrix_scale_effect, dist(traits_all[10]))
mantel(matrix_scale_effect, dist(scale(traits_all[10])))
#stigma_surface
mantel(matrix_scale_effect, dist(traits_all[11]))
mantel(matrix_scale_effect, dist(scale(traits_all[11])))
#style_length
mantel(matrix_scale_effect, dist(traits_all[12]))
mantel(matrix_scale_effect, dist(scale(traits_all[12])))
#style_width
mantel(matrix_scale_effect, dist(traits_all[13]))
mantel(matrix_scale_effect, dist(scale(traits_all[13])))
#ovary_width
mantel(matrix_scale_effect, dist(traits_all[14]))
mantel(matrix_scale_effect, dist(scale(traits_all[14])))
#ovary_length
mantel(matrix_scale_effect, dist(traits_all[15]))
mantel(matrix_scale_effect, dist(scale(traits_all[15])))
#si_index
mantel(matrix_scale_effect, dist(traits_all[16]))
mantel(matrix_scale_effect, dist(scale(traits_all[16])))

