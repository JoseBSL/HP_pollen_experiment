#Analysis with traits

traits_all <- read.csv("Data/traits_all.csv", sep=",")
si_index <- readRDS("Data/si_index.RData")
traits_all$si_index <- si_index
traits_all <- traits_all[,-c(1,2)]

traits_all_dist <- dist(traits_all)
mantel(matrix_scale_effect, traits_all_dist)

#Here I'm scaling all... 
traits_all_scaled <- scale(traits_all)
traits_all_dist <- dist(traits_all_scaled)
mantel(matrix_scale_effect, traits_all_dist)

#Now scale just 
traits_all_scaled <- scale(traits_all[,8:15])
traits_all_dist <- dist(traits_all_scaled)
mantel(matrix_scale_effect, traits_all_dist)


traits_all_scaled <- (traits_all[,16])
traits_all_dist <- dist(traits_all_scaled)
mantel(matrix_scale_effect, traits_all_dist)



mantel(matrix_scale_effect, dist(traits_all[1]))



