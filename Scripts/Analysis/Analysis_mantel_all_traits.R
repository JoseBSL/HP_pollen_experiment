#Part of this script is from Analysis_mantel_test but because it was too long a bit messy
#I'm going to redo it here in a cleaner way

#
##
###
###### MATRIX EFFECT~TRAITS
###
##
#
library(vegan)

#From here I start working with the traits
traits_all <- read.csv("Data/traits_all.csv", sep=",")
rownames(traits_all) <- rownames(matrix_scale_effect)
si_index <- readRDS("Data/si_index.RData")
traits_all$si_index <- si_index
matrix_scale_effect <- readRDS("Data/matrix_scale_effect.Rda")
#I check Mantel between all the traits and the distance matrix of the effect of heterospecific pollen
#IMPORTANT: P values of Mantel seems to be of one sided test 
#For that, first I standarize the values of the different columns
traits_all <- traits_all[,-c(1,2)]
#For mantel I scale the dat.frame
traits_all_scaled <- scale(traits_all)
#Check if the columns are well scaled mean 0 and sd 1
colMeans(traits_all_scaled)
apply(traits_all_scaled, 2, sd)
#Seems that are well scaled 
#Now I apply Mantel
traits_all_scaled_dist <- dist(traits_all_scaled)
mantel(matrix_scale_effect, traits_all_scaled_dist)
mantel(matrix_scale_effect, dist(traits_all))

#significance=0.296, r=0.09
protest(matrix_scale_effect, traits_all_scaled_dist)
protest(dist(matrix_scale_effect),  dist(traits_all))

#significance=0.946, procustes correlation=0.5968
protest(matrix_scale_effect, traits_all_scaled)

#Bioenv, alternative way, similar to Mantel and procustes
#BUT it finds the variables that are  more relevant to our model
#It scale the variables, so it doesn´t matter if they are scaled or not
#We obtain the same result, I check below
traits_all_bioenv <- traits_all[,-c(1,7,11)]
traits_all_bioenv_scaled <- scale(traits_all_bioenv)
bioenv(matrix_scale_effect,traits_all_bioenv)
bioenv(matrix_scale_effect,traits_all_bioenv_scaled)
#bioenv(matrix_scale_effect,traits_all_bioenv_scaled[,-c(6,12,16)])
mantel(matrix_scale_effect,dist(traits_all_bioenv[,13]))
mantel(dist(matrix_scale_effect),dist(traits_all_bioenv[,13]))
protest(dist(matrix_scale_effect),dist(traits_all_bioenv[,13]))



#Start trait by trait

#0)si_index
traits_all_scaled_stigma <- traits_all_bioenv_scaled[,1]
traits_all_scaled_stigma <- as.data.frame(traits_all_scaled_stigma)
traits_all_scaled_stigma <- traits_all_scaled_stigma
rownames(traits_all_scaled_stigma) <- rownames(traits_all_scaled_stigma)
traits_stigma_dist <- dist(traits_all_scaled_stigma, diag=T, upper=T)

traits_si_index_dist <- dist(traits_all$si_index, diag=T, upper=T)
mantel(matrix_scale_effect, traits_si_index_dist)
mantel(matrix_effect_size, traits_si_index_dist)
mantel(abs(matrix_effect_size), traits_si_index_dist)
mantel(abs(matrix_scale_reverted), traits_si_index_dist)


protest(matrix_scale_effect, traits_si_index_dist)
#significance=0.373, procustes correlation=0.3208
bioenv(matrix_scale_effect~traits_all$si_index, method="pearson", trace=T)
#corr=-0.16
adonis(formula=matrix_scale_effect ~si_index ,data=traits_all)


#1)Selfing_rate
traits_all_scaled_stigma <- traits_all_bioenv_scaled[,1]
traits_all_scaled_stigma <- as.data.frame(traits_all_scaled_stigma)
traits_all_scaled_stigma <- traits_all_scaled_stigma
rownames(traits_all_scaled_stigma) <- rownames(traits_all_scaled_stigma)
traits_stigma_dist <- dist(traits_all_scaled_stigma, diag=T, upper=T)
mantel(matrix_scale_effect, traits_stigma_dist)
#significance=0.016, r=0.27
protest(matrix_scale_effect, traits_stigma_dist)
protest(dist(matrix_scale_effect), traits_stigma_dist)
#significance=0.373, procustes correlation=0.3208
#corr=-0.16
adonis(formula=matrix_scale_effect ~stigma_type ,data=traits_all)

#2)pollen_size
traits_all_scaled_self <- traits_all_bioenv_scaled[,2]
traits_all_scaled_self <- as.data.frame(traits_all_scaled_self)
traits_all_scaled_self <- traits_all_scaled_self
rownames(traits_all_scaled_self) <- rownames(traits_all_scaled_self)
traits_self_dist <- dist(traits_all_scaled_self, diag=T, upper=T)
mantel(matrix_scale_effect, traits_self_dist)


#significance=0.424, r=0.03015
protest(matrix_scale_effect, traits_self_dist)
protest(dist(matrix_scale_effect), traits_self_dist)

#significance=0597, procustes correlation=0.3609
adonis(formula=matrix_scale_effect ~Selfing_rate ,data=traits_all)
#R2=0.46, p=0.198
#Mantel gives very low correlation between selfing rate 


#3)mean_pollen_anther
mean_pollen_anther <- traits_all_bioenv_scaled[,3]
mean_pollen_anther <- as.data.frame(mean_pollen_anther)
mean_pollen_anther <- mean_pollen_anther
rownames(mean_pollen_anther) <- rownames(mean_pollen_anther)
mean_pollen_anther_dist <- dist(mean_pollen_anther, diag=T, upper=T)
mantel(matrix_scale_effect,mean_pollen_anther_dist)

#significance=0.233, r=0.1526
protest(matrix_scale_effect, mean_pollen_anther_dist)
protest(dist(matrix_scale_effect), mean_pollen_anther_dist)

#significance=0.519, procustes correlation=-0.05605
#correlation=0.11
adonis(formula=matrix_scale_effect ~mean_pollen_anther ,data=traits_all)


#4)mean_ovules
mean_ovules <- traits_all_bioenv_scaled[,4]
mean_ovules <- as.data.frame(pollen)
mean_ovules <- mean_ovules
rownames(mean_ovules) <- rownames(mean_ovules)
ovule_dist <- dist(mean_ovules, diag=T, upper=T)
mantel(matrix_scale_effect, ovule_dist) 

#significance=0.491,r=-0.1097
protest(matrix_scale_effect, ovule_dist)
protest(dist(matrix_scale_effect), ovule_dist)
#significance=0.907, procustes correlation=0.3174
#correlation=-0.13
adonis(formula=matrix_scale_effect ~mean_ovules ,data=traits_all)

#5)mean_ovules
pollen_ovule_ratio <- traits_all_bioenv_scaled[,5]
pollen_ovule_ratio <- as.data.frame(ovules)
rownames(pollen_ovule_ratio) <- rownames(pollen_ovule_ratio)
pollen_ovule_ratio_dist <- dist(pollen_ovule_ratio, diag=T, upper=T)
mantel(matrix_scale_effect, pollen_ovule_ratio_dist)
#significance=0.487, r=-0.08
protest(matrix_scale_effect, pollen_ovule_ratio_dist)
protest(dist(matrix_scale_effect), pollen_ovule_ratio_dist)
#0.884, procustes correlation=0.2985
#correlation=-0.14
adonis(formula=matrix_scale_effect ~pollen_ovule_ratio ,data=traits_all)

#6)Stigma_area
stigma_area <- traits_all_bioenv_scaled[,6]
stigma_area <- as.data.frame(stigma_area)
rownames(stigma_area) <- rownames(stigma_area)
stigma_area_dist <- dist(stigma_area, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_area_dist)
#significance=0.025, r=0.4002
protest(matrix_scale_effect, stigma_area_dist)
protest(dist(matrix_scale_effect), stigma_area_dist)
#significance=0.657, procustes correlation=0.4041
bioenv(matrix_scale_effect~traits_all$stigma_area, method="pearson", trace=T)
#correlation=-0.0859
adonis(formula=matrix_scale_effect ~stigma_area ,data=traits_all)


#7)Stigma_length
stigma_length <- traits_all_bioenv_scaled[,7]
stigma_length <- as.data.frame(stigma_length)
rownames(stigma_length) <- rownames(stigma_length)
stigma_length_dist <- dist(stigma_length, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_length_dist)
#significance=0.179, r=0.07872
protest(matrix_scale_effect, stigma_length_dist)
protest(dist(matrix_scale_effect), stigma_length_dist)

#significance=0.111, procustes correlation 0.4231
adonis(formula=matrix_scale_effect ~stigma_length ,data=traits_all)

#correlation=-0.167


#8)Stigma width
stigma_width <- traits_all_bioenv_scaled[,8]
stigma_width <- as.data.frame(stigma_width)
stigma_width <- stigma_width
rownames(stigma_width) <- rownames(stigma_width)
stigma_width_dist <- dist(stigma_width, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_width_dist)

#significance=0.891, r=-0.24
protest(matrix_scale_effect, stigma_width_dist)
protest(dist(matrix_scale_effect), stigma_width_dist)
#significance=0.253, procustes correlation=0.3819

#9)Style_length
style_length <- traits_all_bioenv_scaled[,9]
style_length <- as.data.frame(style_length)
rownames(style_length) <- rownames(style_length)
style_length_dist <- dist(style_length, diag=T, upper=T)
mantel(matrix_scale_effect, style_length_dist)
#significance=0.303, r=0.05021
protest(matrix_scale_effect, style_length_dist)
protest(dist(matrix_scale_effect), style_length_dist)

#significance=0.113, procustes correlation=0.3995
#correlation=-0.1757

#10)Style_width
style_width <- traits_all_bioenv_scaled[,10]
style_width <- as.data.frame(style_width)
rownames(style_width) <- rownames(style_width)
style_width_dist <- dist(style_width, diag=T, upper=T)
mantel(matrix_scale_effect, style_width_dist)
#significance=0.0787, r=0.07872
protest(matrix_scale_effect, style_width_dist)
protest(dist(matrix_scale_effect), style_width_dist)

#significance=0.115, procustes correlation=0.4231

#11)Ovary_width
ovary_width <- traits_all_bioenv_scaled[,11]
ovary_width <- as.data.frame(ovary_width)
rownames(ovary_width) <- rownames(ovary_width)
ovary_width_dist <- dist(ovary_width, diag=T, upper=T)
mantel(matrix_scale_effect, ovary_width_dist)


#significance=0.0787, r=0.07872
protest(matrix_scale_effect, ovary_width_dist)
protest(dist(matrix_scale_effect), ovary_width_dist)

#significance=0.115, procustes correlation=0.4231

#12)Ovary_length
ovary_length <- traits_all_bioenv_scaled[,12]
ovary_length <- as.data.frame(ovary_length)
rownames(ovary_length) <- rownames(ovary_length)
ovary_length_dist <- dist(ovary_length, diag=T, upper=T)
mantel(matrix_scale_effect, ovary_length_dist)


#significance=0.0787, r=0.07872
protest(matrix_scale_effect, ovary_length_dist)
protest(dist(matrix_scale_effect), ovary_length_dist)

#significance=0.115, procustes correlation=0.4231

#13)self_i_index
si_index <- traits_all_bioenv_scaled[,13]
si_index <- as.data.frame(si_index)
rownames(si_index) <- rownames(si_index)
si_index_dist <- dist(si_index, diag=T, upper=T)
mantel(matrix_scale_effect, si_index_dist)
mantel(dist(matrix_scale_effect), si_index_dist)

#significance=0.0787, r=0.07872
protest(matrix_scale_effect, si_index_dist)
protest(dist(matrix_scale_effect), si_index_dist)
