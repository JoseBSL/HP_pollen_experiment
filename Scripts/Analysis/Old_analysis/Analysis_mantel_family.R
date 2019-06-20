#Here I'm going to perform the analysis per family
#Just Solanaceae and Brassicaceae, Convolvulaceae has just two species
#load libraries
library(vegan)
library(nlme)
library(dplyr)
library(reshape2)
#load data
matrix_scale_effect <- readRDS("Data/matrix_scale_effect.Rda")
matrix_effect_size <- readRDS("Data/matrix_effect_size.RData")
traits_all <- read.csv("Data/traits_all.csv", sep=",")
rownames(traits_all) <- rownames(matrix_scale_effect)
si_index <- readRDS("Data/si_index.RData")
traits_all$si_index <- si_index
effect_size_all <- readRDS("Data/effect_size_all.RData")
effect_size=melt(matrix_effect_size)
####
####SOLANACEAE####
####
#First Mantel between all traits and Hp effect
#Also with effect sizes
matrix_soly=matrix_scale_effect[c(3,7,9,10),c(3,7,9,10)]
matrix_soly_1=matrix_effect_size[c(3,7,9,10),c(3,7,9,10)]
traits_sola=traits_all[c(3,7,9,10),-c(1,2)]
#scale traits previously analysis
traits_sola=traits_sola[,-c(1,7,11)]
traits_sola=scale(traits_sola)
traits_sola_dist <- dist(traits_sola)
#Mantel vegan package
mantel(dist(matrix_soly),traits_sola_dist)
#Mantel ecodist package
mantel(matrix_soly, traits_sola_dist)
trial=traits_all[,5:10]
traits_sola=as.data.frame(traits_sola)
#maximum rank analysis bioenv
bioenv(matrix_soly,traits_sola)
#Best rank correlation: Mean pollen per anther, style length and si index
#I select these 3 and run mantel with them
best_rank=traits_sola[,c(3,9,13)]
mantel(matrix_soly,dist(best_rank))
protest(dist(matrix_soly),dist(best_rank))

#r=-0.79, p=1
#looking for significance among the maximum rank variables
str(traits_sola)
cor.test(sola$Cohen_d,sola$pollen_size)
traits_sola=scale(traits_sola)
#Now I perform Mantel test between Hp effect matrix and trait by trait
#Selfing rate
mantel(matrix_soly,dist(traits_sola[,1]))
#mantel(dist(matrix_soly),dist(traits_sola[,1]))
#matrix and istance matrix of Hp, checking if the results are different
mantel(matrix_soly_1,dist(traits_sola[,1]))
protest(dist(matrix_soly),dist(traits_sola[,1]))


#This last one is with effect sizes
#Pollen size
mantel(matrix_soly,dist(traits_sola[,2]))
#mantel(dist(matrix_soly),dist(traits_sola[,2]))
#Effect size Mantel
mantel(matrix_soly_1,dist(traits_sola[,2]))
protest(dist(matrix_soly),dist(traits_sola[,2]))

#mean_pollen_anther
mantel(matrix_soly,dist(traits_sola[,3]))
#mantel(dist(matrix_soly),dist(traits_sola[,3]))
mantel(matrix_soly_1,dist(traits_sola[,3]))
protest(dist(matrix_soly),dist(traits_sola[,3]))

#mean_ovules
mantel(matrix_soly,dist(traits_sola[,4]))
#mantel(dist(matrix_soly),dist(traits_sola[,4]))
mantel(matrix_soly_1,dist(traits_sola[,4]))
protest(dist(matrix_soly),dist(traits_sola[,4]))

#pollen_ovule_ratio
mantel(matrix_soly,dist(traits_sola[,5]))
#mantel(dist(matrix_soly),dist(traits_sola[,5]))
mantel(matrix_soly_1,dist(traits_sola[,5]))
protest(dist(matrix_soly),dist(traits_sola[,5]))

#stigma_area
mantel(matrix_soly,dist(traits_sola[,6]))
#mantel(dist(matrix_soly),dist(traits_sola[,6]))
mantel(matrix_soly_1,dist(traits_sola[,6]))
protest(dist(matrix_soly),dist(traits_sola[,6]))


#stigma_length
mantel(matrix_soly,dist(traits_sola[,7]))
#mantel(dist(matrix_soly),dist(traits_sola[,7]))
mantel(matrix_soly_1,dist(traits_sola[,7]))
protest(dist(matrix_soly),dist(traits_sola[,7]))



#stigma_width
mantel(matrix_soly,dist(traits_sola[,8]))
#mantel(dist(matrix_soly),dist(traits_sola[,8]))
mantel(matrix_soly_1,dist(traits_sola[,8]))
protest(dist(matrix_soly),dist(traits_sola[,8]))


#style_length
mantel(matrix_soly,dist(traits_sola[,9]))
mantel(dist(matrix_soly),dist(traits_sola[,9]))
mantel(matrix_soly_1,dist(traits_sola[,9]))
protest(dist(matrix_soly),dist(traits_sola[,9]))


#style_width
mantel(matrix_soly,dist(traits_sola[,10]))
#mantel(dist(matrix_soly),dist(traits_sola[,10]))
mantel(matrix_soly_1,dist(traits_sola[,10]))
protest(dist(matrix_soly),dist(traits_sola[,10]))


#ovary_width
mantel(matrix_soly,dist(traits_sola[,11]))
#mantel(dist(matrix_soly),dist(traits_sola[,11]))
mantel(matrix_soly_1,dist(traits_sola[,11]))
protest(dist(matrix_soly),dist(traits_sola[,11]))


#ovary_length
mantel(matrix_soly,dist(traits_sola[,12]))
#mantel(dist(matrix_soly),dist(traits_sola[,12]))
mantel(matrix_soly_1,dist(traits_sola[,12]))
protest(dist(matrix_soly),dist(traits_sola[,12]))


#si_index
mantel(matrix_soly,dist(traits_sola[,13]))
mantel(dist(matrix_soly),dist(traits_sola[,13]))
mantel(matrix_soly_1,dist(traits_sola[,13]))
protest(dist(matrix_soly),dist(traits_sola[,13]))


#Simple Pearson test between traits and effect
#Style length seems to be the more interesting trait

traits_sola$species <- c("CAAN", "PEIN","SOLY","SOME")
effect_size_sola=effect_size_all[c(1:4),c(1,2)]
effect_size_sola$species <- c("SOME", "SOLY","PEIN","CAAN")
sola <- merge(effect_size_sola,traits_sola,by="species")
sola$style_length<- sola$style_length/1000
cor.test(sola$Cohen_d,sola$style_length)
library("ggpubr")
ggscatter(sola, x = "Cohen_d", y = "style_length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hedges' g", ylab = "Style length (mm)", main="A) Solanaceae")

#Doing it also with HP effect instead of effect sizes
b=as.data.frame(matrix_soly)
a<- rowMeans(b)
a=stack(a)
colnames(a)[2]<- "species"
sola <- merge(a,traits_sola,by="species")
cor.test(sola$values,sola$style_length)

ggscatter(sola, x = "values", y = "style_length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hp effect", ylab = "Style length (mm)", main="A) Solanaceae")

####
####BRASSICACEAE####
####

matrix_brra=matrix_scale_effect[c(1,2,4,8),c(1,2,4,8)]
traits_brra=traits_all[c(1,2,4,8),-c(1,2)]
brra=effect_size_all[c(7:10),c(1,2)]
#scale traits and removed unused variables (stigma, type, number of anthers)
traits_brra=scale(traits_brra[,-c(1,7,11)])
mantel(matrix_brra,dist(traits_brra))

protest(dist(matrix_brra),dist(traits_brra))

#
cor.test(brra$Cohen_d,traits_brra$mean_ovules)
cor.test(brra$Cohen_d,traits_brra$mean_ovules)
bioenv(matrix_brra,traits_brra)

#Selfing_rate
mantel(matrix_brra,dist(traits_brra[,1]))
protest(dist(matrix_brra),dist(traits_brra[,1]))

#pollen_size
mantel(matrix_brra,dist(traits_brra[,2]))
protest(dist(matrix_brra),dist(traits_brra[,2]))

#mean_pollen_anther
mantel(matrix_brra,dist(traits_brra[,3]))
protest(dist(matrix_brra),dist(traits_brra[,3]))

#mean_ovules
mantel(matrix_brra,dist(traits_brra[,4]))
protest(dist(matrix_brra),dist(traits_brra[,4]))

#pollen_ovule_ratio
mantel(matrix_brra,dist(traits_brra[,5]))
protest(dist(matrix_brra),dist(traits_brra[,5]))

#stigma_area
mantel(matrix_brra,dist(traits_brra[,6]))
protest(dist(matrix_brra),dist(traits_brra[,6]))

#stigma_length
mantel(matrix_brra,dist(traits_brra[,7]))
protest(dist(matrix_brra),dist(traits_brra[,7]))

#stigma_width
mantel(matrix_brra,dist(traits_brra[,8]))
protest(dist(matrix_brra),dist(traits_brra[,8]))

#style_length
mantel(matrix_brra,dist(traits_brra[,9]))
protest(dist(matrix_brra),dist(traits_brra[,9]))


#style_width
mantel(matrix_brra,dist(traits_brra[,10]))
protest(dist(matrix_brra),dist(traits_brra[,10]))


#ovary_width
mantel(matrix_brra,dist(traits_brra[,11]))
protest(dist(matrix_brra),dist(traits_brra[,11]))

#ovary_length
mantel(matrix_brra,dist(traits_brra[,12]))
protest(dist(matrix_brra),dist(traits_brra[,12]))

#si_index
mantel(matrix_brra,dist(traits_brra[,13]))
protest(dist(matrix_brra),dist(traits_brra[,13]))



####
effect_size_all$Family<- c("SOME","SOLY","PEIN", "CAAN", "IPPU", "IPAQ", "SIAL", "ERSA", "BRRA", "BROL")

traits_all$Family <- rownames(traits_all)
a<-merge(effect_size_all,traits_all, by="Family")
cor.test(a$Cohen_d,a[,18])
ggscatter(a, x = "Cohen_d", y = "style_length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hedges' g", ylab = "Style length (mm)", main="A) Solanaceae")


save.image("Manuscript_draft/Data/family_cor.RData")



#Example Mantel test from ecodist package
library("ecodist")
set.seed(NULL)

set.seed(9876)
sampleloc <- 1:20
species <- matrix(rnorm(100), nrow = 20, ncol = 5)
sampleloc.edist <- distance(sampleloc, "euclidean")
species.bcdist <- distance(species, "bray-curtis")
mantel(species.bcdist ~ sampleloc.edist)
traits_sola=distance(traits_sola, "bray-curtis")
matrix_soly <- distance(matrix_soly, "euclidean")
#Ok now I'm able to running it
mantel(matrix_soly~ traits_sola, nperm = 1000, nboot = 500, pboot = 0.9, cboot = 0.95)
