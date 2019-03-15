#Here I'm going to perform the analysis per family
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
matrix_soly=matrix_scale_effect[c(3,7,9,10),c(3,7,9,10)]
matrix_soly=matrix_effect_size[c(3,7,9,10),c(3,7,9,10)]

traits_sola=traits_all[c(3,7,9,10),-c(1,2)]
traits_sola_dist <- dist(traits_sola, diag = T)
a <- mantel(matrix_soly,traits_sola_dist)
summary(a)
mantel(matrix_soly,dist(traits_sola[,2]))
cor.test(sola$Cohen_d,sola$pollen_size)


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

save.image("Manuscript_draft/")

####
####BRASSICACEAE####
####
traits_brra=traits_all[c(4,8,9,10),-c(1,2)]
brra=effect_size_all[c(7:10),c(1,2)]

cor.test(brra$Cohen_d,traits_brra$mean_ovules)


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
