#Here I'm going to perform the analysis per family
#load libraries
library(vegan)
library(nlme)

#load data
matrix_scale_effect <- readRDS("Data/matrix_scale_effect.Rda")
matrix_effect_size <- readRDS("Data/matrix_effect_size.RData")
traits_all <- read.csv("Data/traits_all.csv", sep=",")
rownames(traits_all) <- rownames(matrix_scale_effect)
si_index <- readRDS("Data/si_index.RData")
traits_all$si_index <- si_index
effect_size_all <- readRDS("Data/effect_size_all.RData")

####
####SOLANACEAE####
####
matrix_sola=matrix_scale_effect[c(3,7,9,10),c(3,7,9,10)]
traits_sola=traits_all[c(3,7,9,10),-c(1,2)]
traits_sola_dist <- dist(traits_sola, diag = T)
a <- mantel(matrix_soly,traits_sola_dist)
summary(a)



traits_sola$species <- c("CAAN", "PEIN","SOLY","SOME")
effect_size_sola=effect_size_all[c(1:4),c(1,2)]
effect_size_sola$species <- c("SOME", "SOLY","PEIN","CAAN")
sola <- merge(effect_size_sola,traits_sola,by="species")
cor.test(sola$Cohen_d,sola$style_length)
library("ggpubr")
ggscatter(sola, x = "Cohen_d", y = "style_length", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "", ylab = "")