#Phylosignal package
#devtools::install_github("fkeck/phylosignal")

library(ape)
library(phylosignal)
library(adephylo)
library(phylobase)
library(phylosignal)
library(caret)
library(ggplot2)
library(seqHMM)
library(dplyr)
library(reshape2)
data(carni19)

tre <- read.tree(text=carni19$tre)
dat <- list()
dat$mass <- carni19$bm
dat$random <- rnorm(19, sd = 10)
dat$bm <- rTraitCont(tre)
dat <- as.data.frame(dat)
p4d <- phylo4d(tre, dat)
barplot.phylo4d(p4d, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(p4d = p4d, method = "all")
str(dat)

#Now I´m going to try to perform it with my data
pollen_tree=read.tree("Data/pollen_tree_no_outgroup.nwk")
matrix_scale_effect <- readRDS("Data/matrix_scale_effect.Rda")
as.data.frame(matrix_scale_effect)
matrix_scale_effect <- melt(matrix_scale_effect)
colnames(matrix_scale_effect)[3] <- "hp"
str(matrix_scale_effect)
matrix_scale_effect <- as.data.frame(matrix_scale_effect)
hp_mean_sp=matrix_scale_effect
hp_mean_sp <- dcast(Species ~ ., value.var = "hp", fun.aggregate = mean, data = matrix_scale_effect, na.rm= TRUE)
colnames(hp_mean_sp)[3] <- "hp"
colnames(hp_mean_sp)[2] <- "non_focal"

#rename species to match tree
rownames(hp_mean_sp) <- c("Brassica_oleracea","Brassica_rapa","Capsicum_annuum", "Eruca_vesicaria",
                        "Ipomoea_aquatica", "Ipomoea_purpurea", "Petunia_integrifolia", "Sinapis_alba",
                        "Solanum_lycopersicum", "Solanum_melongena")
str(hp_mean_sp)
hp_mean_sp=hp_mean_sp[,-1]
p4d <- phylo4d(pollen_tree, hp_mean_sp)
barplot.phylo4d(p4d, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(p4d = p4d, method = "all")
carni.lipa <- lipaMoran(p4d)
carni.lipa.p4d <- lipaMoran(p4d, as.p4d = TRUE)
barplot.phylo4d(p4d, bar.col=(carni.lipa$p.value < 0.05) + 1, center = FALSE , scale = FALSE)
barplot.phylo4d(carni.lipa.p4d, bar.col = (carni.lipa$p.value < 0.05) + 1, center = FALSE, scale = FALSE)
dotplot(p4d)


carni.lipa <- lipaMoran(p4d)
carni.lipa.p4d <- lipaMoran(p4d, as.p4d = TRUE)
barplot.phylo4d(p4d, bar.col=(carni.lipa$p.value < 0.05) + 1, center = FALSE , scale = FALSE)


traits_all <- read.csv("Data/traits_all.csv", sep=",")
rownames(traits_all) <- c("Brassica_oleracea","Brassica_rapa","Capsicum_annuum", "Eruca_vesicaria",
                          "Ipomoea_aquatica", "Ipomoea_purpurea", "Petunia_integrifolia", "Sinapis_alba",
                          "Solanum_lycopersicum", "Solanum_melongena")
si_index <- readRDS("Data/si_index_1.RData")
traits_all$si_index <- si_index
traits_all=traits_all[,-c(1,2)]
traits_all_subset=traits_all[,8:16]

#traits_all_subset=traits_all[,1:4]

p4d <- phylo4d(pollen_tree, traits_all_subset)
barplot.phylo4d(p4d, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(p4d = p4d, method = "all")
carni.lipa <- lipaMoran(p4d)
carni.lipa.p4d <- lipaMoran(p4d, as.p4d = TRUE)
barplot.phylo4d(p4d, bar.col=(carni.lipa$p.value < 0.05) + 1, center = FALSE , scale = FALSE)
barplot.phylo4d(carni.lipa.p4d, bar.col = (carni.lipa$p.value < 0.05) + 1, center = FALSE, scale = FALSE)
dotplot(p4d)

#Assessing the signal depth with correlograms
mass.crlg <- phyloCorrelogram(p4d, trait = "dt")
plot(mass.crlg)


dotplot(p4d, tree.type = "cladogram")




#Now I´m going to try to perform it with my data
pollen_tree=read.tree("Data/pollen_tree_no_outgroup.nwk")
matrix_scale_effect <- readRDS("Data/matrix_effect_size.RData")
as.data.frame(matrix_scale_effect)
matrix_scale_effect <- melt(matrix_scale_effect)
colnames(matrix_scale_effect)[3] <- "hp"
str(matrix_scale_effect)
matrix_scale_effect <- as.data.frame(matrix_scale_effect)
hp_mean_sp=matrix_scale_effect
hp_mean_sp <- dcast(Focal ~ ., value.var = "hp", fun.aggregate = mean, data = matrix_scale_effect, na.rm= TRUE)
colnames(hp_mean_sp)[2] <- "hp"
str(hp_mean_sp)
hp_mean_sp=hp_mean_sp[,-1]
hp_mean_sp[hp_mean_sp>0]<-0
hp_mean_sp=abs(hp_mean_sp)
p4d <- phylo4d(pollen_tree, hp_mean_sp)
barplot.phylo4d(p4d, tree.type = "phylo", tree.ladderize = TRUE)
phyloSignal(p4d = p4d, method = "all")
carni.lipa <- lipaMoran(p4d)
carni.lipa.p4d <- lipaMoran(p4d, as.p4d = TRUE)
barplot.phylo4d(p4d, bar.col=(carni.lipa$p.value < 0.05) + 1, center = FALSE , scale = FALSE)
barplot.phylo4d(carni.lipa.p4d, bar.col = (carni.lipa$p.value < 0.05) + 1, center = FALSE, scale = FALSE)
dotplot(p4d)
phyloSignal(p4d)
