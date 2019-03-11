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


#Now I´m going to try to perform it with my data
pollen_tree=read.tree("Data/pollen_tree.nwk")
matrix_scale_effect <- readRDS("Data/matrix_scale_effect.Rda")
as.data.frame(matrix_scale_effect)
matrix_scale_effect <- melt(matrix_scale_effect)
colnames(matrix_scale_effect)[3] <- "hp"
