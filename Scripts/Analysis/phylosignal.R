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

