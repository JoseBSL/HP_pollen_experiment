#In this script I´m going to prepare the data to do some first Analysis

#In first place I modify the data to create a matrix
#Then I perform Mantel test between the two matrices (percentage of decrease of seed set and evol. distances)

#Load seed set data for 10 species
soly  <- read.csv("Data/species_seed_set/soly_seed_set.csv", sep=";", stringsAsFactors = F)
some  <- read.csv("Data/species_seed_set/some_seed_set.csv", sep=";", stringsAsFactors = F)
pein  <- read.csv("Data/species_seed_set/pein_seed_set.csv", sep=";", stringsAsFactors = F)
caan  <- read.csv("Data/species_seed_set/caan_seed_set.csv", sep=";", stringsAsFactors = F)
ersa  <- read.csv("Data/species_seed_set/ersa_seed_set.csv", sep=";", stringsAsFactors = F)
brra  <- read.csv("Data/species_seed_set/brra_seed_set.csv", sep=";", stringsAsFactors = F)
sial  <- read.csv("Data/species_seed_set/sial_seed_set.csv", sep=";", stringsAsFactors = F)
brol  <- read.csv("Data/species_seed_set/brol_seed_set.csv", sep=";", stringsAsFactors = F)
ippu  <- read.csv("Data/species_seed_set/ippu_seed_set.csv", sep=";", stringsAsFactors = F)
ipaq  <- read.csv("Data/species_seed_set/ipaq_seed_set.csv", sep=";", stringsAsFactors = F)

#load matrix of evolutionary distances
evo_distance  <- read.csv("Data/species_matrix_phylogenetic_distance_rbcl.csv", sep=";", stringsAsFactors = F)
#Reading data.frame of traits. Keep working with it later...
traits <- read.csv("Data/tab.csv", sep="")

#load libraries
library(dplyr)
library(reshape2)
library(stringr)
library(ape)
library(ade4)
library(vegan)
library(permute)
library(lattice)
library(distances)
library(pdist)
#soly[-grep("100%", soly$Treatment), ]
#To create the matrix of effect we are going to estimate the net effect as 
#Cross-(average of 50% treatments)
#I´m going to try to do it in a for loop


#Deleting column of fruit set for some solanaceae species
some<- some[,-4]
soly<- soly[,-4]
caan<- caan[,-4]
species_list <- list(soly, some, pein, caan, ersa, brra, sial, brol, ippu, ipaq)
i <- NULL
y <- NULL

for (i in species_list){
colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set")
 i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%",
             Treatment!="Cross", Treatment!="Self", Treatment!="Control",Treatment!="Flower Control",
             Treatment!="FC", Treatment!="CROSS", Treatment!="FLOWER CONTROL", Treatment!="control",
             Treatment!="cross", Treatment!="flower control", Treatment!="self", Treatment!="SELF",
             Treatment!="CONTROL")
 i <-i[-grep("100", i$Treatment),] 
  
  y <- rbind(y, i)
}

#Check mean of SOME, there is one extra don´t know why...
mean(y[y$Species=="SOME", "Seed_set"])
y_mean <- dcast(Species + Treatment ~ ., value.var = "Seed_set", fun.aggregate = mean, data = y, na.rm= TRUE)
colnames(y_mean)[3] <- "Seed_set"
Treatment <- str_split_fixed(as.character(y_mean$Treatment), " ", 2)
Treatment <- Treatment[ , -2]
y_mean <- cbind(y_mean, Treatment)
y_mean <- y_mean[ , -2]

#str(y_mean)
#y_mean$Treatment <- as.character(y_mean$Treatment)
#y_mean$Species <- as.character(y_mean$Species)
#y_mean$Seed_set <- as.numeric(y_mean$Seed_set)
matrix <- tapply(y_mean$Seed_set, y_mean[c("Species", "Treatment")], mean)
matrix<- matrix[-11,]

#Fixing this value, don´t know why it doesn´t read it properly. I think it has an space or a weird format
matrix[10,5] <- 0
#SIAL-> It seems that I didn´t do this one

#Now I create the matrix with the cross value


species_list <- list(soly, some, pein, caan, ersa, brra, sial, brol, ippu, ipaq)
i <- NULL
y <- NULL
for (i in species_list){
  colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set")
  i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%")
  i <-i[-grep("100", i$Treatment),] 
  
  y <- rbind(y, i)
}
y_cross_1 <- y[grep("CROSS", y$Treatment), ]
y_cross_2 <- y[grep("cross", y$Treatment), ]
y_cross_3 <- y[grep("Cross", y$Treatment), ]
y_cross_1 <- dcast(Species ~ ., value.var = "Seed_set", fun.aggregate = mean, data = y_cross_1, na.rm= TRUE)
y_cross_2 <- dcast(Species ~ ., value.var = "Seed_set", fun.aggregate = mean, data = y_cross_2, na.rm= TRUE)
y_cross_3 <- dcast(Species ~ ., value.var = "Seed_set", fun.aggregate = mean, data = y_cross_3, na.rm= TRUE)
y_cross <- rbind(y_cross_1, y_cross_2, y_cross_3)
colnames(y_cross)[2] <- "Seed_set_cross"
y_cross$Non_focal <- y_cross$Species
matrix_cross <- tapply(y_cross$Seed_set_cross, y_cross[c("Species", "Non_focal")], mean)

#I edit the matrix manually, values of the diagonal to full row and the diagonal NA
matrix_cross[1,1:10] <- matrix_cross[1,1]#BROL
matrix_cross[2,1:10] <- matrix_cross[2,2]#BRRA
matrix_cross[3,1:10] <- matrix_cross[3,3]#CAAN
matrix_cross[4,1:10] <- matrix_cross[4,4]#ERSA
matrix_cross[5,1:10] <- matrix_cross[5,5]#IPAQ
matrix_cross[6,1:10] <- matrix_cross[6,6]#IPPU
matrix_cross[7,1:10] <- matrix_cross[7,7]#PEIN
matrix_cross[8,1:10] <- matrix_cross[8,8]#SIAL
matrix_cross[9,1:10] <- matrix_cross[9,9]#SOLY
matrix_cross[10,1:10] <- matrix_cross[10,10]#SOME
diag(matrix_cross) <- NA

#The proxy of effect is the decrease in seed set 
matrix_effect <- matrix/matrix_cross * 100
matrix_effect_original <- matrix_effect
matrix_effect[matrix_effect[,]>100] <-100
#To make it more intuitive, 100% 0 percent of seed set
matrix_effect <- 100-matrix_effect

#Now edit the evolutionary distances

evo_distance <- evo_distance[ , -2]
evo_distance <- as.matrix(evo_distance)

rownames(evo_distance) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
evo_distance <- evo_distance[, -1]
colnames(evo_distance) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
diag(evo_distance) <- 0

makeSymm <- function(evo_distance) {
  evo_distance[upper.tri(evo_distance)] <- t(evo_distance)[upper.tri(evo_distance)]
  return(evo_distance)
}
evo_distance <- makeSymm(evo_distance)
diag(evo_distance) <- NA
evo_distance <- evo_distance[order(rownames(evo_distance)), order(colnames(evo_distance))] 
evo_distance <- mapply(evo_distance, FUN=as.numeric)
evo_distance <- matrix(data=evo_distance, ncol=10, nrow=10)
rownames(evo_distance) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
colnames(evo_distance) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
evo_distance <- evo_distance[order(rownames(evo_distance)), order(colnames(evo_distance))] 
diag(evo_distance) <- 0
diag(matrix_effect) <- 0


#Fixing value of SIAL-IPAQ
matrix_effect[8,5]<-0
matrix_effect_original[8,5]<-0
#Mantel test with percentage 100-matrix and normal percentage
mantel.test(matrix_effect, evo_distance, graph = TRUE)
diag(matrix_effect_original) <- 100
mantel.test(matrix_effect_original, evo_distance, graph = TRUE)


#Now with the ITS tree. 
#First I have to fix a bit the data.frame and convert it to a matrix
#load csv of evolutionary distance of ITS, not in matrix format
evo_distance_its  <- read.csv("Data/its_outgroup_pinus.csv", sep=",", stringsAsFactors = F)
evo_distance_its <- evo_distance_its[-1,-c(1,2)]
rownames(evo_distance_its) <- c("BROL", "ERSA", "BRRA", "SIAL", "IPPU", "IPAQ", "PEIN", "CAAN", "SOME", "SOLY")
colnames(evo_distance_its) <- c("BROL", "ERSA", "BRRA", "SIAL", "IPPU", "IPAQ", "PEIN", "CAAN", "SOME", "SOLY")
evo_distance_its <- makeSymm(evo_distance_its)
diag(evo_distance_its) <- 0
evo_distance_its <- evo_distance_its[order(rownames(evo_distance_its)), order(colnames(evo_distance_its))] 
mantel.test(matrix_effect_original, evo_distance_its, graph = TRUE)


#Now I´m going to prepare the traits to check them
#Here I perform mantel test between pollen ovule ratios and seed percentage of seed set reduction
traits <- read.csv("Data/tab.csv", sep="")
traits_1 <- traits[,13]
traits_1 <- data_frame(traits_1)
rownames(traits_1) <- traits[,3]
colnames(traits_1) <- "Pollen ovule raio"
d <- vegdist(traits_1, method="euclidean")
d <- as.matrix(d)
d <- d[order(rownames(d)), order(colnames(d))] 
rownames(d) <- rownames(evo_distance_its)
colnames(d) <- colnames(evo_distance_its)
matrix_effect_original[matrix_effect_original[,]>100] <-100
diag(d)<- 0
diag(matrix_effect_original)<- 100
mantel.test(d, matrix_effect_original)


#Mantel between seed set percentage change and pollen, ovules, pollen size
traits <- read.csv("Data/tab.csv", sep="")
traits_1 <- traits[,c(8,9,11)]
rownames(traits_1) <- traits[,3]
d <- vegdist(traits_1, method="euclidean")
d <- as.matrix(d)
d <- d[order(rownames(d)), order(colnames(d))] 
rownames(d) <- rownames(evo_distance_its)
colnames(d) <- colnames(evo_distance_its)
matrix_effect_original[matrix_effect_original[,]>100] <-100
diag(d)<- 0
diag(matrix_effect_original)<- 100
mantel.test(d, matrix_effect_original)

#Mantel between seed set percentage change and ovules
traits <- read.csv("Data/tab.csv", sep="")
traits_1 <- traits[,11]
traits_1 <- data_frame(traits_1)
rownames(traits_1) <- traits[,3]
d <- vegdist(traits_1, method="euclidean")
d <- as.matrix(d)
d <- d[order(rownames(d)), order(colnames(d))] 
rownames(d) <- rownames(evo_distance_its)
colnames(d) <- colnames(evo_distance_its)
matrix_effect_original[matrix_effect_original[,]>100] <-100
diag(d)<- 0
diag(matrix_effect_original)<- 100
mantel.test(d, matrix_effect_original)


#Now I´m going to prepare the differences of seed set in a different way
#Instead of doing percentages respect the cross. I´m going to check with euclidean distance 
#of the difference cross-(mean of treatments)

View(y_mean)
View(y_cross)
y_mean_cross <- merge(y_mean, y_cross, by="Species")

y_mean_cross$result <- (y_mean_cross$Seed_set_cross)-(y_mean_cross$Seed_set)
y_mean_cross[y_mean_cross[,]<0] <-0
y_mean_cross <- tapply(y_mean_cross$result, y_mean_cross[c("Species", "Treatment")], mean)
diag(y_mean_cross) <-0
#The diagonal should be the values of the cross
#Fix two NA´S
y_mean_cross[10,5]<- 0
y_mean_cross[8,5]<- y_mean_cross[8,6]
y_mean_cross[1,1] <- y_cross[7,2]
y_mean_cross[2,2] <- y_cross[8,2]
y_mean_cross[3,3] <- y_cross[1,2]
y_mean_cross[4,4] <- y_cross[9,2]
y_mean_cross[5,5] <- y_cross[5,2]
y_mean_cross[6,6] <- y_cross[6,2]
y_mean_cross[7,7] <- y_cross[2,2]
y_mean_cross[8,8] <- y_cross[10,2]
y_mean_cross[9,9] <- y_cross[3,2]
y_mean_cross[10,10] <- y_cross[4,2]

pdist <- pdist(y_mean_cross)
pdist <- as.matrix(y_mean_cross)
dist <- dist(y_mean_cross)
dist <- as.matrix(dist)
#Same result calculating matrix distances differently
mantel.test(pdist,d)
mantel.test(dist,d)
mantel(dist,d)
#Now I´m going to try to create binary values for incompatibility and check with mantel
#Another option is to adrees the diffent levels of incompatibility with the self pollination that I made
traits <- read.csv("Data/tab.csv", sep="")
incompatibility <- traits[,c(3,6)]
incompatibility$binary <- c(0,1,1,1,0,0,0,0,1,1)
incompatibility_mod <- incompatibility[,-c(1,2)]
incompatibility_mod <- data.frame(incompatibility_mod)
rownames(incompatibility_mod) <- incompatibility[,1]
m <- vegdist(incompatibility_mod, method="euclidean")
m <- as.matrix(m)
m <- m[order(rownames(m)), order(colnames(m))] 
rownames(m) <- rownames(evo_distance_its)
colnames(m) <- colnames(evo_distance_its)

mantel.test(pdist,m)
mantel.test(dist,m)
mantel(dist,m)
#No correlation with binary
mantel(evo_distance,m)
