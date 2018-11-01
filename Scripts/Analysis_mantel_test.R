#In this script I´m going to prepare the data to do some first Analysis
#In first place I modify the data to create a matrix
#Then I perform Mantel test between the two matrices (percentage of decrease of seed set and evol. distances)
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
evo_distance_rbcl  <- read.csv("Data/species_matrix_phylogenetic_distance_rbcl.csv", sep=";", stringsAsFactors = F)
evo_distance_its  <- read.csv("Data/its_outgroup_pinus.csv", sep=",", stringsAsFactors = F)
#Reading data.frame of traits. Keep working with it later...
traits <- read.csv("Data/tab.csv", sep="")
traits_all <- read.csv("Data/traits_all.csv", sep=",")
traits_all <- traits_all[,-1]

#Standarize values with function scale. standarize valuei=Xi-mean(x)/sd(x)
#Example
head(soly$seed_set)- mean(soly$seed_set)
soly_cross <- filter(soly, Treatment=="CROSS")
scale(soly_cross$seed_set)
((soly_cross[1,4])-mean(soly_cross$seed_set))/sd(soly_cross$seed_set)
#Now prepare all data sets to be standarize
soly$scale_seed<-scale(soly$seed_set)
some$scale_seed<-scale(some$seed_set)
pein$scale_seed<-scale(pein$Seed.production)
caan$scale_seed<-scale(caan$seed_set)
ersa$scale_seed<-scale(ersa$seed.production)
brra$scale_seed<-scale(brra$Seed.production)
sial$scale_seed<-scale(sial$Seed.production)
brol$scale_seed<-scale(brol$Seed.production)
ippu$scale_seed<-scale(ippu$seed.set)
ipaq$scale_seed<-scale(ipaq$seed_set)


#Deleting column of fruit set for some solanaceae species to make equal number of columns
some<- some[,-4]
soly<- soly[,-4]
caan<- caan[,-4]

#Making all columns numerical ones
soly[3:5] <- lapply(soly[3:5], as.numeric)
some[3:5] <- lapply(some[3:5], as.numeric)
caan[3:5] <- lapply(caan[3:5], as.numeric)
pein[3:5] <- lapply(pein[3:5], as.numeric)
brol[3:5] <- lapply(brol[3:5], as.numeric)
brra[3:5] <- lapply(brra[3:5], as.numeric)
ersa[3:5] <- lapply(ersa[3:5], as.numeric)
sial[3:5] <- lapply(sial[3:5], as.numeric)
ipaq[3:5] <- lapply(ipaq[3:5], as.numeric)
ippu[3:5] <- lapply(ippu[3:5], as.numeric)


#Preparing for loop to clean dataframe and select columns of interest
species_list <- list(soly, some, pein, caan, ersa, brra, sial, brol, ippu, ipaq)
i <- NULL
y <- NULL

for (i in species_list){
colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set", "Scale_seed")
 i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%",
             Treatment!="Cross", Treatment!="Self", Treatment!="Control",Treatment!="Flower Control",
             Treatment!="FC", Treatment!="CROSS", Treatment!="FLOWER CONTROL", Treatment!="control",
             Treatment!="cross", Treatment!="flower control", Treatment!="self", Treatment!="SELF",
             Treatment!="CONTROL")
 i <-i[-grep("100", i$Treatment),] 
  
  y <- rbind(y, i)
}

#Now we have a dataframe with all the species, treatments and seed set and standarized seed set
#First one with seed set mean of Treatments
y_mean <- dcast(Species + Treatment ~ ., value.var = "Seed_set", fun.aggregate = mean, data = y, na.rm= TRUE)
colnames(y_mean)[3] <- "Seed_set"
Treatment <- str_split_fixed(as.character(y_mean$Treatment), " ", 2)
Treatment <- Treatment[ , -2]
y_mean <- cbind(y_mean, Treatment)
y_mean <- y_mean[ , -2]
#Second one with seed set mean of standarized treatments
y_mean_scale <- dcast(Species + Treatment ~ ., value.var = "Scale_seed", fun.aggregate = mean, data = y, na.rm= TRUE)
colnames(y_mean_scale)[3] <- "Scale_seed"
Treatment <- str_split_fixed(as.character(y_mean_scale$Treatment), " ", 2)
Treatment <- Treatment[ , -2]
y_mean_scale <- cbind(y_mean_scale, Treatment)
y_mean_scale <- y_mean_scale[ , -2]

#Now I create two matrices one that has seed set and onother with standarized seed set as mention above
matrix <- tapply(y_mean$Seed_set, y_mean[c("Species", "Treatment")], mean) 
matrix<- matrix[-11,]
#Here I have to make the diagonal the values of the cross


matrix_scale <- tapply(y_mean_scale$Scale_seed, y_mean_scale[c("Species", "Treatment")], mean)
matrix_scale <- matrix_scale[-11,]
#In this one I have to make the diagonal 0 because is standarized

diag(matrix_scale) <- 0
#Fixing this two values for now...
matrix[8,5] <- matrix[7,6]
matrix[10,5] <- matrix[10,6]
matrix_scale[8,5] <- matrix_scale[7,6]
matrix_scale[10,5] <- matrix_scale[10,6]


#Now I prepare the two matrices for cross seed set. One is just the seed set without modification
#The other has the standarized seed set (matrix_cross and matrix_cross_scale respectively)

species_list <- list(soly, some, pein, caan, ersa, brra, sial, brol, ippu, ipaq)
i <- NULL
y <- NULL
for (i in species_list){
  colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set", "Scale_seed")
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
#I edit the matrix manually, values of the diagonal to full row 
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


y_cross_scale_1 <- y[grep("CROSS", y$Treatment), ]
y_cross_scale_2 <- y[grep("cross", y$Treatment), ]
y_cross_scale_3 <- y[grep("Cross", y$Treatment), ]
y_cross_scale_1 <- dcast(Species ~ ., value.var = "Scale_seed", fun.aggregate = mean, data = y_cross_scale_1, na.rm= TRUE)
y_cross_scale_2 <- dcast(Species ~ ., value.var = "Scale_seed", fun.aggregate = mean, data = y_cross_scale_2, na.rm= TRUE)
y_cross_scale_3 <- dcast(Species ~ ., value.var = "Scale_seed", fun.aggregate = mean, data = y_cross_scale_3, na.rm= TRUE)
y_cross_scale <- rbind(y_cross_scale_1, y_cross_scale_2, y_cross_scale_3)
colnames(y_cross_scale)[2] <- "Scale_seed"
y_cross_scale$Non_focal <- y_cross_scale$Species
matrix_cross_scale <- tapply(y_cross_scale$Scale_seed, y_cross_scale[c("Species", "Non_focal")], mean)
#I edit the matrix manually, values of the diagonal to full row 
matrix_cross_scale[1,1:10] <- matrix_cross_scale[1,1]#BROL
matrix_cross_scale[2,1:10] <- matrix_cross_scale[2,2]#BRRA
matrix_cross_scale[3,1:10] <- matrix_cross_scale[3,3]#CAAN
matrix_cross_scale[4,1:10] <- matrix_cross_scale[4,4]#ERSA
matrix_cross_scale[5,1:10] <- matrix_cross_scale[5,5]#IPAQ
matrix_cross_scale[6,1:10] <- matrix_cross_scale[6,6]#IPPU
matrix_cross_scale[7,1:10] <- matrix_cross_scale[7,7]#PEIN
matrix_cross_scale[8,1:10] <- matrix_cross_scale[8,8]#SIAL
matrix_cross_scale[9,1:10] <- matrix_cross_scale[9,9]#SOLY
matrix_cross_scale[10,1:10] <- matrix_cross_scale[10,10]#SOME

#Fix the diagonal of matrix_scale because it should the "maximum value" of the cross
diag(matrix_scale) <- diag(matrix_cross_scale)

#I´m going to substract the two matrices BUT I have negative values 
#So I´m going to fix that adding the minimum value to the two matrices
#New minimum should be 0
a <- min(matrix_scale)
#I save this value
#max(matrix_scale)
matrix_scale <- matrix_scale + abs(a)
matrix_cross_scale <- matrix_cross_scale + abs(a)

#Now the negative values are fixed. Time to make the matrix of effect respect the cross with the standarized
#The normal values of seed set are going to be on a side for a while
matrix_scale_effect <- matrix_cross_scale-matrix_scale

#Now edit the evolutionary distances
#They are the distances calculated with MEGA 7 (pairwise distances)
evo_distance_rbcl <- evo_distance_rbcl[ , -2]
evo_distance_rbcl <- as.matrix(evo_distance_rbcl)

rownames(evo_distance_rbcl) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
evo_distance_rbcl <- evo_distance_rbcl[, -1]
colnames(evo_distance_rbcl) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
diag(evo_distance_rbcl) <- 0

makeSymm <- function(evo_distance_rbcl) {
  evo_distance_rbcl[upper.tri(evo_distance_rbcl)] <- t(evo_distance_rbcl)[upper.tri(evo_distance_rbcl)]
  return(evo_distance_rbcl)
}
evo_distance_rbcl <- makeSymm(evo_distance_rbcl)
diag(evo_distance_rbcl) <- NA
evo_distance_rbcl <- evo_distance_rbcl[order(rownames(evo_distance_rbcl)), order(colnames(evo_distance_rbcl))] 
evo_distance_rbcl <- mapply(evo_distance_rbcl, FUN=as.numeric)
evo_distance_rbcl <- matrix(data=evo_distance_rbcl, ncol=10, nrow=10)
rownames(evo_distance_rbcl) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
colnames(evo_distance_rbcl) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
evo_distance_rbcl <- evo_distance_rbcl[order(rownames(evo_distance_rbcl)), order(colnames(evo_distance_rbcl))] 
diag(evo_distance_rbcl) <- 0
#Mantel test with percentage 100-matrix and normal percentage
mantel.test(matrix_scale_effect, evo_distance_rbcl, graph = TRUE)
mantel(matrix_scale_effect, evo_distance_rbcl)
mantel(matrix_scale_effect, sqrt(evo_distance_rbcl))


#Now with the ITS tree. 
#First I have to fix a bit the data.frame and convert it to a matrix
#load csv of evolutionary distance of ITS, not in matrix format
evo_distance_its <- evo_distance_its[-1,-c(1,2)]
rownames(evo_distance_its) <- c("BROL", "ERSA", "BRRA", "SIAL", "IPPU", "IPAQ", "PEIN", "CAAN", "SOME", "SOLY")
colnames(evo_distance_its) <- c("BROL", "ERSA", "BRRA", "SIAL", "IPPU", "IPAQ", "PEIN", "CAAN", "SOME", "SOLY")
evo_distance_its <- makeSymm(evo_distance_its)
diag(evo_distance_its) <- 0
evo_distance_its <- evo_distance_its[order(rownames(evo_distance_its)), order(colnames(evo_distance_its))] 

mantel.test(matrix_effect_original, evo_distance_its, graph = TRUE)
mantel.test(matrix_effect_original, (evo_distance_its^2), graph = TRUE)

mantel(matrix_scale_effect, evo_distance_its)
mantel(matrix_scale_effect, evo_distance_its_square_root)
evo_distance_its_square_root <- sqrt(evo_distance_its)



#From here I start working with the traits
traits_all <- read.csv("Data/traits_all.csv", sep=",")
rownames(traits_all) <- rownames(matrix_scale_effect)
#I check trait by trait first with mantel test
#First with the selfing rate, I extract colum and then create a matrix distance
traits_self <- traits_all[,4]
traits_self <- as.data.frame(traits_self)
traits_self <- 1- traits_self
rownames(traits_self) <- rownames(matrix_scale_effect)
traits_self_dist <- dist(traits_self, diag=T, upper=T, method = "manhattan")
mantel(matrix_scale_effect, traits_self_dist)
bioenv(matrix_scale_effect~as.matrix(traits_all$mean_pollen_anther))

matrix_scale_effect=matrix_scale_effect+abs(min(matrix_scale_effect))


str(traits_self)

#Pollen ovule ratio now

traits_ratio <- traits_all[,8]
traits_ratio <- as.data.frame(traits_ratio)
rownames(traits_ratio) <- rownames(matrix_scale_effect)
traits_ratio_dist <- dist(traits_ratio, diag=T, upper=T)
mantel(matrix_scale_effect, traits_ratio_dist)

#Ovules

traits_ovules <- traits_all[,6]
traits_ovules <- as.data.frame(traits_ovules)
rownames(traits_ovules) <- rownames(matrix_scale_effect)
traits_ovules_dist <- dist(traits_ovules, diag=T, upper=T)
mantel(matrix_scale_effect, traits_ovules_dist)

#Stigmatic area

traits_stigma_area <- traits_all[,9]
traits_stigma_area <- as.data.frame(traits_stigma_area)
rownames(traits_stigma_area) <- rownames(matrix_scale_effect)
traits_stigma_area_dist <- dist(traits_stigma_area, diag=T, upper=T)
mantel(matrix_scale_effect, traits_stigma_area_dist)

#Pollen

traits_pollen <- traits_all[,5]
traits_pollen <- as.data.frame(traits_pollen)
rownames(traits_pollen) <- rownames(matrix_scale_effect)
traits_pollen_dist <- dist(traits_pollen, diag=T, upper=T)
mantel(matrix_scale_effect, traits_pollen_dist)

#Style length

traits_style_length <- traits_all[,13]
traits_style_length <- as.data.frame(traits_style_length)
traits_style_length <- 1- traits_style_length
rownames(traits_style_length) <- rownames(matrix_scale_effect)
traits_style_length_dist <- dist(traits_style_length, diag=T, upper=T)
mantel(matrix_scale_effect, traits_style_length_dist)


#Ovary length

traits_style_length <- traits_all[,13]
traits_style_length <- as.data.frame(traits_style_length)
traits_style_length <- 1- traits_style_length
rownames(traits_style_length) <- rownames(matrix_scale_effect)
traits_style_length_dist <- dist(traits_style_length, diag=T, upper=T)
mantel(matrix_scale_effect, traits_style_dist)


min(matrix_scale_effect)
matrix_scale_effect=matrix_scale_effect+abs(min(matrix_scale_effect))

bioenv(matrix_scale_effect~traits_all$stigma_area, trace=T)

#ejemplo

# The method is very slow for large number of possible subsets.
# Therefore only 6 variables in this example.
data(varespec)
varespec[]
data(varechem)
sol <- bioenv(wisconsin(varespec) ~ log(N) + P + K + Ca + pH + Al, varechem)
sol
summary(sol)










#Here I´m doing Mantel test with all the traits but without scaling!!!!
traits_all_dist <- dist(traits_all, diag=T, upper=T)
mantel(matrix_scale_effect, traits_all_dist)










traits_all_self <- dist(traits_all$Selfing_rate, diag=T, upper=T)
traits_all_self <- as.matrix(traits_all_self)
diag(traits_all_self) <- 1
rownames(traits_all_self) <- rownames(evo_distance_its)
colnames(traits_all_self) <- rownames(evo_distance_its)
mantel(matrix_scale_effect, traits_all_self)


traits_all_ovules <- dist(traits_all$mean_ovules, diag=T, upper=T)
traits_all_ovules <- as.matrix(traits_all_ovules)
diag(traits_all_ovules) <- 1
rownames(traits_all_ovules) <- rownames(evo_distance_its)
colnames(traits_all_ovules) <- rownames(evo_distance_its)
mantel(matrix_scale_effect, traits_all_ovules)


traits_all <- read.csv("Data/traits_all.csv", sep=",")
#Now just with selfing rate
traits_all <- traits_all[,-c(1:3)]
traits_selfing <- traits_all[,1]
traits_selfing <- scale(traits_selfing)
rownames(traits_selfing) <- rownames(evo_distance_its)
traits_selfing <- dist(traits_selfing, diag=T, upper=T)
traits_selfing <- as.matrix(traits_selfing)

traits_selfing <- 1- traits_selfing
mantel(matrix_scale_effect, traits_selfing)


traits_selfing_1 = traits_selfing[, rep(1, each=10)]
rownames(traits_selfing_1) <- rownames(evo_distance_its)
colnames(traits_selfing_1) <- rownames(evo_distance_its)
diag(traits_selfing_1) <- 1
mantel(matrix_scale_effect, traits_selfing_1)
traits_selfing_1_scale <- scale(traits_selfing_1)
mantel(matrix_scale_effect, traits_selfing_1)

traits_selfing_2 <- dist(traits_selfing_1)
mantel(matrix_scale_effect, traits_selfing_1_scale)



traits_selfing_dist <- dist(traits_selfing, diag=T, upper=T)
traits_selfing_dist <- as.matrix(traits_selfing_dist)
rownames(traits_selfing_dist) <- rownames(evo_distance_its)
colnames(traits_selfing_dist) <- rownames(evo_distance_its)

mantel(matrix_scale_effect, traits_selfing_dist)







rownames(traits_all) <- rownames(matrix_scale_effect)
traits_all <- traits_all[,-1]

traits_all_scale <- traits_all

#Scale each trait separately
traits_all_scale$stigma_type <- scale(traits_all$stigma_type)
traits_all_scale$Selfing_rate <-scale(traits_all$Selfing_rate)
traits_all_scale$pollen_size <-scale(traits_all$pollen_size)
traits_all_scale$mean_pollen_anther <-scale(traits_all$mean_pollen_anther)
traits_all_scale$mean_ovules <-scale(traits_all$mean_ovules)
traits_all_scale$pollen_ovule_ratio <-scale(traits_all$pollen_ovule_ratio)
traits_all_scale$anthers <-scale(traits_all$anthers)
traits_all_scale$stigma_area <-scale(traits_all$stigma_area)
traits_all_scale$stigma_length <-scale(traits_all$stigma_length)
traits_all_scale$stigma_surface <-scale(traits_all$stigma_surface)
traits_all_scale$stigma_width <-scale(traits_all$stigma_width)
traits_all_scale$style_length <-scale(traits_all$style_length)
traits_all_scale$style_width <-scale(traits_all$style_width)
traits_all_scale$ovary_width <-scale(traits_all$ovary_width)
traits_all_scale$ovary_length <-scale(traits_all$ovary_length)

traits_all_scale_dist <- dist(traits_all_scale, diag=T, upper=T)




mantel(matrix_scale_effect, traits_all_scale_dist)








#Checking waysto plot distance matrices
#Way 1
library(qgraph)
traits_all_dist_inverse <- 1/traits_all_dist
jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
qgraph(traits_all_dist_inverse, layout='spring', vsize=3)
dev.off()

#Way 2
traits_all_dist_low <- dist(traits_all, diag=T)
traits_all_dist_inverse_low <- 1/traits_all_dist_low

table.dist(traits_all_dist_inverse_low, clabel = 0.8, csize = 1, grid = TRUE, 
           labels=attr(traits_all_dist_inverse, "Labels"))














#