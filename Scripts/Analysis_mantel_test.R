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


#I prepare here the mean of the seed set of standarized treatments
y_mean_scale <- dcast(Species + Treatment ~ ., value.var = "Scale_seed", fun.aggregate = mean, data = y, na.rm= TRUE)
colnames(y_mean_scale)[3] <- "Scale_seed"
Treatment <- str_split_fixed(as.character(y_mean_scale$Treatment), " ", 2)
Treatment <- Treatment[ , -2]
y_mean_scale <- cbind(y_mean_scale, Treatment)
y_mean_scale <- y_mean_scale[ , -2]

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

#Write csv in the two data folders in order to print 
#write.csv(matrix_scale_effect, "Data/matrix_scale_effect.csv")
#write.csv(matrix_scale_effect, "Rmd/Data/matrix_scale_effect.csv")
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
#Mantel test between its distance and the scaled matrix of effect
mantel(matrix_scale_effect, evo_distance_its)
evo_distance_its_square_root <- sqrt(evo_distance_its)
mantel(matrix_scale_effect, evo_distance_its_square_root)
protest(matrix_scale_effect, evo_distance_its_square_root)

#From here I start working with the traits
traits_all <- read.csv("Data/traits_all.csv", sep=",")
rownames(traits_all) <- rownames(matrix_scale_effect)

#I check Mantel between all the traits and the distance matrix of the effect of heterospecific pollen
#IMPORTANT: P values of Mantel seems to be of one sided test 
#For that, first I standarize the values of the different columns


traits_all <- traits_all[,-c(1,2)]
#For mantel I scale the dat.frame
traits_all_scaled <- scale(traits_all)
#Check if the columns are well scaled mean 0 and sd 1
colMeans(traits_all_scaled)
apply(traits_all_scaled, 2, sd)
#Seems that scaled is correct
#Now I apply Mantel
traits_all_scaled_dist <- dist(traits_all_scaled)
mantel(matrix_scale_effect, traits_all_scaled_dist)
protest(matrix_scale_effect, traits_all_scaled_dist)
#With all the traits r=0.07 and significance 0.375


#Moreover, I try an alternative way to Mantel
traits_all_bioenv <- traits_all[,-c(1,2)]
#Bioenv standarize 
bioenv(matrix_scale_effect,traits_all_bioenv)
#Result r=0.45 being pollen ovule ratio, stigma width and style width the best model


#I check trait by trait first with mantel test

#FirstLY, I start with selfing rate, I extract colum and then create a matrix of distance
traits_all_scaled_self <- traits_all_scaled[,2]
traits_all_scaled_self <- as.data.frame(traits_all_scaled_self)
traits_all_scaled_self <- traits_all_scaled_self
rownames(traits_all_scaled_self) <- rownames(traits_all_scaled_self)
traits_self_dist <- dist(traits_all_scaled_self, diag=T, upper=T)
mantel(matrix_scale_effect, traits_self_dist)
#r=0.033 significance=0.399
prot_self <- protest(matrix_scale_effect, traits_self_dist)
plot(prot_self)
#corr proc rotation=

#Mantel gives very low correlation between selfing rate and the effect distance matrix of seed set
#This surprise me...

#Pollen size
pollen_size <- traits_all_scaled[,3]
pollen_size <- as.data.frame(pollen_size)
pollen_size <- pollen_size
rownames(pollen_size) <- rownames(pollen_size)
pollen_size_dist <- dist(pollen_size, diag=T, upper=T)
mantel(matrix_scale_effect, pollen_size_dist)
protest(matrix_scale_effect, pollen_size_dist)

#Pollen per anther
pollen <- traits_all_scaled[,4]
pollen <- as.data.frame(pollen)
pollen <- pollen
rownames(pollen) <- rownames(pollen)
pollen_dist <- dist(pollen, diag=T, upper=T)
mantel(matrix_scale_effect, pollen_dist)
protest(matrix_scale_effect, pollen_dist)
#Ovules

ovules <- traits_all_scaled[,5]
ovules <- as.data.frame(ovules)
rownames(ovules) <- rownames(ovules)
ovules_dist <- dist(ovules, diag=T, upper=T)
mantel(matrix_scale_effect, ovules_dist)
protest(matrix_scale_effect, ovules_dist)

#Pollen ovule ratio now

p_o_ratio <- traits_all_scaled[,6]
p_o_ratio <- as.data.frame(p_o_ratio)
rownames(p_o_ratio) <- rownames(p_o_ratio)
p_o_ratio_dist <- dist(p_o_ratio, diag=T, upper=T)
mantel(matrix_scale_effect, p_o_ratio_dist)
protest(matrix_scale_effect, p_o_ratio_dist)


#Anthers

anthers <- traits_all_scaled[,7]
anthers <- as.data.frame(anthers)
rownames(anthers) <- rownames(anthers)
anthers_dist <- dist(anthers, diag=T, upper=T)
mantel(matrix_scale_effect, anthers_dist)
protest(matrix_scale_effect, anthers_dist)

#Stigma_area
stigma_area <- traits_all_scaled[,8]
stigma_area <- as.data.frame(stigma_area)
rownames(stigma_area) <- rownames(stigma_area)
stigma_area_dist <- dist(stigma_area, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_area_dist)
protest(matrix_scale_effect, stigma_area_dist)


#Stigma_length
stigma_length <- traits_all_scaled[,9]
stigma_length <- as.data.frame(stigma_length)
rownames(stigma_length) <- rownames(stigma_length)
stigma_length_dist <- dist(stigma_length, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_length_dist)
protest(matrix_scale_effect, stigma_length_dist)

#Stigma surface
stigma_surface <- traits_all_scaled[,10]
stigma_surface <- as.data.frame(stigma_surface)
rownames(stigma_surface) <- rownames(stigma_surface)
stigma_surface_dist <- dist(stigma_surface, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_surface_dist)
protest(matrix_scale_effect, stigma_surface_dist)

#Stigma width
stigma_width <- traits_all_scaled[,11]
stigma_width <- as.data.frame(stigma_width)
stigma_width <- 1- stigma_width
rownames(stigma_width) <- rownames(stigma_width)
stigma_width_dist <- dist(stigma_width, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_width_dist)
protest(matrix_scale_effect, stigma_width_dist)

#Style_length
style_length <- traits_all_scaled[,12]
style_length <- as.data.frame(style_length)
style_length <- 1- style_length
rownames(style_length) <- rownames(style_length)
style_length_dist <- dist(style_length, diag=T, upper=T)
mantel(matrix_scale_effect, style_length_dist)
protest(matrix_scale_effect, style_length_dist)

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


#Just checking how dist works
#dist(traits_all$mean_ovules, diag = T, upper=T)
#Distances are equal at both sides of the matrix
#I´m using the default euclidean distance
# sqrt(sum((x-y)2))





#exampple of plotin protest

data(varespec)
vare.dist <- vegdist(wisconsin(varespec))
library(MASS)  ## isoMDS
mds.null <- isoMDS(vare.dist, tol=1e-7)
mds.alt <- isoMDS(vare.dist, initMDS(vare.dist), maxit=200, tol=1e-7)
vare.proc <- procrustes(mds.alt, mds.null)
vare.proc
summary(vare.proc)
plot(vare.proc)
plot(vare.proc, kind=2)
residuals(vare.proc)

data(dune)
data(dune.env)
adonis(dune ~ Management*A1, data=dune.env, permutations=999)
adonis(dune ~ Management*A1, data=dune.env, permutations=99)

adonis(matrix_scale_effect ~ traits_all$Selfing_rate)

min(matrix_scale_effect)
matrix_scale_effect_positive <- min(matrix_scale_effect)+ matrix_scale_effect
adonis(matrix_scale_effect_positive ~ traits_all$Selfing_rate)

 a <- matrix_scale_effect + min(matrix_scale_effect)
