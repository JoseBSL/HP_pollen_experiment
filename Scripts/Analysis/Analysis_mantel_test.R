#In this script I´m going to prepare the data to do some first Analysis
#In first place I modify the data to create a matrix
#Then I perform some analyses to compare matrices 
#1)Between mean scaled Hp effect - mean scaled hand cross pollination ~ evolutive distance
#2)Between mean scaled Hp effect - mean scaled hand cross pollination ~ traits
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
library(plotrix)
library(dplyr)

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
#Example of how the function scale works
soly_cross <- filter(soly, Treatment=="CROSS")
scale(soly_cross$seed_set)
((soly_cross[1,5])-mean(soly_cross$seed_set))/sd(soly_cross$seed_set)
#Scale seed set for 10 species
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


#Deleting column of fruit (just 3 spp had them)
some<- some[,-4]
soly<- soly[,-4]
caan<- caan[,-4]

#Making all columns numerical ones. If not the loop doesn´t run
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


#save.image("seed_set&scaled_seed_set.RData")


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

#Now we have a dataframe "y" with the scaled seed set for all the HP treatments


#Now calculate mean seed set of Hp treatments
y_mean_scale <- dcast(Species + Treatment ~ ., value.var = "Scale_seed", fun.aggregate = mean, data = y, na.rm= TRUE)
#I save these two data.frame "y" and "y_mean_sacale" for analysis in other script

write.csv(y,"Data/y.csv")
write.csv(y_mean_scale, "Data/y_mean_scale.csv")


colnames(y_mean_scale)[3] <- "Scale_seed"
Treatment <- str_split_fixed(as.character(y_mean_scale$Treatment), " ", 2)
Treatment <- Treatment[ , -2]
y_mean_scale <- cbind(y_mean_scale, Treatment)
y_mean_scale <- y_mean_scale[ , -2] 
#Renaming SOME seems to have an space 
y_mean_scale[81:89,1] <- "SOME"


#save.image("seed_set&scaled_seed_set.RData")

#Almost there, dataframe with mean scaled seed set per treatment
#I convert it now to a matrix, and from now operate with matrices
matrix_scale <- tapply(y_mean_scale$Scale_seed, y_mean_scale[c("Species", "Treatment")], mean)


#There is an NA of Treatment that I did´t perform. I give same value of its conspecific
#Criteria for doing this: Both have the biggest pollen
#For the rest of treatments both effects are very similar
matrix_scale[8,5] <- matrix_scale[7,6]
#Meanwhile the diagonal is still NA. Fix that after I prepare the "cross"


# Preparing data frame of the scaled hand cross pollination
species_list <- list(soly, some, pein, caan, ersa, brra, sial, brol, ippu, ipaq)
i <- NULL
y <- NULL
for (i in species_list){
  colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set", "Scale_seed")
  i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%")
  i <-i[-grep("100", i$Treatment),] 
  
  y <- rbind(y, i)
}

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

#Creating matrix of effect (mean cross spp x1-mean HP effect spp x1)
matrix_scale_effect <- matrix_cross_scale-matrix_scale
matrix_scale_reverted <- matrix_scale-matrix_cross_scale

#We set our lower bound to 0
#Maximum theoretical value is our cross, "no treatments could be greater than it"
matrix_scale_effect[matrix_scale_effect<0]<- 0

#saveRDS(matrix_scale_effect, "Manuscript_draft/Data/matrix_scale_effect.Rda")
#saveRDS(matrix_scale_effect, "Data/matrix_scale_effect.Rda")

#There are some values that the effect is greater than the cross
#Maybe fix that?


#
##
###
###### PART 1   MATRIX EFFECT~EVOLUTIVE DISTANCES
###
##
#



#Save matrix of effect csv 
#write.csv(matrix_scale_effect, "Data/matrix_scale_effect.csv")
#write.csv(matrix_scale_effect, "Rmd/Data/matrix_scale_effect.csv")
#Now edit the evolutionary distances
#They are the distances calculated with MEGA 7 (pairwise distances)
#Two different markers for distances RBCL and ITS
evo_distance_rbcl <- evo_distance_rbcl[ , -2]
#Transformto matrix
evo_distance_rbcl <- as.matrix(evo_distance_rbcl)
#same rownames of the matrix of effect
rownames(evo_distance_rbcl) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
evo_distance_rbcl <- evo_distance_rbcl[, -1]
#same colnames of the matrix of effect
colnames(evo_distance_rbcl) <- c("SIAL", "ERSA", "BROL", "BRRA", "IPPU", "IPAQ", "PEIN", "CAAN", "SOLY", "SOME")
diag(evo_distance_rbcl) <- 0
#Because they are distances I make symmetrical the upper part of the matrix
makeSymm <- function(evo_distance_rbcl) {
  evo_distance_rbcl[upper.tri(evo_distance_rbcl)] <- t(evo_distance_rbcl)[upper.tri(evo_distance_rbcl)]
  return(evo_distance_rbcl)
}
evo_distance_rbcl <- makeSymm(evo_distance_rbcl)
#Order columns alphabetically
evo_distance_rbcl <- evo_distance_rbcl[order(rownames(evo_distance_rbcl)), order(colnames(evo_distance_rbcl))] 
#Changing from characters to numerical
evo_distance_rbcl <- mapply(evo_distance_rbcl, FUN=as.numeric)
evo_distance_rbcl <- matrix(data=evo_distance_rbcl, ncol=10, nrow=10)
rownames(evo_distance_rbcl) <- rownames(matrix_scale_effect)
colnames(evo_distance_rbcl) <- rownames(matrix_scale_effect)
evo_distance_rbcl <- evo_distance_rbcl[order(rownames(evo_distance_rbcl)), order(colnames(evo_distance_rbcl))] 


#Now I compare the matrix of scaled seed set (Cross-HP) with the evolutive distances
# There are different ways of doing Mantel on R
#This seems the more complete one so far. Mantel.test doesn´t give all info...
mantel(matrix_scale_effect, evo_distance_rbcl)
#significance=0.024, r=0.29
#Now with the square root of the evolutive distances
#Based on Letten & Cornwell 2014
mantel(matrix_scale_effect, sqrt(evo_distance_rbcl))
#significance=0.017, r=0.32
#When we make our negative values to 0 we reduce a bit the r here
#How do I interpret this?
# Heterospecific pollen effect is significantly correlated with evolutive distance
#Could I say the greater the evolutive distance the smaller the effect?
#Why I say this: Cross-Hp is our proxy,
#if it is a high number it means that the effect was small
p <- mantel.correlog(matrix_scale_effect, sqrt(evo_distance_rbcl))
plot(p)

#Now I perform procrustes test (similar to mantel)
#Peres-Neto & Jackson, 2000 describes the advantages of Procrustes over Mantel

a <-  vegdist(wisconsin(matrix_scale_effect))
prueba_a <- monoMDS(matrix_scale_effect, y = cmdscale(a))
prueba_b <- monoMDS(evo_distance_rbcl)
protest(prueba_a, prueba_b)
protest(matrix_scale_effect, sqrt(evo_distance_rbcl))
plot(protest(matrix_scale_effect, sqrt(evo_distance_rbcl)))
#significance 0.594, correlation=0.47

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
#significance=0.035 ,r=0.2633
#Now with the square root of the evolutive distances
#Based on Letten & Cornwell 2014
evo_distance_its_square_root <- sqrt(evo_distance_its)
mantel(matrix_scale_effect, evo_distance_its_square_root)
mantel(matrix_scale_effect, evo_distance_its_square_root, permutations = 10000)

#significance=0.028, r=0.2764
p <- mantel.correlog(matrix_scale_effect, evo_distance_its_square_root) 
plot(p)
protest(matrix_scale_effect, evo_distance_its_square_root)
#significance=0.765, procustes corr=0.5259


#load Effect sizes
matrix_effect_size <- readRDS("Data/matrix_effect_size.RData")
mantel(matrix_effect_size, sqrt(evo_distance_its))
mantel(abs(matrix_effect_size), sqrt(evo_distance_rbcl))
mantel(abs(matrix_effect_size), sqrt(evo_distance_its))

mantel(abs(matrix_scale_reverted), sqrt(evo_distance_rbcl))

#
##
###
###### PART 2   MATRIX EFFECT~TRAITS
###
##
#


#From here I start working with the traits
traits_all <- read.csv("Data/traits_all.csv", sep=",")
rownames(traits_all) <- rownames(matrix_scale_effect)
si_index <- readRDS("Data/si_index.RData")
traits_all$si_index <- si_index

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
mantel(matrix_effect_size, dist(traits_all))
mantel(abs(matrix_effect_size), dist(traits_all))
mantel(abs(matrix_scale_reverted), dist(traits_all))

#significance=0.296, r=0.09
protest(matrix_scale_effect, traits_all_scaled_dist)
protest(matrix_effect_size,  dist(traits_all))

#significance=0.946, procustes correlation=0.5968
protest(matrix_scale_effect, traits_all_scaled)

#Bioenv, alternative way, similar to Mantel and procustes
#BUT it finds the variables that are  more relevant to our model
#It scale the variables, so it doesn´t matter if they are scaled or not
#We obtain the same result, I check below
traits_all_bioenv <- traits_all
traits_all_bioenv_scaled <- scale(traits_all_bioenv)
bioenv(matrix_scale_effect,traits_all_bioenv)
bioenv(matrix_scale_effect,traits_all_bioenv_scaled)
#bioenv(matrix_scale_effect,traits_all_bioenv_scaled[,-c(6,12,16)])



bio_result <- bioenv(matrix_scale_effect,traits_all_bioenv_scaled)
save(bio_result, file="Manuscript_draft/bio_result.RData")
bio_result[3]
#Result r=0.3711462 being pollen ovule ratio, stigma width and style width the best model
adonis(matrix_scale_effect ~. ,data=traits_all)
traits_all_scaled=as.data.frame(traits_all_scaled)
#Adonis analogous to Manova. Don´t going to pay attention to it for the moment 
#Just seing how it goes and the output
#Don´t know how appropiate it is...
adonis(formula=matrix_scale_effect ~. ,data=traits_all_scaled)
bioenv(matrix_scale_effect, traits_all[,-1])
bioenv(matrix_scale_effect, traits_all[,-1])

#Start trait by trait

#0)si_index
traits_all_scaled_stigma <- traits_all_scaled[,1]
traits_all_scaled_stigma <- as.data.frame(traits_all_scaled_stigma)
traits_all_scaled_stigma <- traits_all_scaled_stigma
rownames(traits_all_scaled_stigma) <- rownames(traits_all_scaled_stigma)
traits_stigma_dist <- dist(traits_all_scaled_stigma, diag=T, upper=T)

traits_si_index_dist <- dist(traits_all$si_index, diag=T, upper=T)
mantel(matrix_scale_effect, traits_si_index_dist)
mantel(matrix_effect_size, traits_si_index_dist)
mantel(abs(matrix_effect_size), traits_si_index_dist)
mantel(abs(matrix_scale_reverted), traits_si_index_dist)

#significance=0.016, r=0.27
protest(matrix_scale_effect, traits_si_index_dist)
#significance=0.373, procustes correlation=0.3208
bioenv(matrix_scale_effect~traits_all$si_index, method="pearson", trace=T)
#corr=-0.16
adonis(formula=matrix_scale_effect ~si_index ,data=traits_all)


#1)Stigma type
traits_all_scaled_stigma <- traits_all_scaled[,1]
traits_all_scaled_stigma <- as.data.frame(traits_all_scaled_stigma)
traits_all_scaled_stigma <- traits_all_scaled_stigma
rownames(traits_all_scaled_stigma) <- rownames(traits_all_scaled_stigma)
traits_stigma_dist <- dist(traits_all_scaled_stigma, diag=T, upper=T)
mantel(matrix_scale_effect, traits_stigma_dist)
mantel(matrix_effect_size, traits_stigma_dist)
mantel(abs(matrix_effect_size), traits_stigma_dist)
mantel(abs(matrix_scale_reverted), traits_stigma_dist)

#significance=0.016, r=0.27
protest(matrix_scale_effect, traits_stigma_dist)
#significance=0.373, procustes correlation=0.3208
bioenv(matrix_scale_effect~traits_all$stigma_type, method="pearson", trace=T)
#corr=-0.16
adonis(formula=matrix_scale_effect ~stigma_type ,data=traits_all)

#2)Selfing rate
traits_all_scaled_self <- traits_all_scaled[,2]
traits_all_scaled_self <- as.data.frame(traits_all_scaled_self)
traits_all_scaled_self <- traits_all_scaled_self
rownames(traits_all_scaled_self) <- rownames(traits_all_scaled_self)
traits_self_dist <- dist(traits_all_scaled_self, diag=T, upper=T)

mantel(matrix_scale_effect, traits_self_dist)
mantel(matrix_effect_size, traits_self_dist)
mantel(abs(matrix_effect_size), traits_self_dist)
mantel(abs(matrix_scale_reverted), traits_self_dist)

#significance=0.424, r=0.03015
protest(matrix_scale_effect, traits_self_dist)
#significance=0597, procustes correlation=0.3609
adonis(formula=matrix_scale_effect ~Selfing_rate ,data=traits_all)
#R2=0.46, p=0.198
#Mantel gives very low correlation between selfing rate 
#and the effect distance matrix of seed set
bioenv(matrix_scale_effect~traits_all$Selfing_rate, method="pearson", trace=T)
#correlation=0.14



#3)Pollen size
pollen_size <- traits_all_scaled[,3]
pollen_size <- as.data.frame(pollen_size)
pollen_size <- pollen_size
rownames(pollen_size) <- rownames(pollen_size)
pollen_size_dist <- dist(pollen_size, diag=T, upper=T)
mantel(matrix_scale_effect, pollen_size_dist)
mantel(matrix_effect_size, pollen_size_dist)
mantel(abs(matrix_effect_size), pollen_size_dist)
mantel(abs(matrix_scale_reverted), pollen_size_dist)

#significance=0.233, r=0.1526
protest(matrix_scale_effect, pollen_size_dist)
#significance=0.519, procustes correlation=-0.05605
bioenv(matrix_scale_effect~traits_all$pollen_size, method="pearson", trace=T)
#correlation=0.11
adonis(formula=matrix_scale_effect ~pollen_size ,data=traits_all)


#4)Pollen per anther
pollen <- traits_all_scaled[,4]
pollen <- as.data.frame(pollen)
pollen <- pollen
rownames(pollen) <- rownames(pollen)
pollen_dist <- dist(pollen, diag=T, upper=T)
mantel(matrix_scale_effect, pollen_dist) 
mantel(matrix_effect_size, pollen_dist)
mantel(abs(matrix_effect_size), pollen_dist)
mantel(abs(matrix_scale_reverted), pollen_dist)

#significance=0.491,r=-0.1097
protest(matrix_scale_effect, pollen_dist)
#significance=0.907, procustes correlation=0.3174
bioenv(matrix_scale_effect~traits_all$mean_pollen_anther, method="pearson", trace=T)
#correlation=-0.13
adonis(formula=matrix_scale_effect ~mean_pollen_anther ,data=traits_all)

#5)Ovules
ovules <- traits_all_scaled[,5]
ovules <- as.data.frame(ovules)
rownames(ovules) <- rownames(ovules)
ovules_dist <- dist(ovules, diag=T, upper=T)
mantel(matrix_scale_effect, ovules_dist)
mantel(matrix_effect_size, ovules_dist)
mantel(abs(matrix_effect_size), ovules_dist)
mantel(abs(matrix_scale_reverted), ovules_dist)

#significance=0.487, r=-0.08
protest(matrix_scale_effect, ovules_dist)
#0.884, procustes correlation=0.2985
bioenv(matrix_scale_effect~traits_all$mean_ovules, method="pearson", trace=T)
#correlation=-0.14
adonis(formula=matrix_scale_effect ~mean_ovules ,data=traits_all)

#6)Pollen ovule ratio now
p_o_ratio <- traits_all_scaled[,6]
p_o_ratio <- as.data.frame(p_o_ratio)
rownames(p_o_ratio) <- rownames(p_o_ratio)
p_o_ratio_dist <- dist(p_o_ratio, diag=T, upper=T)
mantel(matrix_scale_effect, p_o_ratio_dist)
mantel(matrix_effect_size, p_o_ratio_dist)
mantel(abs(matrix_effect_size), p_o_ratio_dist)
mantel(abs(matrix_scale_reverted), p_o_ratio_dist)

#significance=0.752, r=-0.1533
protest(matrix_scale_effect, p_o_ratio_dist)
#significance=0.958, procustes correlation=0.2758
bioenv(matrix_scale_effect~traits_all$pollen_ovule_ratio, method="pearson", trace=T)
#correlation=0.0439
adonis(formula=matrix_scale_effect ~pollen_ovule_ratio ,data=traits_all)

#7)Anthers
anthers <- traits_all_scaled[,7]
anthers <- as.data.frame(anthers)
rownames(anthers) <- rownames(anthers)
anthers_dist <- dist(anthers, diag=T, upper=T)
mantel(matrix_scale_effect, anthers_dist)
mantel(matrix_effect_size, anthers_dist)
mantel(abs(matrix_effect_size), anthers_dist)
mantel(abs(matrix_scale_reverted), anthers_dist)

#significance=0.356, r=0.05802
protest(matrix_scale_effect, anthers_dist)
#significance=0.806, procustes corr=0.2352
bioenv(matrix_scale_effect~traits_all$anthers, method="pearson", trace=T)
#correlation=-0.0406
adonis(formula=matrix_scale_effect ~anthers ,data=traits_all)


#8)Stigma_area
stigma_area <- traits_all_scaled[,8]
stigma_area <- as.data.frame(stigma_area)
rownames(stigma_area) <- rownames(stigma_area)
stigma_area_dist <- dist(stigma_area, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_area_dist)
mantel(matrix_effect_size, stigma_area_dist)
mantel(abs(matrix_effect_size), stigma_area_dist)
mantel(abs(matrix_scale_reverted), stigma_area_dist)

#significance=0.025, r=0.4002
protest(matrix_scale_effect, stigma_area_dist)
#significance=0.657, procustes correlation=0.4041
bioenv(matrix_scale_effect~traits_all$stigma_area, method="pearson", trace=T)
#correlation=-0.0859
adonis(formula=matrix_scale_effect ~stigma_area ,data=traits_all)


#9)Stigma_length
stigma_length <- traits_all_scaled[,9]
stigma_length <- as.data.frame(stigma_length)
rownames(stigma_length) <- rownames(stigma_length)
stigma_length_dist <- dist(stigma_length, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_length_dist)
mantel(matrix_effect_size, stigma_length_dist)
mantel(abs(matrix_effect_size), stigma_length_dist)
mantel(abs(matrix_scale_reverted), stigma_length_dist)

#significance=0.179, r=0.07872
protest(matrix_scale_effect, stigma_length_dist)
protest(matrix_scale_effect, traits_all_scaled$stigma_length)
#significance=0.111, procustes correlation 0.4231
bioenv(matrix_scale_effect~traits_all$stigma_length, method="pearson", trace=T)
adonis(formula=matrix_scale_effect ~stigma_length ,data=traits_all)

#correlation=-0.167


#10)Stigma surface
stigma_surface <- traits_all_scaled[,10]
stigma_surface <- as.data.frame(stigma_surface)
rownames(stigma_surface) <- rownames(stigma_surface)
stigma_surface_dist <- dist(stigma_surface, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_surface_dist)
mantel(matrix_effect_size, stigma_surface_dist)
mantel(abs(matrix_effect_size), stigma_surface_dist)
mantel(abs(matrix_scale_reverted), stigma_surface_dist)

#significance=0.007, r=0.4469
protest(matrix_scale_effect, stigma_surface_dist)
#significance=0.796, procustes correlation=0.4063
bioenv(matrix_scale_effect~traits_all$stigma_surface, method="pearson", trace=T)
#correlation=-0.09


#11)Stigma width
stigma_width <- traits_all_scaled[,11]
stigma_width <- as.data.frame(stigma_width)
stigma_width <- stigma_width
rownames(stigma_width) <- rownames(stigma_width)
stigma_width_dist <- dist(stigma_width, diag=T, upper=T)
mantel(matrix_scale_effect, stigma_width_dist)
mantel(matrix_effect_size, stigma_width_dist)
mantel(abs(matrix_effect_size), stigma_width_dist)
mantel(abs(matrix_scale_reverted), stigma_width_dist)

#significance=0.891, r=-0.24
protest(matrix_scale_effect, stigma_width_dist)
#significance=0.253, procustes correlation=0.3819
bioenv(matrix_scale_effect~traits_all$stigma_width, method="pearson", trace=T)
#correlation=0.33

#12)Style_length
style_length <- traits_all_scaled[,12]
style_length <- as.data.frame(style_length)
rownames(style_length) <- rownames(style_length)
style_length_dist <- dist(style_length, diag=T, upper=T)
mantel(matrix_scale_effect, style_length_dist)
mantel(matrix_effect_size, style_length_dist)
mantel(abs(matrix_effect_size), style_length_dist)
mantel(abs(matrix_scale_reverted), style_length_dist)


#significance=0.303, r=0.05021
protest(matrix_scale_effect, style_length_dist)
#significance=0.113, procustes correlation=0.3995
bioenv(matrix_scale_effect~traits_all$style_length, method="pearson", trace=T)
#correlation=-0.1757

#13)Style_width
style_width <- traits_all_scaled[,13]
style_width <- as.data.frame(style_width)
rownames(style_width) <- rownames(style_width)
style_width_dist <- dist(style_width, diag=T, upper=T)
mantel(matrix_scale_effect, style_width_dist)
mantel(matrix_effect_size, style_width_dist)
mantel(abs(matrix_effect_size), style_width_dist)
mantel(abs(matrix_scale_reverted), style_width_dist)

#significance=0.0787, r=0.07872
protest(matrix_scale_effect, style_width_dist)
#significance=0.115, procustes correlation=0.4231

#13)Ovary_width
ovary_width <- traits_all_scaled[,14]
ovary_width <- as.data.frame(ovary_width)
rownames(ovary_width) <- rownames(ovary_width)
ovary_width_dist <- dist(ovary_width, diag=T, upper=T)
mantel(matrix_scale_effect, ovary_width_dist)
mantel(matrix_effect_size, ovary_width_dist)
mantel(abs(matrix_effect_size), ovary_width_dist)
mantel(abs(matrix_scale_reverted), ovary_width_dist)

#significance=0.0787, r=0.07872
protest(matrix_scale_effect, ovary_width_dist)
#significance=0.115, procustes correlation=0.4231

#13)Ovary_length
ovary_length <- traits_all_scaled[,15]
ovary_length <- as.data.frame(ovary_length)
rownames(ovary_length) <- rownames(ovary_length)
ovary_length_dist <- dist(ovary_length, diag=T, upper=T)
mantel(matrix_scale_effect, ovary_length_dist)
mantel(matrix_effect_size, ovary_length_dist)
mantel(abs(matrix_effect_size), ovary_length_dist)
mantel(abs(matrix_scale_reverted), ovary_length_dist)

#significance=0.0787, r=0.07872
protest(matrix_scale_effect, ovary_length_dist)
#significance=0.115, procustes correlation=0.4231

#13)Ovary_length
si_index <- traits_all_scaled[,16]
si_index <- as.data.frame(si_index)
rownames(si_index) <- rownames(si_index)
si_index <- dist(si_index, diag=T, upper=T)
mantel(matrix_scale_effect, si_index)
#protest(matrix_effect_size, si_index)
mantel(matrix_effect_size, si_index)
mantel(abs(matrix_effect_size), si_index)
mantel(abs(matrix_scale_reverted), si_index)

#significance=0.0787, r=0.07872
protest(matrix_scale_effect, si_index)
#significance=0.115, procustes correlation=0.4231
#Save environment to load object in Markdown
#save.image(file='Manuscript_draft/myEnvironment_mantel.RData')


#I'm going to try to remove both convolvulaceae spp
#And redo analysis
matrix_soly=matrix_scale_effect[c(3,7,9,10),c(3,7,9,10)]
evo_distance_its_square_root=evo_distance_its_square_root[c(3,7,9,10),c(3,7,9,10)]
mantel(matrix_soly,evo_distance_its_square_root)
evo_distance_rbcl=evo_distance_rbcl[-c(5,6),-c(5,6)]
mantel(matrix_scale_effect,sqrt(evo_distance_rbcl))

traits_all=traits_all[c(3,7,9,10),]
mantel(matrix_soly,dist(traits_all[,2]))
traits_soly=traits_all[,-c(1,4,7,14,13,15)]
bioenv(matrix_soly,traits_soly)


mantel(matrix_scale_effect,dist(traits_all[,17]))
traits_all=traits_all[,-c(1,2)]
str(traits_all)
#bioenv(matrix_scale_effect~dist(traits_all), method="pearson", trace=T)







#After checking the correlations between the morphological traits, we clean some variables
#For stigma we are going to use one measurement that is going to be the width seen from the top 
#which is highly correlated with the area (square micrometers) also seen from the top
#For style we are going to use just the length

traits_all_scaled_clean <- traits_all_scaled[,-c(1,3,4,5,7,8,11,12,13,14,15)]
all_scaled_clean_dist <- dist(traits_all_scaled_clean)
mantel(matrix_scale_effect, all_scaled_clean_dist)

protest(matrix_scale_effect, all_scaled_clean_dist)

traits_checking <- traits_all_scaled[,-c(2,3,4,5,7,8,9,11,12,13,14,15)]
traits_checking_dist <- dist(traits_checking)

mantel(matrix_scale_effect, traits_checking_dist)
protest(matrix_scale_effect, traits_checking_dist)

cor.test(traits_all_scaled$stigma_type, traits_all_scaled$stigma_surface)


#
##
#### PART 3 visualising this differences
##
#

#Firs I melt the matyrics into long format
its_distance <- melt(evo_distance_its_square_root, id.vars = as.character(evo_distance_its_square_root[,0]))
scaled_effect <- melt(matrix_scale_effect)

#I assing a new column with colours to differentiate the points
scaled_effect$Species=as.character(scaled_effect$Species)
scaled_effect$col_focal[scaled_effect$Species==c("BROL")] <- "red"
scaled_effect$col_focal[scaled_effect$Species==c("BRRA")] <- "red"
scaled_effect$col_focal[scaled_effect$Species==c("SIAL")] <- "red"
scaled_effect$col_focal[scaled_effect$Species==c("ERSA")] <- "red"
scaled_effect$col_focal[scaled_effect$Species==c("SOME")] <- "blue"
scaled_effect$col_focal[scaled_effect$Species==c("SOLY")] <- "blue"
scaled_effect$col_focal[scaled_effect$Species==c("PEIN")] <- "blue"
scaled_effect$col_focal[scaled_effect$Species==c("CAAN")] <- "blue"
scaled_effect$col_focal[scaled_effect$Species==c("IPAQ")] <- "black"
scaled_effect$col_focal[scaled_effect$Species==c("IPPU")] <- "black"
#Other column in case I want to differentiate by the donor
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("BROL")] <- "red"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("BRRA")] <- "red"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("SIAL")] <- "red"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("ERSA")] <- "red"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("SOME")] <- "blue"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("SOLY")] <- "blue"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("PEIN")] <- "blue"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("CAAN")] <- "blue"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("IPAQ")] <- "blue"
scaled_effect$col_non_focal[scaled_effect$Non_focal ==c("IPPU")] <- "blue"


#Now I plot the points
plot(its_distance$value, scaled_effect$value, main="Scatterplot ", 
     xlab="dist", ylab="effect", pch=19, col=scaled_effect$col_focal)

#locator(1)
draw.circle(0.265,0.22,radius=0.045,nv=100,border="red",col=NA,lty=1,density=NULL,angle=45,lwd=1)
draw.circle(0.265,2.3,radius=0.045,nv=100,border="red",col=NA,lty=1,density=NULL,angle=45,lwd=1)
draw.circle(0.245,1.8,radius=0.07,nv=100,border="blue",col=NA,lty=1,density=NULL,angle=45,lwd=1)

#Maybe a regression line makes the plot more intuituve
#Let's check

#General model for all 
focal <- cbind(scaled_effect,its_distance)
focal <- data.frame(focal, stringsAsFactors = F)

focal_brol <- focal[ which( focal$Species== "BROL"& focal$Non_focal!="BROL") , ]
focal_brra <- focal[ which( focal$Species== "BRRA"& focal$Non_focal!="BRRA") , ]
focal_ersa <- focal[ which( focal$Species== "ERSA"& focal$Non_focal!="ERSA") , ]
focal_sial <- focal[ which( focal$Species== "SIAL"& focal$Non_focal!="SIAL") , ]
focal_some <- focal[ which( focal$Species== "SOME"& focal$Non_focal!="SOME") , ]
focal_soly <- focal[ which( focal$Species== "SOLY"& focal$Non_focal!="SOLY") , ]
focal_pein <- focal[ which( focal$Species== "PEIN"& focal$Non_focal!="PEIN") , ]
focal_caan <- focal[ which( focal$Species== "CAAN"& focal$Non_focal!="CAAN") , ]
focal_ipaq <- focal[ which( focal$Species== "IPAQ"& focal$Non_focal!="IPAQ") , ]
focal_ippu <- focal[ which( focal$Species== "IPPU"& focal$Non_focal!="IPPU") , ]
str(focal_brol)
focal <- rbind(focal_brol, focal_brra, focal_ersa, focal_sial, focal_some, focal_soly, 
      focal_pein, focal_caan, focal_ipaq, focal_ippu)

focal_bra_a <- focal[focal$Species==c("BROL"),]
focal_bra_b <- focal[focal$Species==c("BRRA"),]
focal_bra_c <- focal[focal$Species==c("SIAL"),]
focal_bra_d <- focal[focal$Species==c("ERSA"),]
length((focal_bra$Species))
focal_bra <- rbind(focal_bra_a,focal_bra_b,focal_bra_c,focal_bra_d)
length(prueba$Species)

focal_sol_a <- focal[focal$Species==c("SOME"),]
focal_sol_b <- focal[focal$Species==c("SOLY"),]
focal_sol_c <- focal[focal$Species==c("PEIN"),]
focal_sol_d <- focal[focal$Species==c("CAAN"),]

focal_sol <- rbind(focal_sol_a,focal_sol_b,focal_sol_c,focal_sol_d)

focal_con_a <- focal[focal$Species==c("IPAQ"),]
focal_con_b <- focal[focal$Species==c("IPPU"),]
focal_con <- rbind(focal_con_a, focal_con_b)


colnames(focal)[3] <- "hp_effect"
colnames(focal)[7] <- "its_distance"


plot(focal$hp_effect ~ focal$its_distance, main="", xlim=c(0.05,0.6),
     xlab="Evolutive distance", ylab="Hp effect", pch=19, col=focal$col_focal)

legend(x=0.04,y=3.2,legend=c("Brassicaceae","Convolvulaceae","Solanaceae"),pch=(c(16,16,16)), 
       col = c("red","black","blue"), bty="n",cex=0.6,pt.cex=0.6,xpd=TRUE)

#draw.circle(0.265,0.22,radius=0.045,nv=100,border="red",col=NA,lty=1,density=NULL,angle=45,lwd=1)
#draw.circle(0.265,2.3,radius=0.045,nv=100,border="red",col=NA,lty=1,density=NULL,angle=45,lwd=1)
#draw.circle(0.245,1.8,radius=0.07,nv=100,border="blue",col=NA,lty=1,density=NULL,angle=45,lwd=1)

model_bra <- lm(focal_bra$value ~ focal_bra$value.1)
#summary(model_brra)
abline(model_bra, col="red")
model_sol <- lm(focal_sol$value ~ focal_sol$value.1)
#summary(model_sol)
abline(model_sol, col="blue")
model_con <- lm(focal_con$value ~ focal_con$value.1)
#summary(model_con)
abline(model_con, col="black")


#Now I'm going to prepare the same plot but just with points of each family                 
#donor same family
key(focal_brol)
focal_brol$Non_focal=as.character(focal_brol$Non_focal)
#Brassicaceae donor brassicaceae
a <- subset(focal_brol, Non_focal=="BRRA" | Non_focal=="ERSA"| Non_focal=="SIAL" | Non_focal=="BROL")
b <- subset(focal_brra, Non_focal=="BROL" | Non_focal=="ERSA"| Non_focal=="SIAL" | Non_focal=="BRRA")
c <- subset(focal_ersa, Non_focal=="BROL" | Non_focal=="ERSA"| Non_focal=="SIAL" | Non_focal=="BRRA")
d <- subset(focal_sial, Non_focal=="BROL" | Non_focal=="ERSA"| Non_focal=="SIAL" | Non_focal=="BRRA")

bra <- rbind(a,b,c,d)

#Solanaceae donor solanaceae

e <- subset(focal_soly, Non_focal=="CAAN" | Non_focal=="PEIN"| Non_focal=="SOLY" | Non_focal=="SOME")
f <- subset(focal_some, Non_focal=="CAAN" | Non_focal=="PEIN"| Non_focal=="SOLY" | Non_focal=="SOME")
g <- subset(focal_pein, Non_focal=="CAAN" | Non_focal=="PEIN"| Non_focal=="SOLY" | Non_focal=="SOME")
h <- subset(focal_caan, Non_focal=="CAAN" | Non_focal=="PEIN"| Non_focal=="SOLY" | Non_focal=="SOME")

sol <- rbind(e,f,g,h)

#Convolvulaceae with convolvulaceae

i <- subset(focal_ipaq, Non_focal=="IPPU" | Non_focal=="IPAQ")
k <- subset(focal_ippu, Non_focal=="IPPU" | Non_focal=="IPAQ")

con <- rbind(i,k)

all <- rbind(bra, sol, con)
all$col_focal[all$Species==c("BROL")] <- "red"
all$col_focal[all$Species==c("BRRA")] <- "red"
all$col_focal[all$Species==c("SIAL")] <- "red"
all$col_focal[all$Species==c("ERSA")] <- "red"
all$col_focal[all$Species==c("SOME")] <- "blue"
all$col_focal[all$Species==c("SOLY")] <- "blue"
all$col_focal[all$Species==c("PEIN")] <- "blue"
all$col_focal[all$Species==c("CAAN")] <- "blue"
all$col_focal[all$Species==c("IPAQ")] <- "black"
all$col_focal[all$Species==c("IPPU")] <- "black"

all$close_related <-1

#Half of the plot done, now I prepare distant related donors (not of the same family)
plot(all$value ~jitter(all$close_related, factor=10),xlim=c(0,3), main="",
     xlab="Evolutive distance", ylab="Hp effect", pch=19, col=all$col_focal)

#Brassicaceae donor brassicaceae
a <- subset(focal_brol, Non_focal=="CAAN" | Non_focal=="SOLY"| Non_focal=="SOME" | Non_focal=="PEIN"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")

b <- subset(focal_brra, Non_focal=="CAAN" | Non_focal=="SOLY"| Non_focal=="SOME" | Non_focal=="PEIN"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")

c <- subset(focal_ersa, Non_focal=="CAAN" | Non_focal=="SOLY"| Non_focal=="SOME" | Non_focal=="PEIN"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")

d <- subset(focal_sial, Non_focal=="CAAN" | Non_focal=="SOLY"| Non_focal=="SOME" | Non_focal=="PEIN"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")

bra <- rbind(a,b,c,d)

e <- subset(focal_soly, Non_focal=="SIAL" | Non_focal=="ERSA"| Non_focal=="BROL" | Non_focal=="BRRA"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")
f <- subset(focal_pein, Non_focal=="SIAL" | Non_focal=="ERSA"| Non_focal=="BROL" | Non_focal=="BRRA"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")
g <- subset(focal_some, Non_focal=="SIAL" | Non_focal=="ERSA"| Non_focal=="BROL" | Non_focal=="BRRA"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")
h <- subset(focal_caan, Non_focal=="SIAL" | Non_focal=="ERSA"| Non_focal=="BROL" | Non_focal=="BRRA"
            | Non_focal=="IPAQ"| Non_focal=="IPPU")

sol <- rbind(e,f,g,h)

i <- subset(focal_ipaq, Non_focal=="SOME" | Non_focal=="SOLY"| Non_focal=="PEIN"| Non_focal=="CAAN"
            | Non_focal=="BROL"| Non_focal=="BRRA"| Non_focal=="ERSA"| Non_focal=="SIAL")
k <- subset(focal_ippu, Non_focal=="SOME" | Non_focal=="SOLY"| Non_focal=="PEIN"| Non_focal=="CAAN"
            | Non_focal=="BROL"| Non_focal=="BRRA"| Non_focal=="ERSA"| Non_focal=="SIAL")
con <- rbind(i,k)
all_non_focal <- rbind(bra, sol, con)

all_non_focal$close_related <-2

all <- rbind(all, all_non_focal)

#write.csv(all, "Data/all.csv")

#Now I plot corsses between family and across families, to see if the cloud of point is different
plot(all$value ~jitter(all$close_related, factor=1),xaxt='n',xlim=c(0,3), main="",
     xlab="Evolutive distance", ylab="Hp effect", pch=19, col=all$col_focal)
axis(1, at=1:2, labels=c("family crosses", "across family" ))

all$compatibility <- "black"
all$compatibility[all$Species==c("BROL")] <- "cyan"
all$compatibility[all$Species==c("BRRA")] <- "cyan"
all$compatibility[all$Species==c("ERSA")] <- "red"
all$compatibility[all$Species==c("SIAL")] <- "red"


plot(all$value ~jitter(all$close_related, factor=1),xaxt='n',xlim=c(0,3), main="",
     xlab="Evolutive distance", ylab="Hp effect", pch=19, col=all$compatibility)
axis(1, at=1:2, labels=c("family crosses", "across family crosses" ))

#Because of the interesting results of Brassicaceae I'm going to try to consider just this family 

all_bra <- subset(all, Species %in% c("BROL", "BRRA", "ERSA", "SIAL"))
#To make it more intuitive I rename the columns 
colnames(all_bra)[3] <- "hp_effect"
#Clean unwanted columns
all_bra=all_bra[,-c(6,7)]

#Selfing rates: Percentage of fruit produced with 10 hand self pollination treatments
all_bra$compatibility[all_bra$Species==c("BROL")] <- "blue"  #100
all_bra$compatibility[all_bra$Species==c("BRRA")] <- "red"   #100
all_bra$compatibility[all_bra$Species==c("ERSA")] <- "green" #98
all_bra$compatibility[all_bra$Species==c("SIAL")] <- "grey"  #0 selfing

a<- mean(sial[sial$Treatment=="Self", "Seed.production"])
b<- mean(sial[sial$Treatment=="Cross", "Seed.production"])
100- (a/b*100) #[1] -11.5 so 0 % of decrease

a<- mean(ersa[ersa$Treatment=="Self", "seed.production"])
b<- mean(ersa[ersa$Treatment=="Cross", "seed.production"])
100- (a/b*100) #[1] 98%


plot(all_bra$hp_effect ~jitter(all_bra$close_related, factor=1),xaxt='n',xlim=c(0,3), main="",
     xlab="Evolutive distance", ylab="Hp effect", pch=19, col=all_bra$compatibility)
axis(1, at=1:2, labels=c("family crosses", "across family crosses" ))


#lib
library(ggplot2)
library(cowplot)

all_bra$close_related <- 1
colnames(all_bra)[1]<-"Focal"
head(all_bra)
all_bra$Pollen_recipient[all_bra$Focal=="BROL"] <-"Brassica oleracea. 
Selfing decrease=100%"
all_bra$Pollen_recipient[all_bra$Focal=="BRRA"] <-"Brassica rapa. 
Selfing decrease=100%"
all_bra$Pollen_recipient[all_bra$Focal=="SIAL"] <-"Sinapis alba. 
Selfing decrease=0%"
all_bra$Pollen_recipient[all_bra$Focal=="ERSA"] <-"Eruca versicaria. 
Selfing decrease=98%"

cbPalette <- c("#000000", "#E69F00", "darkturquoise", "deeppink3")
save(all_bra ,file="Manuscript_draft/Data/all_bra.Rda")


 ggplot(all_bra, aes(x=close_related, y=hp_effect)) + 
  geom_jitter(width=0.1,aes(colour = Pollen_recipient),size=4)+
  theme_cowplot()+theme(axis.title.x=element_blank(),
axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position = c(0.7, 0.92))+
  scale_x_continuous(limits = c(0.8, 1.35)) +scale_y_continuous("Heterospecific pollen effect")+
   annotate("text", x = c(0.82,0.82), y=c(0.4,2.2), label = c("Low effect","High effect"))+
   geom_hline(yintercept = 1.5)+scale_color_manual(values = cbPalette)+labs(title = "Brassicaceae spp", subtitle = "")

#Now I do the same for Solanaceae species
 
 all_sol <- subset(all, Species %in% c("SOME", "SOLY", "CAAN", "PEIN"))
 #To make it more intuitive I rename the columns 
 colnames(all_sol)[3] <- "hp_effect"
 #Clean unwanted columns
 all_sol=all_sol[,-c(6,7)]
 
 #Selfing rates: Percentage of fruit produced with 10 hand self pollination treatments
 all_sol$compatibility[all_sol$Species==c("SOME")] <- "blue"  #0
 all_sol$compatibility[all_sol$Species==c("PEIN")] <- "red"   #0
 all_sol$compatibility[all_sol$Species==c("SOLY")] <- "green" #0
 all_sol$compatibility[all_sol$Species==c("CAAN")] <- "black"  #0
 
 plot(all_sol$hp_effect ~jitter(all_sol$close_related, factor=1),xaxt='n',xlim=c(0,3), main="",
      xlab="Evolutive distance", ylab="Hp effect", pch=19, col=all_sol$compatibility)
 axis(1, at=1:2, labels=c("family crosses", "across family crosses" ))
 #Because fruit set is not very informative for solanaceae I check now this numbers with
 #Percentage of decrease of seed set between self and cross
 #This makes me think that in the matrix of traits maybe it should be adressed on this way too
 #pein
 a<- mean(pein[pein$Treatment=="SELF", "Seed.production"])
 b<- mean(pein[pein$Treatment=="CROSS", "Seed.production"])
 100- (a/b*100) #[1] 73.58216 Decrease in seed set for PEIN
 #soly
 a<- mean(soly[soly$Treatment=="SELF", "seed_set"])
 b<- mean(soly[soly$Treatment=="CROSS", "seed_set"])
 100- (a/b*100) #51.89417
 #caan
 a<- mean(caan[caan$treatment=="SELF", "seed_set"])
 b<- mean(caan[caan$treatment=="CROSS", "seed_set"])
 100- (a/b*100) #36.389
 #some
 a<- mean(some[some$Treatment=="SELF", "seed_set"])
 b<- mean(some[some$Treatment=="CROSS", "seed_set"])
 100- (a/b*100) #0% even better with self (-45.46496)
 #Now I'm going to compare with the scaled seed set
 a<- mean(pein[pein$Treatment=="SELF", "scaled_seed"])
 b<- mean(pein[pein$Treatment=="CROSS", "scaled_seed"])
 100- (a/b*100) #[1] 73.58216 Decrease in seed set for PEIN
 
 
 
 all_sol$close_related <- 1
 colnames(all_sol)[1]<-"Focal"
 head(all_sol)
 
 all_sol$Pollen_recipient[all_sol$Focal=="SOME"] <-"Solanum melongena. 
 Selfing decrease=0%"
 all_sol$Pollen_recipient[all_sol$Focal=="PEIN"] <-"Petunia integrifolia. 
 Selfing decrease=73.6%"
 all_sol$Pollen_recipient[all_sol$Focal=="CAAN"] <-"Capsicum annuum. 
 Selfing decrease=36.4%"
 all_sol$Pollen_recipient[all_sol$Focal=="SOLY"] <-"Solanum lycopersicum. 
 Selfing decrease=51.9%"
 
 cbPalette <- c("#000000", "#E69F00", "darkturquoise", "deeppink3")
 save(all_sol ,file="Manuscript_draft/Data/all_sol.Rda")
 
 
 ggplot(all_sol, aes(x=close_related, y=hp_effect)) + 
   geom_jitter(width=0.1,aes(colour = Pollen_recipient),size=4)+
   theme_cowplot()+theme(axis.title.x=element_blank(),
                         axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position = c(0.7, 0.92))+
   scale_x_continuous(limits = c(0.8, 1.35)) +scale_y_continuous("Heterospecific pollen effect")+
   annotate("text", x = c(0.82,0.82), y=c(0.4,2.2), label = c("Low effect","High effect"))+
   geom_hline(yintercept = 1.5)+scale_color_manual(values = cbPalette)+labs(title = "Solanaceae spp", subtitle = "")
 
 
 #Now I do the same for Convolvulaceae species
 
 all_con <- subset(all, Species %in% c("IPAQ", "IPPU"))
 #To make it more intuitive I rename the columns 
 colnames(all_con)[3] <- "hp_effect"
 #Clean unwanted columns
 all_con=all_con[,-c(6,7)]
 
 #Selfing rates: Percentage of fruit produced with 10 hand self pollination treatments
 all_con$compatibility[all_con$Species==c("IPAQ")] <- "blue"  #0
 all_con$compatibility[all_con$Species==c("IPPU")] <- "red"   #0
 
 all_con$close_related <- 1
 colnames(all_con)[1]<-"Focal"
 head(all_con)
 
 a<- mean(ippu[ippu$treatment=="self", "seed.set"])
 b<- mean(ippu[ippu$treatment=="cross", "seed.set"])
 100- (a/b*100) #[1] -173.6842 This means that selfin produced 173% more seeds
 
 a<- mean(ipaq[ipaq$treatment=="self", "seed_set"])
 b<- mean(ipaq[ipaq$treatment=="cross", "seed_set"])
 100- (a/b*100) #[1] 25% 
 
 
 all_con$Pollen_recipient[all_con$Focal=="IPAQ"] <-"Ipomoea aquatica. 
 Selfing decrease=25%"
 all_con$Pollen_recipient[all_con$Focal=="IPPU"] <-"Ipomoea purpurea. 
 Selfing decrease=0%"
 
 cbPalette <- c("#000000", "#E69F00", "darkturquoise", "deeppink3")
 save(all_con ,file="Manuscript_draft/Data/all_con.Rda")
 
 
 ggplot(all_con, aes(x=close_related, y=hp_effect)) + 
   geom_jitter(width=0.1,aes(colour = Pollen_recipient),size=4)+
   theme_cowplot()+theme(axis.title.x=element_blank(),
                         axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position = c(0.6, 0.92))+
   scale_x_continuous(limits = c(0.8, 1.35)) +scale_y_continuous("Heterospecific pollen effect")+
   annotate("text", x = c(0.82,0.82), y=c(0.4,2.2), label = c("Low effect","High effect"))+
   geom_hline(yintercept = 1.5)+scale_color_manual(values = cbPalette)+labs(title = "Solanaceae spp", subtitle = "")
 
 
 #After Nacho and Romina advices I'm going to put on the X axis the selfing rate
 #Basically I have to add just a new column with this values
 #I start with Brassicaceae
 
 a<- mean(sial[sial$Treatment=="Self", "Seed.production"])
 b<- mean(sial[sial$Treatment=="Cross", "Seed.production"])
(a/b*100) #[1] 100% selfing
 
 a<- mean(ersa[ersa$Treatment=="Self", "seed.production"])
 b<- mean(ersa[ersa$Treatment=="Cross", "seed.production"])
 (a/b*100) #[1] 2% selfing
 
 #BROL and BRRA 0%
 
 
#How I should call this column? Selfing decrease doesn't seem that intuitive 
all_bra$compatibility[all_bra$Focal=="BROL"] <- 0
all_bra$compatibility[all_bra$Focal=="BRRA"] <- 0
all_bra$compatibility[all_bra$Focal=="SIAL"] <- 100
all_bra$compatibility[all_bra$Focal=="ERSA"] <- 2
str(all_bra)
all_bra$compatibility=as.numeric(all_bra$compatibility)
ggplot(all_bra, aes(x=compatibility, y=hp_effect)) + 
  geom_jitter(width=1.5,aes(colour = Focal),size=4)


#Solanaceae
all_sol$compatibility[all_sol$Focal=="CAAN"] <- 64
all_sol$compatibility[all_sol$Focal=="SOLY"] <- 48
all_sol$compatibility[all_sol$Focal=="SOME"] <- 100
all_sol$compatibility[all_sol$Focal=="PEIN"] <- 26
all_sol$compatibility=as.numeric(all_sol$compatibility)

ggplot(all_sol, aes(x=compatibility, y=hp_effect)) + 
  geom_jitter(width=1.5,aes(colour = Focal),size=4)

all_con$compatibility[all_con$Focal=="IPAQ"] <- 75
all_con$compatibility[all_con$Focal=="IPPU"] <- 100
all_con$compatibility=as.numeric(all_con$compatibility)

ggplot(all_con, aes(x=compatibility, y=hp_effect)) + 
  geom_jitter(width=1.5,aes(colour = Focal),size=4)

ALL <- rbind(all_bra, all_sol, all_con)

ggplot(ALL, aes(x=compatibility, y=hp_effect)) + 
  geom_jitter(width=1.5,aes(colour = Focal),size=4)

#Load library for GLMM
library(nlme)
library(lme4)

model1=lm(hp_effect~compatibility, data=ALL)
summary(model1)

model2=lme(hp_effect~compatibility, data=ALL, random=)
summary(model2)
coef(model2)
plot(ranef(model2))
plot(model2)

#Lets add new column with a seq of the length of the dataset and i use that as random effect (different individuals)

#save.image(file='Manuscript_draft/myEnv.RData')
#Index of self compatibility Lloyd and Schoen 1992
ALL$indv<- seq.int(1:10)
model2=lme(hp_effect~compatibility, data=ALL, random=~1|indv)
summary(model2)
tempEf$fit <- predict(model) 

#Model.2 <- lmer(hp_effect ~ compatibility + (1 | Focal), data = ALL)
#summary(Model.2)
#ggplot(ALL, aes(x=compatibility, y=hp_effect)) + 
  geom_jitter(width=1.5,aes(colour = Focal),size=4)+geom_line(aes(y=predict(model2), group=hp_effect))
ggplot(ALL, aes(x=compatibility, y=hp_effect)) + 
  geom_jitter(width=1.5,aes(colour = Focal),size=4)+
  geom_abline(aes(intercept=`(Intercept)`, slope=compatibility), as.data.frame(t(fixef(model2))))+theme_cowplot()
