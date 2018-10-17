#In this script I´m going to prepare the data to do some first Analysis

#In first place I modify the data to create a matrix

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

#load libraries

library(dplyr)
library(reshape2)
library(stringr)


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
colnames(y_cross)[2] <- "Seed_seet_cross"
y_all <- merge(y_cross, y_cross, by="Species")




