#In this script I´m going to prepare the data to do some first Analysis

#Load seed set data for 10 species

soly  <- read.csv("Data/species_seed_set/soly_seed_set.csv", sep=";", stringsAsFactors = F)
some  <- read.csv("Data/species_seed_set/some_seed_set.csv", sep=";", stringsAsFactors = F)
pein  <- read.csv("Data/species_seed_set/pein_seed_set.csv", sep=";")
caan  <- read.csv("Data/species_seed_set/caan_seed_set.csv", sep=";")
ersa  <- read.csv("Data/species_seed_set/ersa_seed_set.csv", sep=";")
brra  <- read.csv("Data/species_seed_set/brra_seed_set.csv", sep=";")
sial  <- read.csv("Data/species_seed_set/sial_seed_set.csv", sep=";")
brol  <- read.csv("Data/species_seed_set/brol_seed_set.csv", sep=";")
ippu  <- read.csv("Data/species_seed_set/ippu_seed_set.csv", sep=";")
ipaq  <- read.csv("Data/species_seed_set/ipaq_seed_set.csv", sep=";")

#load libraries

library(dplyr)
str(soly)


soly[-grep("100%", soly$Treatment), ]



#To create the matrix of effect we are going to estimate the net effect as 
#Cross-(average of 50% treatments)
#I´m going to try to do it in a for loop

species_list <- list(soly, some)

i <- NULL
y <- NULL
for (i in species_list){

 i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%")
 i <-i[-grep("100", i$Treatment),] 
  
  y <- rbind(y, i)
}

