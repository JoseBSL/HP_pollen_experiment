#Preparing seed set plots, starting with Petunia

#There are 3 crosses were seeds were lost (2,7,9)
#load libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(lattice)
#read data and cleaning
pein_seed_set <- read.csv("Data/species_seed_set/PEIN_seed_set.csv", sep=";")
#removing samples where seeds got lost, first ones of the experiment
pein_seed_set <- pein_seed_set[-c(42,47,49),]

#
##
###
#PETUNIA
###
##
#
#Removing RARA, species not considered because sterility 

pein_seed_set <- filter(pein_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%")

#adding the focal species, first 50%
Species <- rep("PEIN", 10)
Treatment <- rep("PEIN 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
pein_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production,stringsAsFactors = F)
#adding the focal species, 100%
Species <- rep("PEIN", 10)
Treatment <- rep("PEIN 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
pein_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production,stringsAsFactors = F)
#Order from lower to higher values the average, to plot it nicely
pein_seed_set <- rbind(pein_seed_set, pein_50, pein_100)
pein_seed_set$Treatment<- as.character(pein_seed_set$Treatment)
pein_seed_set <- pein_seed_set[order(pein_seed_set$Treatment),]  
#Organizing with just 50% and cross, self, flower control and control on a side


library(stringr)
pein_seed_set_div <- str_split_fixed(as.character(pein_seed_set$Treatment), " ", 2)
pein_seed_set_div <- data.frame(pein_seed_set_div, stringsAsFactors = F)
colnames(pein_seed_set_div)=c("non_focal","percentage")
pein_seed_set_bind <- cbind(pein_seed_set,pein_seed_set_div)
pein_seed_set_bind <- filter(pein_seed_set_bind, percentage!="50%")
#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(pein_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

pein_seed_set_cross <- filter(pein_seed_set_bind, non_focal %in% c("CROSS"))
pein_seed_set_self <- filter(pein_seed_set_bind, non_focal %in% c("SELF"))
pein_seed_set_control <- filter(pein_seed_set_bind, non_focal %in% c("CONTROL"))
pein_seed_set_flower <- filter(pein_seed_set_bind, non_focal %in% c("FLOWER"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("CROSS"),]
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("SELF"),]
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("CONTROL"),]
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("FLOWER"),]

pein_seed_set_final<-rbind(pein_seed_set_bind,pein_seed_set_cross, pein_seed_set_self, pein_seed_set_control,
                           pein_seed_set_flower)
#deleting extra columns used for data formating
pein_seed_set_final=pein_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
pein_seed_set_final[pein_seed_set_final$Treatment==c("PEIN 50%"),4] <- NA
#changing non_focal species name
pein_seed_set_final$Treatment=as.character(pein_seed_set_final$Treatment)
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="BROL 100%"] <- "Brassicaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="BRRA 100%"] <- "Brassicaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="SIAL 100%"] <- "Brassicaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="ERSA 100%"] <- "Brassicaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="SOME 100%"] <- "Solanaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="SOLY 100%"] <- "Solanaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="CAAN 100%"] <- "Solanaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="PEIN 100%"] <- "Solanaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="IPPU 100 %"] <- "Convolvulaceae"
pein_seed_set_final$Family[pein_seed_set_final$Treatment=="IPAQ 100%"] <- "Convolvulaceae"

#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="IPPU 50 %"] <- "Ipomoea purpurea"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
#pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"

pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="CROSS"] <- "Cross"
pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="SELF"] <- "Self"
pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="CONTROL"] <- "Control"
pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="FLOWER CONTROL"] <- "Flower control"
pein_seed_set_brassicaceae <- filter(pein_seed_set_final, Family %in% c("Brassicaceae"))
pein_seed_set_convolvulaceae <- filter(pein_seed_set_final, Family %in% c("Convolvulaceae"))
pein_seed_set_solanaceae <- filter(pein_seed_set_final, Family %in% c("Solanaceae"))
pein_seed_set_final$Family[is.na(pein_seed_set_final$Family)] <- "Solanum lycopersicum"
pein_seed_set_cross=pein_seed_set_cross[,-c(5,6)]
pein_seed_set_cross$Family <- "other"
pein_seed_set_self=pein_seed_set_self[,-c(5,6)]
pein_seed_set_self$Family <- "other"
pein_seed_set_control=pein_seed_set_control[,-c(5,6)]
pein_seed_set_control$Family <- "other"
pein_seed_set_flower=pein_seed_set_flower[,-c(5,6)]
pein_seed_set_flower$Family <- "other"

pein_seed_set_final=rbind(pein_seed_set_brassicaceae, pein_seed_set_convolvulaceae, 
                          pein_seed_set_solanaceae, pein_seed_set_cross, pein_seed_set_self, pein_seed_set_control,
                          pein_seed_set_flower)

write.csv(pein_seed_set_final, "Rmd/Data/pein_seed_set_100.csv")