#
##
###
#B. OLERACEA
###
##
#
library(dplyr)
library(reshape2)
library(ggplot2)

brol_seed_set <- read.csv("Data/species_seed_set/BROL_seed_set.csv", sep=";")
brol_seed_set <- filter(brol_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%"& Treatment!="COSA 50%"& Treatment!="COSA 100%" )

#adding the focal species, first 50%
Species <- rep("BROL", 10)
Treatment <- rep("BROL 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
brol_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("BROL", 10)
Treatment <- rep("BROL 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
brol_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
colnames(brol_seed_set)[1] <- "Species"
colnames(brol_seed_set)[2] <- "Treatment"
colnames(brol_seed_set)[3] <- "Treatment.number"
colnames(brol_seed_set)[4] <- "Seed.production"
brol_seed_set <- rbind(brol_seed_set, brol_50, brol_100)
brol_seed_set$Treatment<- as.character(brol_seed_set$Treatment)
brol_seed_set <- brol_seed_set[order(brol_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
brol_seed_set_div <- str_split_fixed(as.character(brol_seed_set$Treatment), " ", 2)
brol_seed_set_div <- data.frame(brol_seed_set_div, stringsAsFactors = F)
colnames(brol_seed_set_div)=c("non_focal","percentage")
brol_seed_set_bind <- cbind(brol_seed_set,brol_seed_set_div)
brol_seed_set_bind <- filter(brol_seed_set_bind, percentage!="50%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(brol_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

brol_seed_set_cross <- filter(brol_seed_set_bind, non_focal %in% c("Cross"))
brol_seed_set_self <- filter(brol_seed_set_bind, non_focal %in% c("Self"))
brol_seed_set_control <- filter(brol_seed_set_bind, non_focal %in% c("Control"))
brol_seed_set_flower <- filter(brol_seed_set_bind, non_focal %in% c("FC"))

#Changing 0 for NA'S FOCAL species

# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
brol_seed_set_bind=brol_seed_set_bind[brol_seed_set_bind$non_focal!=c("Cross"),]
brol_seed_set_bind=brol_seed_set_bind[brol_seed_set_bind$non_focal!=c("Self"),]
brol_seed_set_bind=brol_seed_set_bind[brol_seed_set_bind$non_focal!=c("Control"),]
brol_seed_set_bind=brol_seed_set_bind[brol_seed_set_bind$non_focal!=c("FC"),]

brol_seed_set_final<-rbind(brol_seed_set_bind,brol_seed_set_cross, brol_seed_set_self, brol_seed_set_control,
                           brol_seed_set_flower)
#deleting extra columns used for data formating
brol_seed_set_final=brol_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
brol_seed_set_final[brol_seed_set_final$Treatment==c("BROL 50%"),4] <- NA

#changing non_focal species name
brol_seed_set_final$Treatment=as.character(brol_seed_set_final$Treatment)
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="BROL 100%"] <- "Brassicaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="BRRA 100%"] <- "Brassicaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="SIAL 100%"] <- "Brassicaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="ERSA 100%"] <- "Brassicaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="CAAN 100%"] <- "Solanaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="SOLY 100%"] <- "Solanaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="SOME 100%"] <- "Solanaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="PEIN 100%"] <- "Solanaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="IPPU 100%"] <- "Convolvulaceae"
brol_seed_set_final$Family[brol_seed_set_final$Treatment=="IPAQ 100%"] <- "Convolvulaceae"

brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="BROL 100%"] <- "Brassica oleracea"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="BRRA 100%"] <- "Brassica rapa"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="CAAN 100%"] <- "Capsicum annuum"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="ERSA 100%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="IPAQ 100%"] <- "Ipomoea aquatica"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="IPPU 100%"] <- "Ipomoea purpurea"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="PEIN 100%"] <- "Petunia integrifolia"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="SIAL 100%"] <- "Sinapis alba"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="SOLY 100%"] <- "Solanum lycopersicum"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="SOME 100%"] <- "Solanum melongena"
brol_seed_set_final$Treatment[brol_seed_set_final$Treatment=="FC"] <- "Flower control"

brol_seed_set_brassicaceae <- filter(brol_seed_set_final, Family %in% c("Brassicaceae"))
brol_seed_set_convolvulaceae <- filter(brol_seed_set_final, Family %in% c("Convolvulaceae"))
brol_seed_set_solanaceae <- filter(brol_seed_set_final, Family %in% c("Solanaceae"))
brol_seed_set_final$Family[is.na(brol_seed_set_final$Family)] <- "Solanum lycopersicum"

brol_seed_set_cross=brol_seed_set_cross[,-c(5,6)]
brol_seed_set_cross$Family <- "other"
brol_seed_set_self=brol_seed_set_self[,-c(5,6)]
brol_seed_set_self$Family <- "other"
brol_seed_set_control=brol_seed_set_control[,-c(5,6)]
brol_seed_set_control$Family <- "other"
brol_seed_set_flower=brol_seed_set_flower[,-c(5,6)]
brol_seed_set_flower$Family <- "other"

brol_seed_set_final=rbind(brol_seed_set_brassicaceae, brol_seed_set_convolvulaceae, 
                          brol_seed_set_solanaceae, brol_seed_set_cross, brol_seed_set_self, brol_seed_set_control,
                          brol_seed_set_flower)

write.csv(brol_seed_set_final, "Rmd/Data/brol_seed_set_100.csv")
