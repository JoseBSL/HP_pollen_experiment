#
##
###
#S. LYCOPERSICUM
###
##
#


soly_seed_set <- read.csv("Data/species_seed_set/SOLY_seed_set.csv", sep=";")
soly_seed_set <- filter(soly_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%"& Treatment!="COSA 50%"& Treatment!="COSA 100%" )

#adding the focal species, first 50%
Species <- rep("SOLY", 10)
Treatment <- rep("SOLY 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
soly_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("SOLY", 10)
Treatment <- rep("SOLY 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
soly_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
soly_seed_set <- soly_seed_set[,-4]
colnames(soly_seed_set)[1] <- "Species"
colnames(soly_seed_set)[2] <- "Treatment"
colnames(soly_seed_set)[3] <- "Treatment.number"
colnames(soly_seed_set)[4] <- "Seed.production"
soly_seed_set <- rbind(soly_seed_set, soly_50, soly_100)
soly_seed_set$Treatment<- as.character(soly_seed_set$Treatment)
soly_seed_set <- soly_seed_set[order(soly_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
soly_seed_set_div <- str_split_fixed(as.character(soly_seed_set$Treatment), " ", 2)
soly_seed_set_div <- data.frame(soly_seed_set_div, stringsAsFactors = F)
colnames(soly_seed_set_div)=c("non_focal","percentage")
soly_seed_set_bind <- cbind(soly_seed_set,soly_seed_set_div)
soly_seed_set_bind <- filter(soly_seed_set_bind, percentage!="50%")

#Because I want to give it specifically order I do it separately
#caan_seed_set_common <- filter(soly_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

soly_seed_set_cross <- filter(soly_seed_set_bind, non_focal %in% c("CROSS"))
soly_seed_set_self <- filter(soly_seed_set_bind, non_focal %in% c("SELF"))
soly_seed_set_control <- filter(soly_seed_set_bind, non_focal %in% c("CONTROL"))
soly_seed_set_flower <- filter(soly_seed_set_bind, non_focal %in% c("FLOWER"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
soly_seed_set_bind=soly_seed_set_bind[soly_seed_set_bind$non_focal!=c("CROSS"),]
soly_seed_set_bind=soly_seed_set_bind[soly_seed_set_bind$non_focal!=c("SELF"),]
soly_seed_set_bind=soly_seed_set_bind[soly_seed_set_bind$non_focal!=c("CONTROL"),]
soly_seed_set_bind=soly_seed_set_bind[soly_seed_set_bind$non_focal!=c("FLOWER"),]

soly_seed_set_final<-rbind(soly_seed_set_bind,soly_seed_set_cross, soly_seed_set_self, soly_seed_set_control,
                           soly_seed_set_flower)
#deleting extra columns used for data formating
soly_seed_set_final=soly_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
soly_seed_set_final[soly_seed_set_final$Treatment==c("SOLY 50%"),4] <- NA

#changing non_focal species name
soly_seed_set_final$Treatment=as.character(soly_seed_set_final$Treatment)
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="BROL 100%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="BRRA 100%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="SIAL 100%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="ERSA 100%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="CAAN 100%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="SOLY 100%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="SOME 100%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="PEIN 100%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="IPPU 100%"] <- "Convolvulaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="IPAQ 100%"] <- "Convolvulaceae"

#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
#soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"


soly_seed_set_brassicaceae <- filter(soly_seed_set_final, Family %in% c("Brassicaceae"))
soly_seed_set_convolvulaceae <- filter(soly_seed_set_final, Family %in% c("Convolvulaceae"))
soly_seed_set_solanaceae <- filter(soly_seed_set_final, Family %in% c("Solanaceae"))
soly_seed_set_final$Family[is.na(soly_seed_set_final$Family)] <- "Solanum lycopersicum"
soly_seed_set_cross=soly_seed_set_cross[,-c(5,6)]
soly_seed_set_cross$Family <- "other"
soly_seed_set_self=soly_seed_set_self[,-c(5,6)]
soly_seed_set_self$Family <- "other"
soly_seed_set_control=soly_seed_set_control[,-c(5,6)]
soly_seed_set_control$Family <- "other"
soly_seed_set_flower=soly_seed_set_flower[,-c(5,6)]
soly_seed_set_flower$Family <- "other"

soly_seed_set_final=rbind(soly_seed_set_brassicaceae, soly_seed_set_convolvulaceae, 
                          soly_seed_set_solanaceae, soly_seed_set_cross, soly_seed_set_self, soly_seed_set_control,
                          soly_seed_set_flower)
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="CROSS"] <- "Cross"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="SELF"] <- "Self"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="CONTROL"] <- "Control"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="FLOWER CONTROL"] <- "Flower control"

write.csv(soly_seed_set_final, "Rmd/Data/soly_seed_set_100.csv")