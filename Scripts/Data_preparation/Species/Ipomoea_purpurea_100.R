#
##
###
#I. IPPU
###
##
#


ippu_seed_set <- read.csv("Data/species_seed_set/ippu_seed_set.csv", sep=";")
colnames(ippu_seed_set)[1] <- "Species"
colnames(ippu_seed_set)[2] <- "Treatment"
colnames(ippu_seed_set)[3] <- "Treatment.number"
colnames(ippu_seed_set)[4] <- "Seed.production"

ippu_seed_set <- filter(ippu_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%"& Treatment!="COSA 50%"& Treatment!="COSA 100%" )

#adding the focal species, first 50%
Species <- rep("IPPU", 10)
Treatment <- rep("IPPU 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
ippu_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("IPPU", 10)
Treatment <- rep("IPPU 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
ippu_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely

ippu_seed_set <- rbind(ippu_seed_set, ippu_50, ippu_100)
ippu_seed_set$Treatment<- as.character(ippu_seed_set$Treatment)
ippu_seed_set <- ippu_seed_set[order(ippu_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
ippu_seed_set_div <- str_split_fixed(as.character(ippu_seed_set$Treatment), " ", 2)
ippu_seed_set_div <- data.frame(ippu_seed_set_div, stringsAsFactors = F)
colnames(ippu_seed_set_div)=c("non_focal","percentage")
ippu_seed_set_bind <- cbind(ippu_seed_set,ippu_seed_set_div)
ippu_seed_set_bind <- filter(ippu_seed_set_bind, percentage!="50%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(ippu_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

ippu_seed_set_cross <- filter(ippu_seed_set_bind, Treatment %in% c("cross"))
ippu_seed_set_self <- filter(ippu_seed_set_bind, Treatment %in% c("self"))
ippu_seed_set_control <- filter(ippu_seed_set_bind, Treatment %in% c("control"))
ippu_seed_set_flower <- filter(ippu_seed_set_bind, Treatment %in% c("flower control"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
ippu_seed_set_bind=ippu_seed_set_bind[ippu_seed_set_bind$Treatment!=c("cross"),]
ippu_seed_set_bind=ippu_seed_set_bind[ippu_seed_set_bind$Treatment!=c("self"),]
ippu_seed_set_bind=ippu_seed_set_bind[ippu_seed_set_bind$Treatment!=c("control"),]
ippu_seed_set_bind=ippu_seed_set_bind[ippu_seed_set_bind$Treatment!=c("flower control"),]

ippu_seed_set_final<-rbind(ippu_seed_set_bind,ippu_seed_set_cross, ippu_seed_set_self, ippu_seed_set_control,
                           ippu_seed_set_flower)
#deleting extra columns used for data formating
ippu_seed_set_final=ippu_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
ippu_seed_set_final[ippu_seed_set_final$Treatment==c("IPPU 50%"),4] <- NA

#changing non_focal species name
ippu_seed_set_final$Treatment=as.character(ippu_seed_set_final$Treatment)
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="BROL 100%"] <- "Brassicaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="BRRA 100%"] <- "Brassicaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="IPPU 100%"] <- "Brassicaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="ERSA 100%"] <- "Brassicaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="CAAN 100%"] <- "Solanaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="SOLY 100%"] <- "Solanaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="SOME 100%"] <- "Solanaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="PEIN 100%"] <- "Solanaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="IPPU 100%"] <- "Convolvulaceae"
ippu_seed_set_final$Family[ippu_seed_set_final$Treatment=="IPAQ 100%"] <- "Convolvulaceae"

#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
#ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"

ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="cross"] <- "Cross"
ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="self"] <- "Self"
ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="control"] <- "Control"
ippu_seed_set_final$Treatment[ippu_seed_set_final$Treatment=="flower control"] <- "Flower control"


write.csv(ippu_seed_set_final, "Rmd/Data/ippu_seed_set_100.csv")