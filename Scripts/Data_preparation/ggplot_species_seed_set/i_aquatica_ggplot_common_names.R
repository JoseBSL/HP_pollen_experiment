#
##
###
#I. AQUATICA
###
##
#

library(dplyr)
library(ggplot2)
library(reshape2)


ipaq_seed_set <- read.csv("Data/species_seed_set/ipaq_seed_set.csv", sep=";")
colnames(ipaq_seed_set)[1] <- "Species"
colnames(ipaq_seed_set)[2] <- "Treatment"
colnames(ipaq_seed_set)[3] <- "Treatment.number"
colnames(ipaq_seed_set)[4] <- "Seed.production"

ipaq_seed_set <- filter(ipaq_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%"& Treatment!="COSA 50%"& Treatment!="COSA 100%" )

#adding the focal species, first 50%
Species <- rep("IPAQ", 10)
Treatment <- rep("IPAQ 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
ipaq_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("IPAQ", 10)
Treatment <- rep("IPAQ 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
ipaq_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely

ipaq_seed_set <- rbind(ipaq_seed_set, ipaq_50, ipaq_100)
ipaq_seed_set$Treatment<- as.character(ipaq_seed_set$Treatment)
ipaq_seed_set <- ipaq_seed_set[order(ipaq_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
ipaq_seed_set_div <- str_split_fixed(as.character(ipaq_seed_set$Treatment), " ", 2)
ipaq_seed_set_div <- data.frame(ipaq_seed_set_div, stringsAsFactors = F)
colnames(ipaq_seed_set_div)=c("non_focal","percentage")
ipaq_seed_set_bind <- cbind(ipaq_seed_set,ipaq_seed_set_div)
ipaq_seed_set_bind <- filter(ipaq_seed_set_bind, percentage!="100%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(ipaq_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

ipaq_seed_set_cross <- filter(ipaq_seed_set_bind, Treatment %in% c("cross"))
ipaq_seed_set_self <- filter(ipaq_seed_set_bind, Treatment %in% c("self"))
ipaq_seed_set_control <- filter(ipaq_seed_set_bind, Treatment %in% c("control"))
ipaq_seed_set_flower <- filter(ipaq_seed_set_bind, Treatment %in% c("flower control"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
ipaq_seed_set_bind=ipaq_seed_set_bind[ipaq_seed_set_bind$Treatment!=c("cross"),]
ipaq_seed_set_bind=ipaq_seed_set_bind[ipaq_seed_set_bind$Treatment!=c("self"),]
ipaq_seed_set_bind=ipaq_seed_set_bind[ipaq_seed_set_bind$Treatment!=c("control"),]
ipaq_seed_set_bind=ipaq_seed_set_bind[ipaq_seed_set_bind$Treatment!=c("flower control"),]

ipaq_seed_set_final<-rbind(ipaq_seed_set_bind,ipaq_seed_set_cross, ipaq_seed_set_self, ipaq_seed_set_control,
                           ipaq_seed_set_flower)
#deleting extra columns used for data formating
ipaq_seed_set_final=ipaq_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
ipaq_seed_set_final[ipaq_seed_set_final$Treatment==c("IPPA 50%"),4] <- NA

#changing non_focal species name
ipaq_seed_set_final$Treatment=as.character(ipaq_seed_set_final$Treatment)
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="BROL 50%"] <- "Brassicaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="BRRA 50%"] <- "Brassicaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="SIAL 50%"] <- "Brassicaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="ERSA 50%"] <- "Brassicaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="CAAN 50%"] <- "Solanaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="SOLY 50%"] <- "Solanaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="SOME 50%"] <- "Solanaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="PEIN 50%"] <- "Solanaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="IPPU 50%"] <- "Convolvulaceae"
ipaq_seed_set_final$Family[ipaq_seed_set_final$Treatment=="IPAQ 50%"] <- "Convolvulaceae"

ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="BROL 50%"] <- "Wild cabbage"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="BRRA 50%"] <- "Pak choi"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="ERSA 50%"] <- "Rocket" #Eruca sativa seems to be a synonym
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="IPAQ 50%"] <- "Water morning glory"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="IPPU 50%"] <- "Morning glory"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="SIAL 50%"] <- "White mustard"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="SOLY 50%"] <- "Tomato"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="SOME 50%"] <- "Eggplant"

ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="cross"] <- "Cross"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="self"] <- "Self"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="control"] <- "Control"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="flower control"] <- "Flower control"

ipaq_seed_set_brassicaceae <- filter(ipaq_seed_set_final, Family %in% c("Brassicaceae"))
ipaq_seed_set_convolvulaceae <- filter(ipaq_seed_set_final, Family %in% c("Convolvulaceae"))
ipaq_seed_set_solanaceae <- filter(ipaq_seed_set_final, Family %in% c("Solanaceae"))
ipaq_seed_set_final$Family[is.na(ipaq_seed_set_final$Family)] <- "Solanum lycopersicum"

ipaq_seed_set_cross=ipaq_seed_set_cross[,-c(5,6)]
ipaq_seed_set_cross$Family <- "other"
ipaq_seed_set_self=ipaq_seed_set_self[,-c(5,6)]
ipaq_seed_set_self$Family <- "other"
ipaq_seed_set_control=ipaq_seed_set_control[,-c(5,6)]
ipaq_seed_set_control$Family <- "other"
ipaq_seed_set_flower=ipaq_seed_set_flower[,-c(5,6)]
ipaq_seed_set_flower$Family <- "other"

ipaq_seed_set_final=rbind(ipaq_seed_set_brassicaceae, ipaq_seed_set_convolvulaceae, 
                          ipaq_seed_set_solanaceae, ipaq_seed_set_cross, ipaq_seed_set_self, ipaq_seed_set_control,
                          ipaq_seed_set_flower)

ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="flower control"] <- "Flower control"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="control"] <- "Control"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="self"] <- "Self"
ipaq_seed_set_final$Treatment[ipaq_seed_set_final$Treatment=="cross"] <- "Cross"


write.csv(ipaq_seed_set_final, "Rmd/Data/ipaq_seed_set_final_common_names.csv")

#Different colour per Treatment
p <- ggplot(ipaq_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Ipomea aquatica",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#Different colour per family
p <- ggplot(ipaq_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Ipomea aquatica",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#I think we kind of missing something with boxplots
#I'm going to sum seeds per Treatment to see if it improves the visualization of the differences
ipaq_seed_set$Treatment<- as.character(ipaq_seed_set$Treatment)
ipaq_seed_set_sum <- dcast(factor(Treatment, levels=unique(Treatment))+Family~., value.var = "Seed.production", fun.aggregate = sum, data =ipaq_seed_set_final , na.rm= TRUE)
colnames(ipaq_seed_set_sum)<- c("Treatment","Family","Sum_seed")


p<-ggplot(ipaq_seed_set_sum, aes(x=factor(Treatment, levels=unique(Treatment)), y=Sum_seed)) +
  geom_bar(stat="identity")+ labs(title="Ipomea aquatica",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 
p+theme(legend.position="none")

