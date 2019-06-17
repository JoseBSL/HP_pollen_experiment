#
##
###
#E. VERSICARIA
###
##
#


ersa_seed_set <- read.csv("Data/species_seed_set/ersa_seed_set.csv", sep=";")
ersa_seed_set <- filter(ersa_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%"& Treatment!="COSA 50%"& Treatment!="COSA 100%" )

#adding the focal species, first 50%
Species <- rep("ERSA", 10)
Treatment <- rep("ERSA 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
ersa_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("ERSA", 10)
Treatment <- rep("ERSA 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
ersa_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
colnames(ersa_seed_set)[1] <- "Species"
colnames(ersa_seed_set)[2] <- "Treatment"
colnames(ersa_seed_set)[3] <- "Treatment.number"
colnames(ersa_seed_set)[4] <- "Seed.production"
ersa_seed_set <- rbind(ersa_seed_set, ersa_50, ersa_100)
ersa_seed_set$Treatment<- as.character(ersa_seed_set$Treatment)
ersa_seed_set <- ersa_seed_set[order(ersa_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
ersa_seed_set_div <- str_split_fixed(as.character(ersa_seed_set$Treatment), " ", 2)
ersa_seed_set_div <- data.frame(ersa_seed_set_div, stringsAsFactors = F)
colnames(ersa_seed_set_div)=c("non_focal","percentage")
ersa_seed_set_bind <- cbind(ersa_seed_set,ersa_seed_set_div)
ersa_seed_set_bind <- filter(ersa_seed_set_bind, percentage!="100%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(ersa_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

ersa_seed_set_cross <- filter(ersa_seed_set_bind, non_focal %in% c("Cross"))
ersa_seed_set_self <- filter(ersa_seed_set_bind, non_focal %in% c("Self"))
ersa_seed_set_control <- filter(ersa_seed_set_bind, non_focal %in% c("Control"))
ersa_seed_set_flower <- filter(ersa_seed_set_bind, non_focal %in% c("Flower"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
ersa_seed_set_bind=ersa_seed_set_bind[ersa_seed_set_bind$non_focal!=c("Cross"),]
ersa_seed_set_bind=ersa_seed_set_bind[ersa_seed_set_bind$non_focal!=c("Self"),]
ersa_seed_set_bind=ersa_seed_set_bind[ersa_seed_set_bind$non_focal!=c("Control"),]
ersa_seed_set_bind=ersa_seed_set_bind[ersa_seed_set_bind$non_focal!=c("Flower"),]

ersa_seed_set_final<-rbind(ersa_seed_set_bind,ersa_seed_set_cross, ersa_seed_set_self, ersa_seed_set_control,
                           ersa_seed_set_flower)
#deleting extra columns used for data formating
ersa_seed_set_final=ersa_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
ersa_seed_set_final[ersa_seed_set_final$Treatment==c("ERSA 50%"),4] <- NA

#changing non_focal species name
ersa_seed_set_final$Treatment=as.character(ersa_seed_set_final$Treatment)
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="BROL 50%"] <- "Brassicaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="BRRA 50%"] <- "Brassicaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="SIAL 50%"] <- "Brassicaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="ERSA 50%"] <- "Brassicaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="CAAN 50%"] <- "Solanaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="SOLY 50%"] <- "Solanaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="SOME 50%"] <- "Solanaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="PEIN 50%"] <- "Solanaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="IPPU 50%"] <- "Convolvulaceae"
ersa_seed_set_final$Family[ersa_seed_set_final$Treatment=="IPAQ 50%"] <- "Convolvulaceae"

ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"
ersa_seed_set_final$Treatment[ersa_seed_set_final$Treatment=="FC"] <- "Flower control"


ersa_seed_set_brassicaceae <- filter(ersa_seed_set_final, Family %in% c("Brassicaceae"))
ersa_seed_set_convolvulaceae <- filter(ersa_seed_set_final, Family %in% c("Convolvulaceae"))
ersa_seed_set_solanaceae <- filter(ersa_seed_set_final, Family %in% c("Solanaceae"))
ersa_seed_set_final$Family[is.na(ersa_seed_set_final$Family)] <- "Solanum lycopersicum"

ersa_seed_set_cross=ersa_seed_set_cross[,-c(5,6)]
ersa_seed_set_cross$Family <- "other"
ersa_seed_set_self=ersa_seed_set_self[,-c(5,6)]
ersa_seed_set_self$Family <- "other"
ersa_seed_set_control=ersa_seed_set_control[,-c(5,6)]
ersa_seed_set_control$Family <- "other"
ersa_seed_set_flower=ersa_seed_set_flower[,-c(5,6)]
ersa_seed_set_flower$Family <- "other"

ersa_seed_set_final=rbind(ersa_seed_set_brassicaceae, ersa_seed_set_convolvulaceae, 
                          ersa_seed_set_solanaceae, ersa_seed_set_cross, ersa_seed_set_self, ersa_seed_set_control,
                          ersa_seed_set_flower)


write.csv(ersa_seed_set_final, "Rmd/Data/ersa_seed_set_final.csv")



p <- ggplot(ersa_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Eruca versicaria",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#Different colour per family
p <- ggplot(ersa_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Eruca versicaria",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#I think we kind of missing something with boxplots
#I'm going to sum seeds per Treatment to see if it improves the visualization of the differences
ersa_seed_set$Treatment<- as.character(ersa_seed_set$Treatment)
ersa_seed_set_sum <- dcast(factor(Treatment, levels=unique(Treatment))+Family~., value.var = "Seed.production", fun.aggregate = sum, data =ersa_seed_set_final , na.rm= TRUE)
colnames(ersa_seed_set_sum)<- c("Treatment","Family","Sum_seed")


p<-ggplot(ersa_seed_set_sum, aes(x=Treatment, y=Sum_seed)) +
  geom_bar(stat="identity")+ labs(title="Eruca versicaria",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 
p+theme(legend.position="none")
