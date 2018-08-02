#
##
###
#S. ALBA  
###
##
#


sial_seed_set <- read.csv("Data/species_seed_set/sial_seed_set.csv", sep=";")
sial_seed_set <- filter(sial_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%"& Treatment!="COSA 50%"& Treatment!="COSA 100%" )

#adding the focal species, first 50%
Species <- rep("SIAL", 10)
Treatment <- rep("SIAL 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
sial_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("SIAL", 10)
Treatment <- rep("SIAL 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
sial_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
colnames(sial_seed_set)[1] <- "Species"
colnames(sial_seed_set)[2] <- "Treatment"
colnames(sial_seed_set)[3] <- "Treatment.number"
colnames(sial_seed_set)[4] <- "Seed.production"
sial_seed_set <- rbind(sial_seed_set, sial_50, sial_100)
sial_seed_set$Treatment<- as.character(sial_seed_set$Treatment)
sial_seed_set <- sial_seed_set[order(sial_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
sial_seed_set_div <- str_split_fixed(as.character(sial_seed_set$Treatment), " ", 2)
sial_seed_set_div <- data.frame(sial_seed_set_div, stringsAsFactors = F)
colnames(sial_seed_set_div)=c("non_focal","percentage")
sial_seed_set_bind <- cbind(sial_seed_set,sial_seed_set_div)
sial_seed_set_bind <- filter(sial_seed_set_bind, percentage!="100%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(sial_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

sial_seed_set_cross <- filter(sial_seed_set_bind, non_focal %in% c("Cross"))
sial_seed_set_self <- filter(sial_seed_set_bind, non_focal %in% c("Self"))
sial_seed_set_control <- filter(sial_seed_set_bind, non_focal %in% c("Control"))
sial_seed_set_flower <- filter(sial_seed_set_bind, non_focal %in% c("FC"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
sial_seed_set_bind=sial_seed_set_bind[sial_seed_set_bind$non_focal!=c("Cross"),]
sial_seed_set_bind=sial_seed_set_bind[sial_seed_set_bind$non_focal!=c("Self"),]
sial_seed_set_bind=sial_seed_set_bind[sial_seed_set_bind$non_focal!=c("Control"),]
sial_seed_set_bind=sial_seed_set_bind[sial_seed_set_bind$non_focal!=c("FC"),]

sial_seed_set_final<-rbind(sial_seed_set_bind,sial_seed_set_cross, sial_seed_set_self, sial_seed_set_control,
                           sial_seed_set_flower)
#deleting extra columns used for data formating
sial_seed_set_final=sial_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
sial_seed_set_final[sial_seed_set_final$Treatment==c("SIAL 50%"),4] <- NA

#changing non_focal species name
sial_seed_set_final$Treatment=as.character(sial_seed_set_final$Treatment)
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="BROL 50%"] <- "Brassicaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="BRRA 50%"] <- "Brassicaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="SIAL 50%"] <- "Brassicaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="ERSA 50%"] <- "Brassicaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="CAAN 50%"] <- "Solanaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="SOLY 50%"] <- "Solanaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="SOME 50%"] <- "Solanaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="PEIN 50%"] <- "Solanaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="IPPU 50%"] <- "Convolvulaceae"
sial_seed_set_final$Family[sial_seed_set_final$Treatment=="IPAQ 50%"] <- "Convolvulaceae"

sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
sial_seed_set_final$Treatment[sial_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"

#Different colour per Treatment
p <- ggplot(sial_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Sinapis alba",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#Different colour per family
p <- ggplot(sial_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Sinapis alba",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#I think we kind of missing something with boxplots
#I'm going to sum seeds per Treatment to see if it improves the visualization of the differences
sial_seed_set$Treatment<- as.character(sial_seed_set$Treatment)
sial_seed_set_sum <- dcast(factor(Treatment, levels=unique(Treatment))+Family~., value.var = "Seed.production", fun.aggregate = sum, data =sial_seed_set_final , na.rm= TRUE)
colnames(sial_seed_set_sum)<- c("Treatment","Family","Sum_seed")


p<-ggplot(sial_seed_set_sum, aes(x=Treatment, y=Sum_seed)) +
  geom_bar(stat="identity")+ labs(title="Sinapis alba",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 
p+theme(legend.position="none")
