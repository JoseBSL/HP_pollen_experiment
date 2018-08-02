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
soly_seed_set_bind <- filter(soly_seed_set_bind, percentage!="100%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(soly_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

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
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="BROL 50%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="BRRA 50%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="SIAL 50%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="ERSA 50%"] <- "Brassicaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="CAAN 50%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="SOLY 50%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="SOME 50%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="PEIN 50%"] <- "Solanaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="IPPU 50%"] <- "Convolvulaceae"
soly_seed_set_final$Family[soly_seed_set_final$Treatment=="IPAQ 50%"] <- "Convolvulaceae"

soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
soly_seed_set_final$Treatment[soly_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"

#Different colour per Treatment
p <- ggplot(soly_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Solanum lycopersicum",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#Different colour per family
p <- ggplot(soly_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Solanum lycopersicum",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#I think we kind of missing something with boxplots
#I'm going to sum seeds per Treatment to see if it improves the visualization of the differences
soly_seed_set$Treatment<- as.character(soly_seed_set$Treatment)
soly_seed_set_sum <- dcast(factor(Treatment, levels=unique(Treatment))+Family~., value.var = "Seed.production", fun.aggregate = sum, data =caan_seed_set_final , na.rm= TRUE)
colnames(soly_seed_set_sum)<- c("Treatment","Family","Sum_seed")


p<-ggplot(soly_seed_set_sum, aes(x=Treatment, y=Sum_seed)) +
  geom_bar(stat="identity")+ labs(title="Solanum lycopersicum",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 
p+theme(legend.position="none")
