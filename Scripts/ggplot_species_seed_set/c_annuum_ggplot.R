#
##
###
#C. ANNUUM 
###
##
#
caan_seed_set <- read.csv("Data/species_seed_set/CAAN_seed_set.csv", sep=";")

#adding the focal species, first 50%
Species <- rep("CAAN", 10)
Treatment <- rep("CAAN 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
caan_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("CAAN", 10)
Treatment <- rep("CAAN 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
caan_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
caan_seed_set <- caan_seed_set[,-4]
colnames(caan_seed_set)[1] <- "Species"
colnames(caan_seed_set)[2] <- "Treatment"
colnames(caan_seed_set)[3] <- "Treatment.number"
colnames(caan_seed_set)[4] <- "Seed.production"
caan_seed_set <- rbind(caan_seed_set, caan_50, caan_100)
caan_seed_set$Treatment<- as.character(caan_seed_set$Treatment)
caan_seed_set <- caan_seed_set[order(caan_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
caan_seed_set_div <- str_split_fixed(as.character(caan_seed_set$Treatment), " ", 2)
caan_seed_set_div <- data.frame(caan_seed_set_div, stringsAsFactors = F)
colnames(caan_seed_set_div)=c("non_focal","percentage")
caan_seed_set_bind <- cbind(caan_seed_set,caan_seed_set_div)
caan_seed_set_bind <- filter(caan_seed_set_bind, percentage!="100%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(caan_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

caan_seed_set_cross <- filter(caan_seed_set_bind, non_focal %in% c("CROSS"))
caan_seed_set_self <- filter(caan_seed_set_bind, non_focal %in% c("SELF"))
caan_seed_set_control <- filter(caan_seed_set_bind, non_focal %in% c("CONTROL"))
caan_seed_set_flower <- filter(caan_seed_set_bind, non_focal %in% c("FLOWER"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
caan_seed_set_bind=caan_seed_set_bind[caan_seed_set_bind$non_focal!=c("CROSS"),]
caan_seed_set_bind=caan_seed_set_bind[caan_seed_set_bind$non_focal!=c("SELF"),]
caan_seed_set_bind=caan_seed_set_bind[caan_seed_set_bind$non_focal!=c("CONTROL"),]
caan_seed_set_bind=caan_seed_set_bind[caan_seed_set_bind$non_focal!=c("FLOWER"),]

caan_seed_set_final<-rbind(caan_seed_set_bind,caan_seed_set_cross, caan_seed_set_self, caan_seed_set_control,
                           caan_seed_set_flower)
#deleting extra columns used for data formating
caan_seed_set_final=caan_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
caan_seed_set_final[caan_seed_set_final$Treatment==c("CAAN 50%"),4] <- NA

#changing non_focal species name
caan_seed_set_final$Treatment=as.character(caan_seed_set_final$Treatment)
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="BROL 50%"] <- "Brassicaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="BRRA 50%"] <- "Brassicaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="SIAL 50%"] <- "Brassicaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="ERSA 50%"] <- "Brassicaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="caan 50%"] <- "Solanaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="SOLY 50%"] <- "Solanaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="CAAN 50%"] <- "Solanaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="PEIN 50%"] <- "Solanaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="IPPU 50%"] <- "Convolvulaceae"
caan_seed_set_final$Family[caan_seed_set_final$Treatment=="IPAQ 50%"] <- "Convolvulaceae"

caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
caan_seed_set_final$Treatment[caan_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"



#Different colour per treatment
p <- ggplot(caan_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Capsicum annuum",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#Different colour per family
p <- ggplot(caan_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Capsicum annuum",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#I think we kind of missing something with boxplots
#I'm going to sum seeds per Treatment to see if it improves the visualization of the differences
caan_seed_set$Treatment<- as.character(caan_seed_set$Treatment)
caan_seed_set_sum <- dcast(factor(Treatment, levels=unique(Treatment))+Family~., value.var = "Seed.production", fun.aggregate = sum, data =caan_seed_set_final , na.rm= TRUE)
colnames(caan_seed_set_sum)<- c("Treatment","Family","Sum_seed")


p<-ggplot(caan_seed_set_sum, aes(x=Treatment, y=Sum_seed)) +
  geom_bar(stat="identity")+ labs(title="Capsicum annuum",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 
p+theme(legend.position="none")
