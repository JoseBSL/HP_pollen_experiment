#
##
###
#B. RAPA
###
##
#


brra_seed_set <- read.csv("Data/species_seed_set/BRRA_seed_set.csv", sep=";")
brra_seed_set <- filter(brra_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%"& Treatment!="COSA 50%"& Treatment!="COSA 100%" )

#adding the focal species, first 50%
Species <- rep("BRRA", 10)
Treatment <- rep("BRRA 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
brra_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("BRRA", 10)
Treatment <- rep("BRRA 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
brra_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
colnames(brra_seed_set)[1] <- "Species"
colnames(brra_seed_set)[2] <- "Treatment"
colnames(brra_seed_set)[3] <- "Treatment.number"
colnames(brra_seed_set)[4] <- "Seed.production"
brra_seed_set <- rbind(brra_seed_set, brra_50, brra_100)
brra_seed_set$Treatment<- as.character(brra_seed_set$Treatment)
brra_seed_set <- brra_seed_set[order(brra_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
brra_seed_set_div <- str_split_fixed(as.character(brra_seed_set$Treatment), " ", 2)
brra_seed_set_div <- data.frame(brra_seed_set_div, stringsAsFactors = F)
colnames(brra_seed_set_div)=c("non_focal","percentage")
brra_seed_set_bind <- cbind(brra_seed_set,brra_seed_set_div)
brra_seed_set_bind <- filter(brra_seed_set_bind, percentage!="100%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(brra_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

brra_seed_set_cross <- filter(brra_seed_set_bind, non_focal %in% c("Cross"))
brra_seed_set_self <- filter(brra_seed_set_bind, non_focal %in% c("Self"))
brra_seed_set_control <- filter(brra_seed_set_bind, non_focal %in% c("Control"))
brra_seed_set_flower <- filter(brra_seed_set_bind, non_focal %in% c("Flower"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
brra_seed_set_bind=brra_seed_set_bind[brra_seed_set_bind$non_focal!=c("Cross"),]
brra_seed_set_bind=brra_seed_set_bind[brra_seed_set_bind$non_focal!=c("Self"),]
brra_seed_set_bind=brra_seed_set_bind[brra_seed_set_bind$non_focal!=c("Control"),]
brra_seed_set_bind=brra_seed_set_bind[brra_seed_set_bind$non_focal!=c("Flower"),]

brra_seed_set_final<-rbind(brra_seed_set_bind,brra_seed_set_cross, brra_seed_set_self, brra_seed_set_control,
                           brra_seed_set_flower)
#deleting extra columns used for data formating
brra_seed_set_final=brra_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
brra_seed_set_final[brra_seed_set_final$Treatment==c("BRRA 50%"),4] <- NA

#changing non_focal species name
brra_seed_set_final$Treatment=as.character(brra_seed_set_final$Treatment)
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="BROL 50%"] <- "Brassicaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="BRRA 50%"] <- "Brassicaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="SIAL 50%"] <- "Brassicaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="ERSA 50%"] <- "Brassicaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="CAAN 50%"] <- "Solanaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="SOLY 50%"] <- "Solanaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="SOME 50%"] <- "Solanaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="PEIN 50%"] <- "Solanaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="IPPU 50%"] <- "Convolvulaceae"
brra_seed_set_final$Family[brra_seed_set_final$Treatment=="IPAQ 50%"] <- "Convolvulaceae"

brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
brra_seed_set_final$Treatment[brra_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"

#Different colour per Treatment
p <- ggplot(brra_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Brassica rapa",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#Different colour per family
p <- ggplot(brra_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Brassica rapa",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#I think we kind of missing something with boxplots
#I'm going to sum seeds per Treatment to see if it improves the visualization of the differences
brra_seed_set$Treatment<- as.character(brra_seed_set$Treatment)
brra_seed_set_sum <- dcast(factor(Treatment, levels=unique(Treatment))+Family~., value.var = "Seed.production", fun.aggregate = sum, data =brra_seed_set_final , na.rm= TRUE)
colnames(brra_seed_set_sum)<- c("Treatment","Family","Sum_seed")


p<-ggplot(brra_seed_set_sum, aes(x=Treatment, y=Sum_seed)) +
  geom_bar(stat="identity")+ labs(title="Brassica rapa",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 
p+theme(legend.position="none")
