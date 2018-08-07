#
##
###
#S. MELONGENA
###
##
#

#Taking out species that weren't used
some_seed_set <- read.csv("Data/species_seed_set/SOME_seed_set.csv", sep=";")
some_seed_set <- filter(some_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%" & Treatment!="COSA 50%" & Treatment!="COSA 100%")

#adding the focal species, first 50%
Species <- rep("SOME", 10)
Treatment <- rep("SOME 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
some_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("SOME", 10)
Treatment <- rep("SOME 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
some_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
some_seed_set<- some_seed_set[,-3]
colnames(some_seed_set)[3] <- "Treatment.number"
colnames(some_seed_set)[4] <- "Seed.production"
some_seed_set <- rbind(some_seed_set, some_50, some_100)
some_seed_set$Treatment<- as.character(some_seed_set$Treatment)
some_seed_set <- some_seed_set[order(some_seed_set$Treatment),]  

#Organizing with just 50% and cross, self, flower control and control on a side

library(stringr)
some_seed_set_div <- str_split_fixed(as.character(some_seed_set$Treatment), " ", 2)
some_seed_set_div <- data.frame(some_seed_set_div, stringsAsFactors = F)
colnames(some_seed_set_div)=c("non_focal","percentage")
some_seed_set_bind <- cbind(some_seed_set,some_seed_set_div)
some_seed_set_bind <- filter(some_seed_set_bind, percentage!="100%")

#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(some_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

some_seed_set_cross <- filter(some_seed_set_bind, non_focal %in% c("CROSS"))
some_seed_set_self <- filter(some_seed_set_bind, non_focal %in% c("SELF"))
some_seed_set_control <- filter(some_seed_set_bind, non_focal %in% c("CONTROL"))
some_seed_set_flower <- filter(some_seed_set_bind, non_focal %in% c("FLOWER"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
some_seed_set_bind=some_seed_set_bind[some_seed_set_bind$non_focal!=c("CROSS"),]
some_seed_set_bind=some_seed_set_bind[some_seed_set_bind$non_focal!=c("SELF"),]
some_seed_set_bind=some_seed_set_bind[some_seed_set_bind$non_focal!=c("CONTROL"),]
some_seed_set_bind=some_seed_set_bind[some_seed_set_bind$non_focal!=c("FLOWER"),]

some_seed_set_final<-rbind(some_seed_set_bind,some_seed_set_cross, some_seed_set_self, some_seed_set_control,
                           some_seed_set_flower)
#deleting extra columns used for data formating
some_seed_set_final=some_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
some_seed_set_final[some_seed_set_final$Treatment==c("SOME 50%"),4] <- NA

#changing non_focal species name
some_seed_set_final$Treatment=as.character(some_seed_set_final$Treatment)
some_seed_set_final$Family[some_seed_set_final$Treatment=="BROL 50%"] <- "Brassicaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="BRRA 50%"] <- "Brassicaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="SIAL 50%"] <- "Brassicaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="ERSA 50%"] <- "Brassicaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="SOME 50%"] <- "Solanaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="SOLY 50%"] <- "Solanaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="CAAN 50%"] <- "Solanaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="PEIN 50%"] <- "Solanaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="IPPU 50%"] <- "Convolvulaceae"
some_seed_set_final$Family[some_seed_set_final$Treatment=="IPAQ 50%"] <- "Convolvulaceae"

some_seed_set_final$Treatment[some_seed_set_final$Treatment=="BROL 50%"] <- "Brassica oleracea"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="BRRA 50%"] <- "Brassica rapa"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="CAAN 50%"] <- "Capsicum annuum"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="ERSA 50%"] <- "Eruca vesicaria" #Eruca sativa seems to be a synonym
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="IPAQ 50%"] <- "Ipomoea aquatica"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="IPPU 50%"] <- "Ipomoea purpurea"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="PEIN 50%"] <- "Petunia integrifolia"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="SIAL 50%"] <- "Sinapis alba"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="SOLY 50%"] <- "Solanum lycopersicum"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="SOME 50%"] <- "Solanum melongena"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="CROSS"] <- "Cross"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="SELF"] <- "Self"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="CONTROL"] <- "Control"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="FLOWER CONTROL"] <- "Flower control"

some_seed_set_brassicaceae <- filter(some_seed_set_final, Family %in% c("Brassicaceae"))
some_seed_set_convolvulaceae <- filter(some_seed_set_final, Family %in% c("Convolvulaceae"))
some_seed_set_solanaceae <- filter(some_seed_set_final, Family %in% c("Solanaceae"))
some_seed_set_final$Family[is.na(some_seed_set_final$Family)] <- "Solanum lycopersicum"
some_seed_set_cross=some_seed_set_cross[,-c(5,6)]
some_seed_set_cross$Family <- "other"
some_seed_set_self=some_seed_set_self[,-c(5,6)]
some_seed_set_self$Family <- "other"
some_seed_set_control=some_seed_set_control[,-c(5,6)]
some_seed_set_control$Family <- "other"
some_seed_set_flower=some_seed_set_flower[,-c(5,6)]
some_seed_set_flower$Family <- "other"

some_seed_set_final=rbind(some_seed_set_brassicaceae, some_seed_set_convolvulaceae, 
                          some_seed_set_solanaceae, some_seed_set_cross, some_seed_set_self, some_seed_set_control,
                          some_seed_set_flower)
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="CROSS"] <- "Cross"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="SELF"] <- "Self"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="CONTROL"] <- "Control"
some_seed_set_final$Treatment[some_seed_set_final$Treatment=="FLOWER CONTROL"] <- "Flower control"

write.csv(some_seed_set_final, "Rmd/Data/some_seed_set_final.csv")
cbPalette <- c( "#56B4E9","#E69F00", "#999999", "#009E73")
#Different colour per family
d <- ggplot(some_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot(outlier.shape = NA)+
  labs(title="Solanum melongena",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=cbPalette)+ geom_jitter(width = 0.3,shape=1,size=0.8, aes(colour=Family))+scale_color_manual(values = cbPalette) + stat_summary(fun.y=mean, geom="point", shape="*", size=5) +theme(legend.position="none")
d
#Different colour per species
p <- ggplot(some_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Solanum melongena",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#Different colour per family
p <- ggplot(some_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Solanum melongena",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#I think we kind of missing something with boxplots
#I'm going to sum seeds per Treatment to see if it improves the visualization of the differences
some_seed_set$Treatment<- as.character(some_seed_set$Treatment)
some_seed_set_sum <- dcast(factor(Treatment, levels=unique(Treatment))+Family~., value.var = "Seed.production", fun.aggregate = sum, data =some_seed_set_final , na.rm= TRUE)
colnames(some_seed_set_sum)<- c("Treatment","Family","Sum_seed")


p<-ggplot(some_seed_set_sum, aes(x=Treatment, y=Sum_seed)) +
  geom_bar(stat="identity")+ labs(title="Solanum melongena",x="", y = "Seeds")+aes(fill=Family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) 
p+theme(legend.position="none")
