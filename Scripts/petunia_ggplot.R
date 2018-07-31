#Preparing seed set plots, starting with Petunia

#There are 3 crosses were seeds were lost (2,7,9)
#load libraries
library(reshape2)
library(ggplot2)
library(dplyr)

#read data and cleaning
pein_seed_set <- read.csv("Data/species_seed_set/PEIN_seed_set.csv", sep=";")
#removing samples where seeds got lost, first ones of the experiment
pein_seed_set <- pein_seed_set[-c(42,47,49),]

#
##
###
#PETUNIA
###
##
#
#Removing RARA, species not considered because sterility 

pein_seed_set <- filter(pein_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%")

#adding the focal species, first 50%
Species <- rep("PEIN", 10)
Treatment <- rep("PEIN 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
pein_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production,stringsAsFactors = F)
#adding the focal species, 100%
Species <- rep("PEIN", 10)
Treatment <- rep("PEIN 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
pein_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production,stringsAsFactors = F)
#Order from lower to higher values the average, to plot it nicely
pein_seed_set <- rbind(pein_seed_set, pein_50, pein_100)
pein_seed_set$Treatment<- as.character(pein_seed_set$Treatment)
pein_seed_set <- pein_seed_set[order(pein_seed_set$Treatment),]  
#Organizing with just 50% and cross, self, flower control and control on a side


library(stringr)
pein_seed_set_div <- str_split_fixed(as.character(pein_seed_set$Treatment), " ", 2)
pein_seed_set_div <- data.frame(pein_seed_set_div, stringsAsFactors = F)
colnames(pein_seed_set_div)=c("non_focal","percentage")
pein_seed_set_bind <- cbind(pein_seed_set,pein_seed_set_div)
pein_seed_set_bind <- filter(pein_seed_set_bind, percentage!="100%")
#Because I want to give it specifically order I do it separately
#pein_seed_set_common <- filter(pein_seed_set_bind, non_focal %in% c("CROSS", "SELF","CONTROL","FLOWER"))

pein_seed_set_cross <- filter(pein_seed_set_bind, non_focal %in% c("CROSS"))
pein_seed_set_self <- filter(pein_seed_set_bind, non_focal %in% c("SELF"))
pein_seed_set_control <- filter(pein_seed_set_bind, non_focal %in% c("CONTROL"))
pein_seed_set_flower <- filter(pein_seed_set_bind, non_focal %in% c("FLOWER"))

#Changing 0 for NA'S FOCAL species




# I HAVENT BEEN ABLE TO WORK OUT ALL AT THE SAME TIME SO i DO IOT SEPARATELY
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("CROSS"),]
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("SELF"),]
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("CONTROL"),]
pein_seed_set_bind=pein_seed_set_bind[pein_seed_set_bind$non_focal!=c("FLOWER"),]

pein_seed_set_final<-rbind(pein_seed_set_bind,pein_seed_set_cross, pein_seed_set_self, pein_seed_set_control,
                           pein_seed_set_flower)
#deleting extra columns used for data formating
pein_seed_set_final=pein_seed_set_final[,-c(5,6)]
#adding NA'S to the focal species
pein_seed_set_final[pein_seed_set_final$Treatment==c("PEIN 50%"),4] <- NA
#changing non_focal species name
pein_seed_set_final$Family <- NA
pein_seed_set_final[pein_seed_set_final$Treatment==c("BROL 50%", "BRRA 50%"), 5] <- "a"


#locking factor level to maintain order
p <- ggplot(pein_seed_set_final, aes(x = factor(Treatment, levels=unique(Treatment)), y = Seed.production)) +   geom_boxplot()+
  labs(title="Petunia integrifolia",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) + geom_jitter(width = 0.2)

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")
