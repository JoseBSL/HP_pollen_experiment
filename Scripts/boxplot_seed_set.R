#Preparing seed set plots, starting with Petunia

#There are 3 crosses were seeds were lost (2,7,9)
#load libraries
library(reshape2)
library(ggplot2)



#read data and cleaning
pein_seed_set <- read.csv("Data/species_seed_set/PEIN_seed_set.csv", sep=";")
pein_seed_set <- pein_seed_set[-c(42,47,49),]
some_seed_set <- read.csv("Data/species_seed_set/SOME_seed_set.csv", sep=";")

#
##
###
#PETUNIA
###
##
#


#Removing RARA, species not considered because sterility 
library(dplyr)
pein_seed_set <- filter(pein_seed_set, Treatment!="RARA 100%" & Treatment!="RARA 50%")

#adding the focal species, first 50%
Species <- rep("PEIN", 10)
Treatment <- rep("PEIN 50%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
pein_50 <- data.frame(Species,Treatment, Treatment.number, Seed.production)
#adding the focal species, 100%
Species <- rep("PEIN", 10)
Treatment <- rep("PEIN 100%", 10)
Treatment.number <- seq(1:10)
Seed.production <- rep(0, 10)
pein_100<-  data.frame(Species,Treatment, Treatment.number, Seed.production)
#Order from lower to higher values the average, to plot it nicely
pein_seed_set <- rbind(pein_seed_set, pein_50, pein_100)

pein_seed_set <- pein_seed_set[order(pein_seed_set$Treatment),]  


p <- ggplot(pein_seed_set, aes(x = Treatment, y = Seed.production)) +   geom_boxplot()+
  labs(title="Petunia integrifolia",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#
##
###
#S. MELONGENA
###
##
#

#Taking out species that weren't used

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
some_seed_set <- rbind(some_seed_set, some_50, some_100)
some_seed_set <- some_seed_set[order(some_seed_set$Treatment),]  

colnames(some_seed_set)[5] <- "Seed.production"


p <- ggplot(some_seed_set, aes(x = Treatment, y = Seed.production)) +   geom_boxplot()+
  labs(title="Solanum melongena",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


#
##
###
#C. ANNUUM 
###
##
#


