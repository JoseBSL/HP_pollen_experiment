#Preparing seed set plots, starting with Petunia

#There are 3 crosses were seeds were lost (2,7,9)
#load libraries
library(reshape2)
library(ggplot2)
library(dplyr)



#read data and cleaning
pein_seed_set <- read.csv("Data/species_seed_set/PEIN_seed_set.csv", sep=";")
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


p <- ggplot(caan_seed_set, aes(x = Treatment, y = Seed.production)) +   geom_boxplot()+
  labs(title="Capsicum annuum",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

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


p <- ggplot(soly_seed_set, aes(x = Treatment, y = Seed.production)) +   geom_boxplot()+
  labs(title="Solanum lycopersicum",x="", y = "Seeds")+aes(fill=Treatment)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")
