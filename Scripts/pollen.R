library(ggplot2)
library(forcats)
library(Hmisc)
library(Formula)


p <- read.csv("data/Pollen.csv", sep = ";")

#Deleting extra row (21)
p <-p[-21,]

boxplot(p)
#Reorganize data for ggplot
p<-stack(p)
#colnames
colnames(p)[1]<-"pollen"
colnames(p)[2]<-"species"

all<-p
solanaceae<-subset(p)[1:100,]
brassicaceae<- subset(p)[101:180,]

#Solanaceae

p <- ggplot(solanaceae, aes(x = reorder(species, pollen, FUN = median), y = pollen)) +   geom_boxplot()+
  labs(title="Solanaceae",x="", y = "pollen")+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black")


#Brassicaceae

p <- ggplot(brassicaceae, aes(x = reorder(species, pollen, FUN = median), y = pollen)) +   geom_boxplot()+
  labs(title="Brassicaceae",x="", y = "pollen")+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black")

#all

p <- ggplot(all, aes(x = reorder(species, pollen, FUN = median), y = pollen)) +   geom_boxplot()+
  labs(title="All species",x="", y = "pollen")+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black")

