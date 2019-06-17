library(ggplot2)
library(forcats)
library(Hmisc)
library(Formula)


p <- read.csv("data/Pollen.csv", sep = ";")

#Adding common names
colnames(p) <- c("Capsicum", "Petunia", "Tomato", "Tomato_buzz", "Eggplant",
                 "Wild cabbage", "White mustard", "Pak choi", "Rocket", "Morning glory",
               "Water morning glory")
#Removing Pollen count of tomato buzzed
p<- p[,-4]
#boxplot(p)
#Reorganize data for ggplot
p<-stack(p)
p$species <- as.character(p$species)
#colnames
colnames(p)[1]<-"pollen"
colnames(p)[2]<-"species"
#Adding family to put colour per family
p$family[p$species=="Capsicum"] <- "Solanaceae"
p$family[p$species=="Tomato"] <- "Solanaceae"
p$family[p$species=="Petunia"] <- "Solanaceae"
p$family[p$species=="Eggplant"] <- "Solanaceae"
p$family[p$species=="Wild cabbage"] <- "Brassicaceae"
p$family[p$species=="Pak choi"] <- "Brassicaceae"
p$family[p$species=="Rocket"] <- "Brassicaceae"
p$family[p$species=="White mustard"] <- "Brassicaceae"
p$family[p$species=="Morning glory"] <- "Convolvulaceae"
p$family[p$species=="Water morning glory"] <- "Convolvulaceae"













all<-p
solanaceae<-subset(p)[1:100,]
brassicaceae<- subset(p)[101:180,]

write.csv(all, "Rmd/Data/all.csv")

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



library(ggplot2)


p <- read.csv("data/Pollen.csv", sep = ";")



#boxplot(p)
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
  labs(title="Solanaceae",x="", y = "pollen")+aes(fill=species)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")

#Brassicaceae
p <- ggplot(brassicaceae, aes(x = reorder(species, pollen, FUN = median), y = pollen)) +   geom_boxplot()+
  labs(title="Brassicaceae",x="", y = "pollen")+aes(fill=species)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black") +theme(legend.position="none")


cbPalette <- c( "#56B4E9","#E69F00", "#999999", "#009E73")

cbPalette <- c( "#56B4E9","#E69F00","#009E73")

#all

 ggplot(all, aes(x = reorder(species, pollen, FUN = median, na.rm = TRUE), y = pollen)) +   geom_boxplot(outlier.shape = NA)+
  labs(title="All species",x="", y = "Pollen per anther")+aes(fill=family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=cbPalette)+ geom_jitter(width = 0.3,shape=1,size=0.8, aes(colour=family))+scale_color_manual(values = cbPalette) + stat_summary(fun.y=mean, geom="point", shape="*", size=5) +theme(legend.position="none")
