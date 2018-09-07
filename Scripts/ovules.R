#load libraries

library(ggplot2)
library(forcats)
library(Hmisc)
library(Formula)
#load data
d<-read.csv("Data/Ovules.csv", sep=";")
d<- d[,-c(9,11)]
colnames(d)[1]<-"Capsicum"
colnames(d)[2]<-"Tomato"
colnames(d)[3]<-"Eggplant"
colnames(d)[4]<-"Petunia"
colnames(d)[5]<-"Wild cabagge"
colnames(d)[6]<-"Pak choi"
colnames(d)[7]<-"Rocket"
colnames(d)[8]<-"White mustard"
colnames(d)[9]<-"Water morning glory"
colnames(d)[10]<-"Morning glory"


#Reorganise data
d <- stack(d)
#Naming columns
colnames(d)[1]<-"ovules"
colnames(d)[2]<-"species"

family<- c(rep("solanaceae",60),rep("brassicaceae",60),rep("convolvulaceae",30))

d[,"family"] <- family

write.csv(d, "Rmd/Data/d.csv")

#boxplot from ggplot2
ggplot(d, aes(x = species, y = ovules)) + geom_boxplot()

#boxplot from ggplot2 reorder
ggplot(d, aes(x = reorder(species, ovules, FUN = median), y = ovules)) + geom_boxplot()

#subset per family (solanaceae, brassicaceae, convolvulaceae)
solanaceae<-subset(d)[1:60,]
brassicaceae<-subset(d)[61:120,]

# OVULES SOLANACEAE
ggplot(solanaceae, aes(x = species, y = ovules)) + geom_boxplot()

ggplot(solanaceae, aes(x = reorder(species, ovules, FUN = median), y = ovules)) +   geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

p <- ggplot(solanaceae, aes(x = reorder(species, ovules, FUN = median), y = ovules)) +   geom_boxplot()+
labs(title="Solanaceae",x="", y = "Ovules")+
theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black")

# OVULES BRASSICACEAE

p <- ggplot(brassicaceae, aes(x = reorder(species, ovules, FUN = median), y = ovules)) +   geom_boxplot()+
  labs(title="Brassicaceae",x="", y = "Ovules")+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black")


#Ovules all

p <- ggplot(d, aes(x = reorder(species, ovules, FUN = median), y = ovules)) +   geom_boxplot()+
  labs(title="Ovules per spp.",x="", y = "Ovules")+
  theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black")


#Plot all

cbPalette <- c( "#56B4E9","#E69F00","#009E73")

ggplot(d, aes(x = reorder(species, ovules, FUN = median, na.rm = TRUE), y = ovules)) +
  geom_boxplot(outlier.shape = NA)+labs(title="All species",x="", y = "Nº of ovules")+
  aes(fill=family)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5)) +scale_fill_manual(values=cbPalette)+
  geom_jitter(width = 0.3,shape=1,size=0.8, aes(colour=family))+
  scale_color_manual(values = cbPalette) + 
  stat_summary(fun.y=mean, geom="point", shape="*", size=5) +
  theme(legend.title = element_blank())+facet_grid(. ~ family)


#Density plot of number of ovules 

library(cowplot)
theme_set(theme_gray())
#Preparing a plot of just Brassicaceae
d <- d[d$species!="Eggplant" & d$species!="Capsicum" & d$species!="Tomato" &
         d$species!="Petunia" & d$species!="Morning glory" & d$species!="Water morning glory",]

iris2 <- ggplot(d, aes(x = ovules, fill = species)) +
  geom_density(alpha = 0.7) +
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid( iris2, labels = "AUTO")
