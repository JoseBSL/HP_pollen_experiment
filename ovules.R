#load libraries

library(ggplot2)
library(forcats)

#load data
d<-read.csv("Ovules.csv", sep=";")

colnames(d)[1]<-"C. annuum"
colnames(d)[2]<-"S. lycopersicum"
colnames(d)[3]<-"S. melongera"
colnames(d)[4]<-"P. integrifolia"


#Reorganise data
d <- stack(d)
#Naming columns
colnames(d)[1]<-"ovules"
colnames(d)[2]<-"species"


#boxplot from ggplot2
ggplot(d, aes(x = species, y = ovules)) + geom_boxplot()

#boxplot from ggplot2 reorder
ggplot(d, aes(x = reorder(species, ovules, FUN = median), y = ovules)) + geom_boxplot()

#subset per family (solanaceae, brassicaceae, convolvulaceae)
solanaceae<-subset(d)[1:60,]

ggplot(solanaceae, aes(x = species, y = ovules)) + geom_boxplot()

ggplot(solanaceae, aes(x = reorder(species, ovules, FUN = median), y = ovules)) +   geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()

p <- ggplot(solanaceae, aes(x = reorder(species, ovules, FUN = median), y = ovules)) +   geom_boxplot()+
labs(title="Solanaceae",x="", y = "Ovules")+
theme(plot.title = element_text(hjust = 0.5))

p + stat_summary(fun.y=mean, geom="point", shape="*", size=5, colour="black")


