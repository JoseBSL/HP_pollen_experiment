#load libraries

library(ggplot2)
library(forcats)

#load data
d<-read.csv("Ovules.csv", sep=";")

#Reorganise data
d <- stack(d)

#Naming columns

colnames(d)[1]<-"ovules"
colnames(d)[2]<-"species"

#boxplot from ggplot2
ggplot(d, aes(x = species, y = ovules)) + geom_boxplot()





