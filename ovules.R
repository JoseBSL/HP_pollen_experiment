#load libraries

library(ggplot2)

ovules<-read.csv("Ovules.csv", sep=";")

ovules <- stack(ovules)





ggplot(solanaceae)+ geom_boxplot()





