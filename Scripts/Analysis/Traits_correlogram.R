##################################################################################################################################################################
#Correlation plot between traits
##################################################################################################################################################################
#install.packages("corrplot")
library(corrplot)

#I'm going to try to show our traits on this way
M <- cor(mtcars)
corrplot(M, method = "circle")


#read data
traits_all <- read.csv("Data/Csv/traits_all.csv")
si_index <- readRDS("Data/RData/si_index.RData")
traits_all$si_index <- si_index
traits_all=traits_all[,-c(1,2)]
traits_all=traits_all[,-c(1,7,11)]
#Change order of ovary width length I rename below in row and colname
a<-traits_all[,11]
b<-traits_all[,12]
traits_all[,11] <-b
traits_all[,12]<- a
traits_all<- cor(traits_all)

rownames(traits_all)<- c("Selfing rate", "Pollen size", "Pollen per anther", "Number of ovules", "Pollen-ovule ratio",
                         "Stigmatic area", "Stigma length", "Stigma width", "Style length", "Style width",
                         "Ovary length", "Ovary width", "SI index")

colnames(traits_all)<- c("Selfing rate", "Pollen size", "Pollen per anther", "Number of ovules", "Pollen-ovule ratio",
                "Stigmatic area", "Stigma length", "Stigma width", "Style length", "Style width",
                "Ovary length", "Ovary width", "SI index")
  
corrplot(traits_all, method = "circle")
corrplot(traits_all, type = "upper")

corrplot(traits_all, type = "lower",  tl.col = "black", tl.srt = 45)

#Add Compatibility index!

traits_all_filtered <- traits_all[-c(1,4,6,7,8,10:13),-c(1,4,6,7,8,10:13)]
corrplot(traits_all_filtered, type = "lower",  tl.col = "black", tl.srt = 45)

