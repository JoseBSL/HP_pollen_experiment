#install.packages("corrplot")
library(corrplot)

#I'm going to try to show our traits on this way
M <- cor(mtcars)
corrplot(M, method = "circle")

#We can kill two birds with one stone, showing the correlation and all the traits


a <- read.csv("Data/traits_all.csv")
a=a[,-c(1,2)]
a<- cor(a)
str(a)
a=a[-c(1,7,11),-c(1,7,11)]

rownames(a)<- c("Selfing rate", "Pollen size", "Pollen per anther", "Ovules", "Pollen-ovule ratio",
                "Stigmatic area", "Stigma length", "Stigma width", "Style length", "Style width",
                "Ovary width", "Ovary length")

colnames(a)<- c("Selfing rate", "Pollen size", "Pollen per anther", "Ovules", "Pollen-ovule ratio",
                "Stigmatic area", "Stigma length", "Stigma width", "Style length", "Style width",
                "Ovary width", "Ovary length")
  
corrplot(a, method = "circle")
corrplot(a, type = "upper")

corrplot(a, type = "lower",  tl.col = "black", tl.srt = 45)
