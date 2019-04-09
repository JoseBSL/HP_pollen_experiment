#install.packages("corrplot")
library(corrplot)

#I'm going to try to show our traits on this way
M <- cor(mtcars)
corrplot(M, method = "circle")

#We can kill two birds with one stone, showing the correlation and all the traits


a <- read.csv("Data/traits_all.csv")
a=a[,-c(1,2)]
a<- cor(a)

corrplot(a, method = "circle")
corrplot(a, type = "upper")
