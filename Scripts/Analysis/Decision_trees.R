#Regression trees

#Load library
library(tree)
library(rpart)

#Now I prepare the data

all <- read.csv("Data/all.csv")
str(all)

tree.model <- tree(Species ~ value + close_related  , data=all)
plot(tree.model)
text(tree.model, cex=.75)

pruned.tree <- prune.tree(tree.model, best=4)
plot(pruned.tree)
text(pruned.tree)


library(rpart)
library(rpart.plot)
reg.tree <- rpart(value ~ Species , data = all)
rpart.plot(reg.tree, type = 4)

bra <- subset(all, Species=="BROL"|Species=="BRRA"|Species=="SIAL"|Species=="ERSA")
all$Comp[all$Species=="BROL"] <- 100
all$Comp[all$Species=="BRRA"] <- 20
all$Comp[all$Species=="SIAL"] <- 10
all$Comp[all$Species=="ERSA"] <- 0

str(bra)

reg.tree <- rpart(value ~ Species+ Non_focal, data = all)
rpart.plot(reg.tree, type = 4)

