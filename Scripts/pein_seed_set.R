#Preparing seed set plots, starting with Petunia

#There are 3 crosses were seeds were lost (2,7,9)
#load libraries
library(reshape2)

#read data
pein_seed_set <- read.csv("Data/species_seed_set/PEIN_seed_set.csv", sep=";")
pein_seed_set <- pein_seed_set[-c(42,47,49),]

mean_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = pein_seed_set, na.rm= TRUE)

colnames(mean_seed_seet)[2] <-"avg"

sd_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sd, data = pein_seed_set, na.rm= TRUE)

colnames(sd_seed_seet)[2] <-"sdev"


x <- 1:24


plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars"
)
# add arrows
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_seed_seet$Treatment)
axis(x, at=1:24, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Unify mean and sd with merge
mean_sd <- merge(mean_seed_seet,sd_seed_seet, by="Treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered <- mean_sd[order(mean_sd$avg),] 

x <- 1:24
plot(x, mean_sd_ordered$avg,
     ylim=range(c(mean_sd_ordered$avg-mean_sd_ordered$sdev, mean_sd_ordered$avg+mean_sd_ordered$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered$avg-mean_sd_ordered$sdev, x, mean_sd_ordered$avg+mean_sd_ordered$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered$Treatment)
axis(x, at=1:24, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now with eggplant (S. melongena, variety "little fingers")
#SOME

some_seed_set <- read.csv("Data/species_seed_set/SOME_seed_set.csv", sep=";")
mean_seed_seet_some <- dcast(Treatment ~ ., value.var = "seed_set", fun.aggregate = mean, data = some_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_some)[2] <-"avg"
sd_seed_seet_some <- dcast(Treatment ~ ., value.var = "seed_set", fun.aggregate = sd, data = some_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_some)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_some <- merge(mean_seed_seet_some,sd_seed_seet_some, by="Treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_some <- mean_sd_some[order(mean_sd_some$avg),] 

x <- 1:23
plot(x, mean_sd_ordered_some$avg,
     ylim=range(c(mean_sd_ordered_some$avg-mean_sd_ordered_some$sdev, mean_sd_ordered_some$avg+mean_sd_ordered_some$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_some$avg-mean_sd_ordered_some$sdev, x, mean_sd_ordered_some$avg+mean_sd_ordered_some$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_some$Treatment)
axis(x, at=1:24, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now with capscum (C. annuum, variety "California wonder")
#CAAN
caan_seed_set <- read.csv("Data/species_seed_set/CAAN_seed_set.csv", sep=";")
mean_seed_seet_caan <- dcast(treatment ~ ., value.var = "seed_set", fun.aggregate = mean, data = caan_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_caan)[2] <-"avg"
sd_seed_seet_caan <- dcast(treatment ~ ., value.var = "seed_set", fun.aggregate = sd, data = caan_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_caan)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_caan <- merge(mean_seed_seet_caan,sd_seed_seet_caan, by="treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_caan <- mean_sd_caan[order(mean_sd_caan$avg),] 

x <- 1:22
plot(x, mean_sd_ordered_caan$avg,
     ylim=range(c(mean_sd_ordered_caan$avg-mean_sd_ordered_caan$sdev, mean_sd_ordered_caan$avg+mean_sd_ordered_caan$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_caan$avg-mean_sd_ordered_caan$sdev, x, mean_sd_ordered_caan$avg+mean_sd_ordered_caan$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_caan$treatment)
axis(x, at=1:24, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

