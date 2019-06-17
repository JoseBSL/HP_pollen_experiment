#Preparing seed set plots, starting with Petunia

#There are 3 crosses were seeds were lost (2,7,9)
#load libraries
library(reshape2)
library(ggplot2)
#read data
pein_seed_set <- read.csv("Data/species_seed_set/PEIN_seed_set.csv", sep=";")
pein_seed_set <- pein_seed_set[-c(42,47,49),]

mean_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = pein_seed_set, na.rm= TRUE)

colnames(mean_seed_seet)[2] <-"avg"

sd_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sd, data = pein_seed_set, na.rm= TRUE)

colnames(sd_seed_seet)[2] <-"sdev"



#Unify mean and sd with merge
mean_sd <- merge(mean_seed_seet,sd_seed_seet, by="Treatment")
#Removing RARA, species not considered because sterility 
mean_sd<- mean_sd[-c(16,17),]

merge_1 <- data.frame(mean_sd, stringsAsFactors = F)
  
merge_1$Treatment<- as.character(merge_1$Treatment)
pein <- c("PEIN", NA, NA)

str(merge_1)
mean_sd_1 <- rbind(merge_1,pein)

#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered <- mean_sd[order(mean_sd$avg),]  



mean_sd_ordered_alphabetically<- mean_sd_ordered[order(mean_sd_ordered$Treatment),]

#ploting averages with sd
x <- 1:22
plot(x, mean_sd_ordered$avg,
     ylim=range(c(mean_sd_ordered$avg-mean_sd_ordered$sdev, mean_sd_ordered$avg+mean_sd_ordered$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered$avg-mean_sd_ordered$sdev, x, mean_sd_ordered$avg+mean_sd_ordered$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered$Treatment)
axis(x, at=1:22, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now I'm going to try to represent the same data in a different way, summatory of seed set which could be more informative

summatory_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sum, data = pein_seed_set, na.rm= TRUE)

#Delete RARA rows, species not included because sterility
summatory_seed_seet <- summatory_seed_seet[-c(16,17),]
colnames(summatory_seed_seet)[2] <- "seeds"

p <- ggplot(data=summatory_seed_seet, aes(x=reorder(Treatment, seeds, colour = cut), y=seeds))+
  geom_bar(stat="identity", width=0.5)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_manual(values=colours)
p + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))+ labs(x = "Species", y="Seeds summatory")

#Now with eggplant (S. melongena, variety "little fingers")
#SOME

some_seed_set <- read.csv("Data/species_seed_set/SOME_seed_set.csv", sep=";")
mean_seed_seet_some <- dcast(Treatment ~ ., value.var = "seed_set", fun.aggregate = mean, data = some_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_some)[2] <-"avg"
sd_seed_seet_some <- dcast(Treatment ~ ., value.var = "seed_set", fun.aggregate = sd, data = some_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_some)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_some <- merge(mean_seed_seet_some,sd_seed_seet_some, by="Treatment")
#Delete RARA rows, species not included because sterility
mean_sd_some <- mean_sd_some[-c(8,9,20),]

#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_some <- mean_sd_some[order(mean_sd_some$avg),] 

x <- 1:22
plot(x, mean_sd_ordered_some$avg,
     ylim=range(c(mean_sd_ordered_some$avg-mean_sd_ordered_some$sdev, mean_sd_ordered_some$avg+mean_sd_ordered_some$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_some$avg-mean_sd_ordered_some$sdev, x, mean_sd_ordered_some$avg+mean_sd_ordered_some$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_some$Treatment)
axis(x, at=1:22, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)


#Now I'm going to try to represent the same data in a different way, summatory of seed set which could be more informative

summatory_seed_seet_some <- dcast(Treatment ~ ., value.var = "seed_set", fun.aggregate = sum, data = some_seed_set, na.rm= TRUE)

#Delete RARA rows, species not included because sterility
summatory_seed_seet_some <- summatory_seed_seet_some[-c(8,9,20),]
colnames(summatory_seed_seet_some)[2] <- "seeds"

p <- ggplot(data=summatory_seed_seet_some, aes(x=reorder(Treatment, seeds, colour = cut), y=seeds))+
  geom_bar(stat="identity", width=0.5)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_manual(values=colours)
p + ggtitle("") + theme(plot.title = element_text(hjust = 0.5))+ labs(x = "Species", y="Seeds summatory")


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
axis(x, at=1:22, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now with tomato (S. lycopersicum, variety "Tommy Joe")
#SOLY
soly_seed_set <- read.csv("Data/species_seed_set/SOLY_seed_set.csv", sep=";")
mean_seed_seet_soly <- dcast(Treatment ~ ., value.var = "seed_set", fun.aggregate = mean, data = soly_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_soly)[2] <-"avg"
sd_seed_seet_soly <- dcast(Treatment ~ ., value.var = "seed_set", fun.aggregate = sd, data = soly_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_soly)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_soly <- merge(mean_seed_seet_soly,sd_seed_seet_soly, by="Treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_soly <- mean_sd_soly[order(mean_sd_soly$avg),] 

x <- 1:26
plot(x, mean_sd_ordered_soly$avg,
     ylim=range(c(mean_sd_ordered_soly$avg-mean_sd_ordered_soly$sdev, mean_sd_ordered_soly$avg+mean_sd_ordered_soly$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_soly$avg-mean_sd_ordered_soly$sdev, x, mean_sd_ordered_soly$avg+mean_sd_ordered_soly$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_soly$Treatment)
axis(x, at=1:26, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now with BROL (B. oleracea, variety "capitata")
#BROL
brol_seed_set <- read.csv("Data/species_seed_set/BROL_seed_set.csv", sep=";")
mean_seed_seet_brol <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = brol_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_brol)[2] <-"avg"
sd_seed_seet_brol <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sd, data = brol_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_brol)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_brol <- merge(mean_seed_seet_brol,sd_seed_seet_brol, by="Treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_brol <- mean_sd_brol[order(mean_sd_brol$avg),] 

x <- 1:25
plot(x, mean_sd_ordered_brol$avg,
     ylim=range(c(mean_sd_ordered_brol$avg-mean_sd_ordered_brol$sdev, mean_sd_ordered_brol$avg+mean_sd_ordered_brol$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_brol$avg-mean_sd_ordered_brol$sdev, x, mean_sd_ordered_brol$avg+mean_sd_ordered_brol$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_brol$Treatment)
axis(x, at=1:25, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)


#Now with SIAL (S. alba, white mustard)
#SIAL
sial_seed_set <- read.csv("Data/species_seed_set/SIAL_seed_set.csv", sep=";")
mean_seed_seet_sial <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = sial_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_sial)[2] <-"avg"
sd_seed_seet_sial <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sd, data = sial_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_sial)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_sial <- merge(mean_seed_seet_sial,sd_seed_seet_sial, by="Treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_sial <- mean_sd_sial[order(mean_sd_sial$avg),] 

x <- 1:22
plot(x, mean_sd_ordered_sial$avg,
     ylim=range(c(mean_sd_ordered_sial$avg-mean_sd_ordered_sial$sdev, mean_sd_ordered_sial$avg+mean_sd_ordered_sial$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_sial$avg-mean_sd_ordered_sial$sdev, x, mean_sd_ordered_sial$avg+mean_sd_ordered_sial$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_sial$Treatment)
axis(x, at=1:22, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now with ERSA (E. versicaria, wild rocket)
#ERSA

ersa_seed_set <- read.csv("Data/species_seed_set/ERSA_seed_set.csv", sep=";")
mean_seed_seet_ersa <- dcast(Treatment ~ ., value.var = "seed.production", fun.aggregate = mean, data = ersa_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_ersa)[2] <-"avg"
sd_seed_seet_ersa <- dcast(Treatment ~ ., value.var = "seed.production", fun.aggregate = sd, data = ersa_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_ersa)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_ersa <- merge(mean_seed_seet_ersa,sd_seed_seet_ersa, by="Treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_ersa <- mean_sd_ersa[order(mean_sd_ersa$avg),] 

x <- 1:22
plot(x, mean_sd_ordered_ersa$avg,
     ylim=range(c(mean_sd_ordered_ersa$avg-mean_sd_ordered_ersa$sdev, mean_sd_ordered_ersa$avg+mean_sd_ordered_ersa$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_ersa$avg-mean_sd_ordered_ersa$sdev, x, mean_sd_ordered_ersa$avg+mean_sd_ordered_ersa$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_ersa$Treatment)
axis(x, at=1:22, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now with BRRA (B. rapa, chinensis)
#BRRA

brra_seed_set <- read.csv("Data/species_seed_set/BRRA_seed_set.csv", sep=";")
mean_seed_seet_brra <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = brra_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_brra)[2] <-"avg"
sd_seed_seet_brra <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sd, data = brra_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_brra)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_brra <- merge(mean_seed_seet_brra,sd_seed_seet_brra, by="Treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_brra <- mean_sd_brra[order(mean_sd_brra$avg),] 

x <- 1:22
plot(x, mean_sd_ordered_brra$avg,
     ylim=range(c(mean_sd_ordered_brra$avg-mean_sd_ordered_brra$sdev, mean_sd_ordered_brra$avg+mean_sd_ordered_brra$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_brra$avg-mean_sd_ordered_brra$sdev, x, mean_sd_ordered_brra$avg+mean_sd_ordered_brra$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_brra$Treatment)
axis(x, at=1:22, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)

#Now with IPAQ (I. aquatica)
#IPAQ

ipaq_seed_set <- read.csv("Data/species_seed_set/IPAQ_seed_set.csv", sep=";")
mean_seed_seet_ipaq <- dcast(treatment ~ ., value.var = "seed_set", fun.aggregate = mean, data = ipaq_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_ipaq)[2] <-"avg"
sd_seed_seet_ipaq <- dcast(treatment ~ ., value.var = "seed_set", fun.aggregate = sd, data = ipaq_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_ipaq)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_ipaq <- merge(mean_seed_seet_ipaq,sd_seed_seet_ipaq, by="treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_ipaq <- mean_sd_ipaq[order(mean_sd_ipaq$avg),] 

x <- 1:24
plot(x, mean_sd_ordered_ipaq$avg,
     ylim=range(c(mean_sd_ordered_ipaq$avg-mean_sd_ordered_ipaq$sdev, mean_sd_ordered_ipaq$avg+mean_sd_ordered_ipaq$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_ipaq$avg-mean_sd_ordered_ipaq$sdev, x, mean_sd_ordered_ipaq$avg+mean_sd_ordered_ipaq$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_ipaq$treatment)
axis(x, at=1:24, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)


#Now with IPPU (I. purpurea)
#IPPU

ippu_seed_set <- read.csv("Data/species_seed_set/IPPU_seed_set.csv", sep=";")
mean_seed_seet_ippu <- dcast(treatment ~ ., value.var = "seed.set", fun.aggregate = mean, data = ippu_seed_set, na.rm= TRUE)
colnames(mean_seed_seet_ippu)[2] <-"avg"
sd_seed_seet_ippu <- dcast(treatment ~ ., value.var = "seed.set", fun.aggregate = sd, data = ippu_seed_set, na.rm= TRUE)
colnames(sd_seed_seet_ippu)[2] <-"sdev"

#Unify mean and sd with merge
mean_sd_ippu <- merge(mean_seed_seet_ippu,sd_seed_seet_ippu, by="treatment")
#Order from lower to higher values the average, to plot it nicely
mean_sd_ordered_ippu <- mean_sd_ippu[order(mean_sd_ippu$avg),] 

x <- 1:24
plot(x, mean_sd_ordered_ippu$avg,
     ylim=range(c(mean_sd_ordered_ippu$avg-mean_sd_ordered_ippu$sdev, mean_sd_ordered_ippu$avg+mean_sd_ordered_ippu$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars")
# add arrows
arrows(x, mean_sd_ordered_ippu$avg-mean_sd_ordered_ippu$sdev, x, mean_sd_ordered_ippu$avg+mean_sd_ordered_ippu$sdev, length=0.05, angle=90, code=3)
lablist.x<-as.vector(mean_sd_ordered_ippu$treatment)
axis(x, at=1:24, labels = FALSE)
text(x, par("usr")[3] - 0.2, labels = lablist.x, adj = 1.25,srt = 45, xpd = TRUE)
