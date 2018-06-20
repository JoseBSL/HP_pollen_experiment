#Preparing seed set plots, starting with Petunia

#There are 3 crosses were seeds were lost (2,7,9)
#load libraries
library(reshape2)

#read data
pein_seed_set <- read.csv("Data/species_seed_set/PEIN_seed_set.csv", sep=";")


mean_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = pein_seed_set, na.rm= TRUE)

mean_seed_seet$.-> avg

sd_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sd, data = pein_seed_set, na.rm= TRUE)

sd_seed_seet$. -> sdev


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



