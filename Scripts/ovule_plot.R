
#OVYLE NUMBER OF THE DIFFERENT SPECIES N=15

library(reshape2)

# Consider NA's as 0, I have to double check this

ovules <- read.csv("Ovules.csv", sep=";")

#plot(ovules)

mean_ovule <- dcast(Species ~ ., value.var = "Ovules", fun.aggregate = mean, data = ovules, na.rm= TRUE)

colnames(mean_ovule)[2] <- "avg"



sd_ovule <- dcast(Species ~ ., value.var = "Ovules", fun.aggregate = sd, data = ovules, na.rm= TRUE)

colnames(sd_ovule)[2] <- "sdev"


x <- 1:8


plot(x, mean_ovule$avg,
     ylim=range(c(mean_ovule$avg-sd_ovule$sdev, mean_ovule$avg+sd_ovule$sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="N of Ovules")

# hack: we draw arrows but with very special "arrowheads"
arrows(x, mean_ovule$avg-sd_ovule$sdev, x, mean_ovule$avg+sd_ovule$sdev, length=0.05, angle=90, code=3)

axis(x, at=1:8, labels = mean_ovule$Species,cex=0.2, tick = FALSE, padj= -0.5, side = 1)


