
#In this script I'm going to represent the seed set of the different species 

#First species Petunia integrifolia, CODE=PEIN, N=10 for each treatment, just represented the ones that we have obtained seed set


library(reshape2)

# Consider NA's as 0, I have to double check this

seed_set <- read.csv("PEIN_seed_set.csv", sep=";")

#Delete Cross with lost seeds
seed_set[c(12,17,19),4] <- NA
#Deleted

mean_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = mean, data = seed_set, na.rm= TRUE)

mean_seed_seet$.-> avg

sd_seed_seet <- dcast(Treatment ~ ., value.var = "Seed.production", fun.aggregate = sd, data = seed_set, na.rm= TRUE)

sd_seed_seet$. -> sdev

sdev[4] <- 0

plot(seed_set, cex.axis=0.5)

x <- 1:7

plot(x, avg,
     ylim=range(c(avg-sdev, avg+sdev)),
     pch=19, xlab="", xaxt='n', ylab="Mean +/- SD",
     main="Scatter plot with std.dev error bars"
)
# hack: we draw arrows but with very special "arrowheads"
arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)

axis(x, at=1:7, labels = mean_seed_seet$Treatment,cex=0.2, tick = FALSE, padj= -0.5, side = 1)


