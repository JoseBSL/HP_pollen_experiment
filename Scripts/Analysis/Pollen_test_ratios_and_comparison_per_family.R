#Here I try to compare the different proportions of pollen between the different groups

library(lme4)
library(emmeans)

total_pollen <- read.csv("Data/total_pollen.csv")

total_pollen=total_pollen[,-1]
colnames(total_pollen)[3] <- "pollen"
head(total_pollen)


# Dividing by focal pollen and non focal pollen
pollen_foc <- subset(total_pollen, variable=="focal_pollen")
pollen_non <- subset(total_pollen, variable=="non_focal_pollen")

total  <- as.numeric(pollen_foc$pollen) + as.numeric(pollen_non$pollen)
pollen_non$total_pollen  <- total
pollen_non$fam_non <- c(rep("C", 8), rep("S", 6), rep("B", 6))
pollen_non$fam_foc <- c("S","B","B","B","S","S","S","B","C","B","B", "B", "C", "B", "C", "S",
                        "S", "C","S","S")

pollen_non <- pollen_non[,-c(1,2)]
colnames(pollen_non)[1] <-"hp_pollen"
pollen_non$hp_ratio <- as.numeric(pollen_non$hp_pollen)/as.numeric(pollen_non$total_pollen)
hist(pollen_non$hp_ratio)

junk.glmer = lm(hp_ratio ~ fam_non, data = pollen_non)
lsm = lsmeans(junk.glmer, "fam_non", type = "response")
pairs(lsm)

library(reshape2)

hp_pollen_mean <- dcast(fam_non ~ ., value.var = "hp_pollen", 
                         fun.aggregate = mean, data = pollen_non, na.rm= TRUE)
colnames(hp_pollen_mean)[2] <- "hp_pollen"

total_pollen_mean <- dcast(fam_non~ ., value.var = "total_pollen", 
                            fun.aggregate = mean, data = pollen_non, na.rm= TRUE)

colnames(hp_pollen_mean)[2] <- "total_pollen_mean"


prop.test(x = hp_pollen_mean[1,2], n = total_pollen_mean[1,2], p = 0.5, 
          correct = FALSE)
