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
pollen_non$fam_non <- c(rep("Convolvulaceae", 8), rep("Solanaceae", 6), rep("Brassicaceae", 6))
pollen_non$fam_foc <- c("Solanaceae","Brassicaceae","Brassicaceae","Brassicaceae","Solanaceae",
                        "Solanaceae","Solanaceae","Brassicaceae","Convolvulaceae","Brassicaceae",
                        "Brassicaceae", "Brassicaceae", "Convolvulaceae", "Brassicaceae", "Convolvulaceae", "Solanaceae",
                        "Solanaceae", "Convolvulaceae","Solanaceae","Solanaceae")

pollen_non <- pollen_non[,-c(1,2)]
colnames(pollen_non)[1] <-"hp_pollen"
pollen_non$hp_ratio <- as.numeric(pollen_non$hp_pollen)/as.numeric(pollen_non$total_pollen)
hist(pollen_non$hp_ratio)

pollen_foc$total_pollen  <- total
pollen_foc$fam_non <- c(rep("Convolvulaceae", 8), rep("Solanaceae", 6), rep("Brassicaceae", 6))
pollen_foc$fam_foc <- c("Solanaceae","Brassicaceae","Brassicaceae","Brassicaceae","Solanaceae",
                        "Solanaceae","Solanaceae","Brassicaceae","Convolvulaceae","Brassicaceae",
                        "Brassicaceae", "Brassicaceae", "Convolvulaceae", "Brassicaceae", "Convolvulaceae", "Solanaceae",
                        "Solanaceae", "Convolvulaceae","Solanaceae","Solanaceae")

pollen_foc <- pollen_foc[,-c(1,2)]
colnames(pollen_foc)[1] <-"hp_pollen"
pollen_foc$hp_ratio <- as.numeric(pollen_foc$hp_pollen)/as.numeric(pollen_foc$total_pollen)
hist(pollen_foc$hp_ratio)




junk.glmer = lm(hp_ratio ~ fam_non, data = pollen_non)
lsm = lsmeans(junk.glmer, "fam_non", type = "response")
pairs(lsm)
library(reshape2)
library(multcompView)
library(lsmeans)
library(multcomp)
CLD <- CLD(lsm,alpha=0.05,adjust="tukey")

#Follow example to plot it


library(ggplot2)
ggplot(CLD,aes(x= fam_non,y= lsmean,
label = .group)) +theme_minimal()+
geom_point(shape  = 15,size   = 4) +ggtitle("B) Comparison of ratios as donor")+
geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Families")+
geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


saveRDS(pollen_non, "Data/pollen_non.RData")


hp_pollen_mean <- dcast(fam_non ~ ., value.var = "hp_pollen", 
                         fun.aggregate = mean, data = pollen_non, na.rm= TRUE)
colnames(hp_pollen_mean)[2] <- "hp_pollen"

total_pollen_mean <- dcast(fam_non~ ., value.var = "total_pollen", 
                            fun.aggregate = mean, data = pollen_non, na.rm= TRUE)

colnames(hp_pollen_mean)[2] <- "total_pollen_mean"


prop.test(x = hp_pollen_mean[1,2], n = total_pollen_mean[1,2], p = 0.5, 
          correct = FALSE, alternative = "greater")


prop.test(x = hp_pollen_mean[2,2], n = total_pollen_mean[1,2], p = 0.5, 
          correct = FALSE, alternative = "less")

prop.test(x = hp_pollen_mean[3,2], n = total_pollen_mean[1,2], p = 0.5, 
          correct = FALSE, alternative = "less")

#With pollen focal
hp_pollen_mean <- dcast(fam_non ~ ., value.var = "hp_pollen", 
                        fun.aggregate = mean, data = pollen_foc, na.rm= TRUE)

total_pollen_mean <- dcast(fam_non~ ., value.var = "total_pollen", 
                           fun.aggregate = mean, data = pollen_foc, na.rm= TRUE)


colnames(hp_pollen_mean)[2] <- "hp_pollen"

prop.test(x = hp_pollen_mean[1,2], n = total_pollen_mean[1,2], p = 0.5, 
          correct = FALSE)


prop.test(x = hp_pollen_mean[2,2], n = total_pollen_mean[1,2], p = 0.5, 
          correct = FALSE)

prop.test(x = hp_pollen_mean[3,2], n = total_pollen_mean[1,2], p = 0.5, 
          correct = FALSE)



junk.glmer = lm(hp_ratio ~ fam_foc, data = pollen_foc)
lsm = lsmeans(junk.glmer, "fam_foc", type = "response")
pairs(lsm)
CLD <- CLD(lsm,alpha=0.05,adjust="tukey")

par(mfrow=c(2,2))
ggplot(CLD,aes(x= fam_foc,y= lsmean,
               label = .group)) +theme_minimal()+
  geom_point(shape  = 15,size   = 4) +ggtitle("A) Comparison of ratios as recipient")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Families")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


