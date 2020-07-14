#Here I try to compare the different proportions of pollen between the different groups
#I use Tukey method

library(lme4)
library(emmeans)
library(reshape2)
library(multcompView)
library(lsmeans)
library(multcomp)
library(ggplot2)
library(DHARMa)

total_pollen <- read.csv("Data/Csv/total_pollen.csv")

total_pollen=total_pollen[,-1]
colnames(total_pollen)[3] <- "pollen"
head(total_pollen)


# Dividing by focal pollen and non focal pollen
# In order to compare pollen as donor and recipient per family 
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


#Analyse data and plot it

m1 = lm(hp_ratio ~ fam_non, data = pollen_non)
plot(m1)
#goodness of fit of the model
testDispersion(m1)
simulationOutput <- simulateResiduals(fittedModel = m1, plot = T)

lsm = lsmeans(m1, "fam_non", type = "response")
pairs(lsm)
CLD <- CLD(lsm,alpha=0.05,adjust="tukey")

ggplot(CLD,aes(x= fam_non,y= lsmean,
label = .group)) +theme_minimal()+
geom_point(shape  = 15,size   = 4) +ggtitle("B) Comparison of ratios as donor")+
geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL),
width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Families")+
geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black")


#Analyse data and plot it
m2 = lm(hp_ratio ~ fam_foc, data = pollen_foc)
lsm = lsmeans(m2, "fam_foc", type = "response")
pairs(lsm)
CLD <- CLD(lsm,alpha=0.05,adjust="tukey")

par(mfrow=c(2,2))
ggplot(CLD,aes(x= fam_foc,y= lsmean,
               label = .group)) +theme_minimal()+
  geom_point(data = CLD, aes(x = fam_foc, y = lsmean, color = fam_foc),shape  = 15,size   = 4) +
scale_color_manual(name = "Families",values = c("Convolvulaceae" = "#009E73", "Solanaceae" = "#D55E00", "Brassicaceae" = "#0072B2"))+ggtitle("A) Comparison of ratios as recipient")+
  geom_errorbar(aes(ymin  =  lower.CL,ymax  =  upper.CL,  color = fam_foc),
                width =  0.2,size =  0.7)+ylab("Least square mean")+xlab("Families")+
  geom_text(nudge_x = c(0, 0, 0), nudge_y = c(0.8, 0.8, 0.8),color   = "black") + theme(legend.position = "none")


