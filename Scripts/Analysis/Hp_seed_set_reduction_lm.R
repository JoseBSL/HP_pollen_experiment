
#Statistical differences between seed set with the different treatment

#load libraries
library(dplyr)
library(nlme)
library(lme4)
library(forcats)
library(jtools)
library(fitdistrplus)

#Read ovule number data 
ovule_number <- readRDS("Data/RData/ovule_number.RData")
#Following the advice of a rviewer we are going to use seed/ovule as proxy of effect
#and analyse it with a binomial distribution

#50-50% pollen analysis treatments vs cross (control). Log transform seed set to make it closer to normality
#I do it for each species separately and compare with all the treatments

#
#
#SOLANACEAE 1st
#
#


#PEIN
pein_seed_set_final <- read.csv("Raw_data/pein_seed_set_final.csv", stringsAsFactors = T)
pein_seed_set_final <- subset(pein_seed_set_final, Treatment!="FLOWER CONTROL" & Treatment!="CONTROL" & Treatment!="SELF")
pein_seed_set_final=na.omit(pein_seed_set_final)
pein_seed_set_final$Treatment <- factor(pein_seed_set_final$Treatment)
levels(pein_seed_set_final$Treatment)
pein_seed_set_final$Treatment <- relevel(pein_seed_set_final$Treatment, ref="CROSS")
model1=lm(log(1+Seed.production)~Treatment, data=pein_seed_set_final)
summary(model1)
pein <- summ(model1, model.info=FALSE, model.fit=FALSE)
pein <- as.data.frame(pein$coeftable)
rownames(pein) <- gsub("Treatment", "", rownames(pein))
rownames(pein) <- paste("Petunia integrifolia", rownames(pein), sep=" - ")

#SOLY
soly_seed_set_final <- read.csv("Raw_data/soly_seed_set_final.csv", stringsAsFactors = T)
soly_seed_set_final <- subset(soly_seed_set_final, Treatment!="Flower control" & Treatment!="Control" & Treatment!="Self")
soly_seed_set_final=na.omit(soly_seed_set_final)
soly_seed_set_final$Treatment <- factor(soly_seed_set_final$Treatment)
levels(soly_seed_set_final$Treatment)
soly_seed_set_final$Treatment <- relevel(soly_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=soly_seed_set_final)
summary(model1)
soly <- summ(model1, model.info=FALSE, model.fit=FALSE)
soly <- as.data.frame(soly$coeftable)
rownames(soly) <- gsub("Treatment", "", rownames(soly))
rownames(soly) <- paste("Solanum lycopersicum", rownames(soly), sep=" - ")


#SOME
some_seed_set_final <- read.csv("Raw_data/some_seed_set_final.csv", stringsAsFactors = T)
some_seed_set_final <- subset(some_seed_set_final, Treatment!="Flower control" & Treatment!="Control" & Treatment!="Self")
some_seed_set_final=na.omit(some_seed_set_final)
some_seed_set_final$Treatment <- factor(some_seed_set_final$Treatment)
levels(some_seed_set_final$Treatment)
some_seed_set_final$Treatment <- relevel(some_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=some_seed_set_final)
summary(model1)
some <- summ(model1, model.info=FALSE, model.fit=FALSE)
some <- as.data.frame(some$coeftable)
rownames(some) <- gsub("Treatment", "", rownames(some))
rownames(some) <- paste("Solanum melongena", rownames(some), sep=" - ")
#CAAN
caan_seed_set_final <- read.csv("Raw_data/caan_seed_set_final.csv", stringsAsFactors = T)
caan_seed_set_final <- subset(caan_seed_set_final, Treatment!="FLOWER CONTROL" & Treatment!="CONTROL" & Treatment!="SELF")
caan_seed_set_final=na.omit(caan_seed_set_final)
caan_seed_set_final$Treatment <- factor(caan_seed_set_final$Treatment)
levels(caan_seed_set_final$Treatment)
caan_seed_set_final$Treatment <- relevel(caan_seed_set_final$Treatment, ref="CROSS")
model1=lm(log(1+Seed.production)~Treatment, data=caan_seed_set_final)
summary(model1)
caan <- summ(model1, model.info=FALSE, model.fit=FALSE)
caan <- as.data.frame(caan$coeftable)
rownames(caan) <- gsub("Treatment", "", rownames(caan))
rownames(caan) <- paste("Capsicum annuum", rownames(caan), sep=" - ")

#
#
#BRASSICACEAE 2nd
#
#


#BROL
brol_seed_set_final <- read.csv("Raw_data/brol_seed_set_final.csv", stringsAsFactors = T)
brol_seed_set_final <- subset(brol_seed_set_final, Treatment!="FC" & Treatment!="Control" & Treatment!="Self")
brol_seed_set_final=na.omit(brol_seed_set_final)
brol_seed_set_final$Treatment <- factor(brol_seed_set_final$Treatment)
levels(brol_seed_set_final$Treatment)
brol_seed_set_final$Treatment <- relevel(brol_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=brol_seed_set_final)
summary(model1)
brol <- summ(model1, model.info=FALSE, model.fit=FALSE)
brol <- as.data.frame(brol$coeftable)
rownames(brol) <- gsub("Treatment", "", rownames(brol))
rownames(brol) <- paste("Brassica oleracea", rownames(brol), sep=" - ")

#BRRA
brra_seed_set_final <- read.csv("Raw_data/brra_seed_set_final.csv", stringsAsFactors = T)
brra_seed_set_final <- subset(brra_seed_set_final, Treatment!="Flower Control" & Treatment!="Control" & Treatment!="Self")
brra_seed_set_final=na.omit(brra_seed_set_final)
brra_seed_set_final$Treatment <- factor(brra_seed_set_final$Treatment)
levels(brra_seed_set_final$Treatment)
brra_seed_set_final$Treatment <- relevel(brra_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=brra_seed_set_final)
summary(model1)
brra <- summ(model1, model.info=FALSE, model.fit=FALSE)
brra <- as.data.frame(brra$coeftable)
rownames(brra) <- gsub("Treatment", "", rownames(brra))
rownames(brra) <- paste("Brassica rapa", rownames(brra), sep=" - ")


#SIAL
sial_seed_set_final <- read.csv("Raw_data/sial_seed_set_final.csv", stringsAsFactors = T)
sial_seed_set_final <- subset(sial_seed_set_final, Treatment!="Flower Control" & Treatment!="Control" & Treatment!="Self")
sial_seed_set_final=na.omit(sial_seed_set_final)
sial_seed_set_final$Treatment <- factor(sial_seed_set_final$Treatment)
levels(sial_seed_set_final$Treatment)
sial_seed_set_final$Treatment <- relevel(sial_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=sial_seed_set_final)
summary(model1)
sial <- summ(model1, model.info=FALSE, model.fit=FALSE)
sial<- as.data.frame(sial$coeftable)
rownames(sial) <- gsub("Treatment", "", rownames(sial))
rownames(sial) <- paste("Sinapis alba", rownames(sial), sep=" - ")


#ERSA
ersa_seed_set_final <- read.csv("Raw_data/ersa_seed_set_final.csv", stringsAsFactors = T)
ersa_seed_set_final <- subset(ersa_seed_set_final, Treatment!="Flower Control" & Treatment!="Control" & Treatment!="Self")
ersa_seed_set_final=na.omit(ersa_seed_set_final)
ersa_seed_set_final$Treatment <- factor(ersa_seed_set_final$Treatment)
levels(ersa_seed_set_final$Treatment)
ersa_seed_set_final$Treatment <- relevel(ersa_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=ersa_seed_set_final)
summary(model1)
ersa <- summ(model1, model.info=FALSE, model.fit=FALSE)
ersa <- as.data.frame(ersa$coeftable)
rownames(ersa) <- gsub("Treatment", "", rownames(ersa))
rownames(ersa) <- paste("Eruca sativa", rownames(ersa), sep=" - ")

#
#
#CONVOLVULACEAE 3rd
#
#

#IPPU
ippu_seed_set_final <- read.csv("Raw_data/ippu_seed_set_final.csv", stringsAsFactors = T)
ippu_seed_set_final <- subset(ippu_seed_set_final, Treatment!="Flower control" & Treatment!="Control" & Treatment!="Self")
ippu_seed_set_final=na.omit(ippu_seed_set_final)
ippu_seed_set_final$Treatment <- factor(ippu_seed_set_final$Treatment)
levels(ippu_seed_set_final$Treatment)
ippu_seed_set_final$Treatment <- relevel(ippu_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=ippu_seed_set_final)
summary(model1)
ippu <- summ(model1, model.info=FALSE, model.fit=FALSE)
ippu <- as.data.frame(ippu$coeftable)
rownames(ippu) <- gsub("Treatment", "", rownames(ippu))
rownames(ippu) <- paste("Ipomoea purpurea", rownames(ippu), sep=" - ")

#IPAQ
ipaq_seed_set_final <- read.csv("Raw_data/ipaq_seed_set_final.csv", stringsAsFactors = T)
ipaq_seed_set_final <- subset(ipaq_seed_set_final, Treatment!="Flower control" & Treatment!="Ipomoea aquatica" & Treatment!="Control" & Treatment!="Self")
ipaq_seed_set_final=na.omit(ipaq_seed_set_final)
ipaq_seed_set_final$Treatment <- factor(ipaq_seed_set_final$Treatment)
levels(ipaq_seed_set_final$Treatment)
ipaq_seed_set_final$Treatment <- relevel(ipaq_seed_set_final$Treatment, ref="Cross")
model1=lm(log(1+Seed.production)~Treatment, data=ipaq_seed_set_final)
summary(model1)
ipaq <- summ(model1, model.info=FALSE, model.fit=FALSE)
ipaq <- as.data.frame(ipaq$coeftable)
rownames(ipaq) <- gsub("Treatment", "", rownames(ipaq))
rownames(ipaq) <- paste("Ipomoea aquatica", rownames(ipaq), sep=" - ")

table <- rbind(pein, soly, some , caan, brol, brra, ersa, ippu, ipaq)
saveRDS(table, "Data/RData/lm_table.RData")



#rownames(table) <- c(rownames(pein), rownames(soly),  rownames(some),  rownames(caan), 
#rownames(brol),  rownames(brra),  rownames(ersa),  rownames(ippu),
#rownames(ipaq))





#saverds files to load them in other script

#saveRDS(pein, "Data/pein.RData")
#saveRDS(soly, "Data/soly.RData")
#saveRDS(caan, "Data/caan.RData")
#saveRDS(some, "Data/some.RData")
#saveRDS(brol, "Data/brol.RData")
#saveRDS(brra, "Data/brra.RData")
#saveRDS(ersa, "Data/ersa.RData")
#saveRDS(sial, "Data/sial.RData")
#saveRDS(ippu, "Data/ippu.RData")
#saveRDS(ipaq, "Data/ipaq.RData")



#
##
####
#####NOW I ANALYSYS 100% POLLEN TREATMENTS
####
##
#

#SOLANACEAE
#PEIN
pein_seed_set_100 <- read.csv("Raw_data/pein_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=pein_seed_set_100)
summary(mod1)
#No seeds produced with 100% treatments. For all p<0.05 and 0.01
#No fruits

#SOLY
soly_seed_set_100 <- read.csv("Raw_data/soly_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=soly_seed_set_100)
summary(mod1)
#3 seeds produced with SIAL 100%. All differ significantly p<0.05 and 0.01
#Check for fruits!

#SOME
some_seed_set_100 <- read.csv("Raw_data/some_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=some_seed_set_100)
summary(mod1)
#36 seeds produced with PEIN 100% , control 0 important to remark.
#All differ significantly p<0.05 and 0.01
#Check for fruits

#CAAN
caan_seed_set_100 <- read.csv("Raw_data/caan_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=caan_seed_set_100)
summary(mod1)
#All differ significantly p<0.05 and 0.01
#127 seeds with SIAL 100% and ersa 100% 3 seeds
#No seeds in control (emasculated flowers)

#BRASSICACEAE
#BROL
brol_seed_set_100 <- read.csv("Raw_data/brol_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=brol_seed_set_100)
summary(mod1)
#All differ significantly p<0.5 and 0.01
#No seeds produced

#BRRA
brra_seed_set_100 <- read.csv("Raw_data/brra_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=brra_seed_set_100)
summary(mod1)
#All differ significantly p<0.5 and 0.01
#BROL 100% 2 and 13 seeds produced for two treatments. Hybridization?

#SIAL
sial_seed_set_100 <- read.csv("Raw_data/sial_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=sial_seed_set_100)
summary(mod1)
#BROL 100% 3 TREATMENTS PRODUCED SEEDS. 7,5,7. Hybridization?
#CAAN 100% 2 TREATMENTS PRODUCED SEEDS. 1 and 6.
#SOLY 100% 1 seed with one treatment. 
#significant differences for all but not with BROL

#ERSA
ersa_seed_set_100 <- read.csv("Raw_data/ersa_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ersa_seed_set_100)
summary(mod1)
#significant differences for all
#2 seeds produced with one treatment of pein

#CONVOLVULACEAE
#IPPU
ippu_seed_set_100 <- read.csv("Raw_data/ippu_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ippu_seed_set_100)
summary(mod1)
#significant differences for all
#No seeds produced with 100% treatments

#IPAQ
ipaq_seed_set_100 <- read.csv("Raw_data/ipaq_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ipaq_seed_set_100)
summary(mod1)
#significant differences for all
#No seeds produced with 100% treatments


#I'm going to compile now the fruit information.


#I'm going to check now the effect of the different donors on seed set
pein_seed_set_final$scaled <- scale(pein_seed_set_final$Seed.production)
soly_seed_set_final$scaled <- scale(soly_seed_set_final$Seed.production)
some_seed_set_final$scaled <- scale(some_seed_set_final$Seed.production)
soly_seed_set_final$scaled <- scale(soly_seed_set_final$Seed.production)
sial_seed_set_final$scaled <- scale(sial_seed_set_final$Seed.production)
ersa_seed_set_final$scaled <- scale(ersa_seed_set_final$Seed.production)
brra_seed_set_final$scaled <- scale(brra_seed_set_final$Seed.production)
brol_seed_set_final$scaled <- scale(brol_seed_set_final$Seed.production)
ipaq_seed_set_final$scaled <- scale(ipaq_seed_set_final$Seed.production)
ippu_seed_set_final$scaled <- scale(ippu_seed_set_final$Seed.production)





       