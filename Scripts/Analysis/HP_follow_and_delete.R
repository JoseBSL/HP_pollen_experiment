##############################################################################################################################
#Statistical differences between seed set with the different treatment
##############################################################################################################################
#load libraries
library(dplyr)
library(nlme)
library(lme4)
library(forcats)
library(jtools)
library(fitdistrplus)

#50-50% pollen analysis treatments vs cross (control). GLM WITH QUASIPOISSON

##############################################################################################################################
#
#
#SOLANACEAE 1st
#
#
##############################################################################################################################
#PEIN
#read data
pein_seed_set_final <- read.csv("Raw_data/pein_seed_set_final.csv", stringsAsFactors = T)
#filter data
pein_seed_set_final <- subset(pein_seed_set_final, Treatment!="FLOWER CONTROL" & Treatment!="CONTROL" & Treatment!="SELF")
#remove NA´S
pein_seed_set_final=na.omit(pein_seed_set_final)
#convert to factor
pein_seed_set_final$Treatment <- factor(pein_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(pein_seed_set_final$Treatment)
pein_seed_set_final$Treatment <- relevel(pein_seed_set_final$Treatment, ref="CROSS")
#scale data
pein_seed_set_final$Seed.production <- scale(pein_seed_set_final$Seed.production)
#run linear model
lm_pein=lm(Seed.production~Treatment, data=pein_seed_set_final)
summary(lm_pein)
#save output on a dataframe
summary(lm_pein)
lm_pein <- summ(lm_pein, model.info=FALSE, model.fit=FALSE)
lm_pein <- as.data.frame(lm_pein$coeftable)
rownames(lm_pein) <- gsub("Treatment", "", rownames(lm_pein))
rownames(lm_pein) <- paste("M1 Petunia integrifolia", rownames(lm_pein), sep=" - ")

##############################################################################################################################
#SOLY
#read data
soly_seed_set_final <- read.csv("Raw_data/soly_seed_set_final.csv", stringsAsFactors = T)
#filter data
soly_seed_set_final <- subset(soly_seed_set_final, Treatment!="Flower control" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
soly_seed_set_final=na.omit(soly_seed_set_final)
#convert to factor
soly_seed_set_final$Treatment <- factor(soly_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(soly_seed_set_final$Treatment)
soly_seed_set_final$Treatment <- relevel(soly_seed_set_final$Treatment, ref="Cross")
#scale data
soly_seed_set_final$Seed.production <- scale(soly_seed_set_final$Seed.production)
#run linear model
lm_soly=lm(Seed.production~Treatment, data=soly_seed_set_final)
summary(lm_soly)
#save output on a dataframe
summary(lm_soly)
lm_soly <- summ(lm_soly, model.info=FALSE, model.fit=FALSE)
lm_soly <- as.data.frame(lm_soly$coeftable)
rownames(lm_soly) <- gsub("Treatment", "", rownames(lm_soly))
rownames(lm_soly) <- paste("M2 Solanum lycopersicum", rownames(lm_soly), sep=" - ")

##############################################################################################################################
#SOME
#read data
some_seed_set_final <- read.csv("Raw_data/some_seed_set_final.csv", stringsAsFactors = T)
#filter data
some_seed_set_final <- subset(some_seed_set_final, Treatment!="Flower control" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
some_seed_set_final=na.omit(some_seed_set_final)
#convert to factor
some_seed_set_final$Treatment <- factor(some_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(some_seed_set_final$Treatment)
some_seed_set_final$Treatment <- relevel(some_seed_set_final$Treatment, ref="Cross")
#scale data
some_seed_set_final$Seed.production <- scale(some_seed_set_final$Seed.production)
#run linear model
lm_some=lm(Seed.production~Treatment, data=some_seed_set_final)
summary(lm_some)
#save output on a dataframe
summary(lm_some)
lm_some <- summ(lm_some, model.info=FALSE, model.fit=FALSE)
lm_some <- as.data.frame(lm_some$coeftable)
rownames(lm_some) <- gsub("Treatment", "", rownames(lm_some))
rownames(lm_some) <- paste("M3 Solanum melongena", rownames(lm_some), sep=" - ")

##############################################################################################################################
#CAAN
#read data
caan_seed_set_final <- read.csv("Raw_data/caan_seed_set_final.csv", stringsAsFactors = T)
#filter data
caan_seed_set_final <- subset(caan_seed_set_final, Treatment!="FLOWER CONTROL" & Treatment!="CONTROL" & Treatment!="SELF")
#remove NA´S
caan_seed_set_final=na.omit(caan_seed_set_final)
#convert to factor
caan_seed_set_final$Treatment <- factor(caan_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(caan_seed_set_final$Treatment)
caan_seed_set_final$Treatment <- relevel(caan_seed_set_final$Treatment, ref="CROSS")
#scale data
caan_seed_set_final$Seed.production <- scale(caan_seed_set_final$Seed.production)
#run linear model
lm_caan=lm(Seed.production~Treatment, data=caan_seed_set_final)
summary(lm_caan)
#save output on a dataframe
summary(lm_caan)
lm_caan <- summ(lm_caan, model.info=FALSE, model.fit=FALSE)
lm_caan <- as.data.frame(lm_caan$coeftable)
rownames(lm_caan) <- gsub("Treatment", "", rownames(lm_caan))
rownames(lm_caan) <- paste("M4 Capsicum annuum", rownames(lm_caan), sep=" - ")


##############################################################################################################################
#
#
#BRASSICACEAE 2nd
#
#
##############################################################################################################################

#BROL
#read data
brol_seed_set_final <- read.csv("Raw_data/brol_seed_set_final.csv", stringsAsFactors = T)
#filter data
brol_seed_set_final <- subset(brol_seed_set_final, Treatment!="FC" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
brol_seed_set_final=na.omit(brol_seed_set_final)
#convert to factor
brol_seed_set_final$Treatment <- factor(brol_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(brol_seed_set_final$Treatment)
brol_seed_set_final$Treatment <- relevel(brol_seed_set_final$Treatment, ref="Cross")
#scale data
brol_seed_set_final$Seed.production <- scale(brol_seed_set_final$Seed.production)
#run linear model
lm_brol=lm(Seed.production~Treatment, data=brol_seed_set_final)
summary(lm_brol)
#save output on a dataframe
summary(lm_brol)
lm_brol <- summ(lm_brol, model.info=FALSE, model.fit=FALSE)
lm_brol <- as.data.frame(lm_brol$coeftable)
rownames(lm_brol) <- gsub("Treatment", "", rownames(lm_brol))
rownames(lm_brol) <- paste("M5 Brassica oleracea", rownames(lm_brol), sep=" - ")



##############################################################################################################################

#BRRA
#read data
brra_seed_set_final <- read.csv("Raw_data/brra_seed_set_final.csv", stringsAsFactors = T)
#filter data
brra_seed_set_final <- subset(brra_seed_set_final, Treatment!="Flower Control" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
brra_seed_set_final=na.omit(brra_seed_set_final)
#convert to factor
brra_seed_set_final$Treatment <- factor(brra_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(brra_seed_set_final$Treatment)
brra_seed_set_final$Treatment <- relevel(brra_seed_set_final$Treatment, ref="Cross")
#scale data
brra_seed_set_final$Seed.production <- scale(brra_seed_set_final$Seed.production)
#run linear model
lm_brra=lm(Seed.production~Treatment, data=brra_seed_set_final)
summary(lm_brra)
#save output on a dataframe
summary(lm_brra)
lm_brra <- summ(lm_brra, model.info=FALSE, model.fit=FALSE)
lm_brra <- as.data.frame(lm_brra$coeftable)
rownames(lm_brra) <- gsub("Treatment", "", rownames(lm_brra))
rownames(lm_brra) <- paste("M6 Brassica rapa", rownames(lm_brra), sep=" - ")

##############################################################################################################################

#SIAL
#read data
sial_seed_set_final <- read.csv("Raw_data/sial_seed_set_final.csv", stringsAsFactors = T)
#filter data
sial_seed_set_final <- subset(sial_seed_set_final, Treatment!="Flower Control" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
sial_seed_set_final=na.omit(sial_seed_set_final)
#convert to factor
sial_seed_set_final$Treatment <- factor(sial_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(sial_seed_set_final$Treatment)
sial_seed_set_final$Treatment <- relevel(sial_seed_set_final$Treatment, ref="Cross")
#scale data
sial_seed_set_final$Seed.production <- scale(sial_seed_set_final$Seed.production)
#run linear model
lm_sial=lm(Seed.production~Treatment, data=sial_seed_set_final)
summary(lm_sial)
#save output on a dataframe
summary(lm_sial)
lm_sial <- summ(lm_sial, model.info=FALSE, model.fit=FALSE)
lm_sial <- as.data.frame(lm_sial$coeftable)
rownames(lm_sial) <- gsub("Treatment", "", rownames(lm_sial))
rownames(lm_sial) <- paste("M7 Sinapis alba", rownames(lm_sial), sep=" - ")
##############################################################################################################################

#ERSA
#read data
ersa_seed_set_final <- read.csv("Raw_data/ersa_seed_set_final.csv", stringsAsFactors = T)
#filter data
ersa_seed_set_final <- subset(ersa_seed_set_final, Treatment!="Flower Control" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
ersa_seed_set_final=na.omit(ersa_seed_set_final)
#convert to factor
ersa_seed_set_final$Treatment <- factor(ersa_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(ersa_seed_set_final$Treatment)
ersa_seed_set_final$Treatment <- relevel(ersa_seed_set_final$Treatment, ref="Cross")
#scale data
ersa_seed_set_final$Seed.production <- scale(ersa_seed_set_final$Seed.production)
#run linear model
lm_ersa=lm(Seed.production~Treatment, data=ersa_seed_set_final)
summary(lm_ersa)
#save output on a dataframe
summary(lm_ersa)
lm_ersa <- summ(lm_ersa, model.info=FALSE, model.fit=FALSE)
lm_ersa <- as.data.frame(lm_ersa$coeftable)
rownames(lm_ersa) <- gsub("Treatment", "", rownames(lm_ersa))
rownames(lm_ersa) <- paste("M8 Eruca sativa", rownames(lm_ersa), sep=" - ")

##############################################################################################################################
#
#
#CONVOLVULACEAE 3rd
#
#
##############################################################################################################################

#IPPU
#read data
ippu_seed_set_final <- read.csv("Raw_data/ippu_seed_set_final.csv", stringsAsFactors = T)
#filter data
ippu_seed_set_final <- subset(ippu_seed_set_final, Treatment!="Flower control" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
ippu_seed_set_final=na.omit(ippu_seed_set_final)
#convert to factor
ippu_seed_set_final$Treatment <- factor(ippu_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(ippu_seed_set_final$Treatment)
ippu_seed_set_final$Treatment <- relevel(ippu_seed_set_final$Treatment, ref="Cross")
#scale data
ippu_seed_set_final$Seed.production <- scale(ippu_seed_set_final$Seed.production)
#run linear model
lm_ippu=lm(Seed.production~Treatment, data=ippu_seed_set_final)
summary(lm_ippu)
#save output on a dataframe
summary(lm_ippu)
lm_ippu <- summ(lm_ippu, model.info=FALSE, model.fit=FALSE)
lm_ippu <- as.data.frame(lm_ippu$coeftable)
rownames(lm_ippu) <- gsub("Treatment", "", rownames(lm_ippu))
rownames(lm_ippu) <- paste("M9 Ipomoea purpurea", rownames(lm_ippu), sep=" - ")
##############################################################################################################################




#IPPU
#read data
ipaq_seed_set_final <- read.csv("Raw_data/ipaq_seed_set_final.csv", stringsAsFactors = T)
#filter data
ipaq_seed_set_final <- subset(ipaq_seed_set_final, Treatment!="Flower control" & Treatment!="Ipomoea aquatica" & Treatment!="Control" & Treatment!="Self")
#remove NA´S
ipaq_seed_set_final=na.omit(ipaq_seed_set_final)
#convert to factor
ipaq_seed_set_final$Treatment <- factor(ipaq_seed_set_final$Treatment)
#set as reference level the cross treatment
levels(ipaq_seed_set_final$Treatment)
ipaq_seed_set_final$Treatment <- relevel(ipaq_seed_set_final$Treatment, ref="Cross")
#scale data
ipaq_seed_set_final$Seed.production <- scale(ipaq_seed_set_final$Seed.production)
#run linear model
lm_ipaq=lm(Seed.production~Treatment, data=ipaq_seed_set_final)
summary(lm_ipaq)
#save output on a dataframe
summary(lm_ipaq)
lm_ipaq <- summ(lm_ipaq, model.info=FALSE, model.fit=FALSE)
lm_ipaq <- as.data.frame(lm_ipaq$coeftable)
rownames(lm_ipaq) <- gsub("Treatment", "", rownames(lm_ipaq))
rownames(lm_ipaq) <- paste("M10 Ipomoea aquatica", rownames(lm_ipaq), sep=" - ")


table <- rbind(lm_pein, lm_soly, lm_some , lm_caan, lm_brol, lm_brra, lm_ersa, lm_ippu, lm_ipaq)
#fixing species name
gsub("vesicaria", "sativa", rownames(table))
#save dataframe with all outputs
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

##############################################################################################################################
#
##
####
#####NOW I ANALYSYS 100% POLLEN TREATMENTS
####
##
#
##############################################################################################################################

#SOLANACEAE
#PEIN
pein_seed_set_100 <- read.csv("Raw_data/pein_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=pein_seed_set_100)
summary(mod1)
#No seeds produced with 100% treatments. For all p<0.05 and 0.01
#No fruits
##############################################################################################################################

#SOLY
soly_seed_set_100 <- read.csv("Raw_data/soly_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=soly_seed_set_100)
summary(mod1)
#3 seeds produced with SIAL 100%. All differ significantly p<0.05 and 0.01
#Check for fruits!
##############################################################################################################################

#SOME
some_seed_set_100 <- read.csv("Raw_data/some_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=some_seed_set_100)
summary(mod1)
#36 seeds produced with PEIN 100% , control 0 important to remark.
#All differ significantly p<0.05 and 0.01
#Check for fruits
##############################################################################################################################

#CAAN
caan_seed_set_100 <- read.csv("Raw_data/caan_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=caan_seed_set_100)
summary(mod1)
#All differ significantly p<0.05 and 0.01
#127 seeds with SIAL 100% and ersa 100% 3 seeds
#No seeds in control (emasculated flowers)
##############################################################################################################################

#BRASSICACEAE
#BROL
brol_seed_set_100 <- read.csv("Raw_data/brol_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=brol_seed_set_100)
summary(mod1)
#All differ significantly p<0.5 and 0.01
#No seeds produced
##############################################################################################################################

#BRRA
brra_seed_set_100 <- read.csv("Raw_data/brra_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=brra_seed_set_100)
summary(mod1)
#All differ significantly p<0.5 and 0.01
#BROL 100% 2 and 13 seeds produced for two treatments. Hybridization?
##############################################################################################################################

#SIAL
sial_seed_set_100 <- read.csv("Raw_data/sial_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=sial_seed_set_100)
summary(mod1)
#BROL 100% 3 TREATMENTS PRODUCED SEEDS. 7,5,7. Hybridization?
#CAAN 100% 2 TREATMENTS PRODUCED SEEDS. 1 and 6.
#SOLY 100% 1 seed with one treatment. 
#significant differences for all but not with BROL
##############################################################################################################################

#ERSA
ersa_seed_set_100 <- read.csv("Raw_data/ersa_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ersa_seed_set_100)
summary(mod1)
#significant differences for all
#2 seeds produced with one treatment of pein
##############################################################################################################################

#CONVOLVULACEAE
#IPPU
ippu_seed_set_100 <- read.csv("Raw_data/ippu_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ippu_seed_set_100)
summary(mod1)
#significant differences for all
#No seeds produced with 100% treatments
##############################################################################################################################

#IPAQ
ipaq_seed_set_100 <- read.csv("Raw_data/ipaq_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ipaq_seed_set_100)
summary(mod1)
#significant differences for all
#No seeds produced with 100% treatments
##############################################################################################################################



