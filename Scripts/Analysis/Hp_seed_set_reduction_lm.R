
#Statistical differences between seed set with treatment

library(dplyr)
library(nlme)
library(lme4)
library(reshape2)
library(ggplot2)
library(jtools)
#50-50% pollen analysis respect cross. At the moment seed set is not scaled for this case.
#I do it for each species separately
#PEIN
pein_seed_set_final <- read.csv("Rmd/Data/pein_seed_set_final.csv")
pein_seed_set_final_TEST <- subset(pein_seed_set_final, Treatment==c("CROSS") | Treatment==("Brassica oleracea"))

t.test(Seed.production~Treatment, data=pein_seed_set_final_TEST)
str(pein_seed_set_final)
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=pein_seed_set_final)
mod1
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
TukeyHSD(aov(mod1))
pein_seed_set_final=na.omit(pein_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="CROSS"), data=pein_seed_set_final, random=~1|Treatment.number)
pein <- summary(model2)$tTable
#For Petunia BRRA, CAAN  and ERSA didn´t reduce seed set significatively 

#SOLY
soly_seed_set_final <- read.csv("Rmd/Data/soly_seed_set_final.csv")
soly_seed_set_final=na.omit(soly_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=soly_seed_set_final, random=~1|Treatment.number)
summary(model2)
soly <- summary(model2)$tTable
#For tomato just BROL don´t produce a significant decrease in seed set p<0.05

#SOME
some_seed_set_final <- read.csv("Rmd/Data/some_seed_set_final.csv")
some_seed_set_final=na.omit(some_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=some_seed_set_final, random=~1|Treatment.number)
summary(model2)
some <- summary(model2)$tTable

#SOLY, PEIN, IPPU,IPAQ, BROL p>0.05

#CAAN
caan_seed_set_final <- read.csv("Rmd/Data/caan_seed_set_final.csv")
caan_seed_set_final=na.omit(caan_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="CROSS"), data=caan_seed_set_final, random=~1|Treatment.number)
summary(model2)
caan <- summary(model2)$tTable

#Eruca versicaria is the only species that didn't reduce seed set p<0.05

#BROL
brol_seed_set_final <- read.csv("Rmd/Data/brol_seed_set_final.csv")
brol_seed_set_final=na.omit(brol_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=brol_seed_set_final, random=~1|Treatment.number)
summary(model2)
brol <- summary(model2)$tTable

#All different from cross p<0.05

#BRRA
brra_seed_set_final <- read.csv("Rmd/Data/brra_seed_set_final.csv")
brra_seed_set_final=na.omit(brra_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=brra_seed_set_final, random=~1|Treatment.number)
summary(model2)
brra <- summary(model2)$tTable

#al different from cross p<0.05

#SIAL
sial_seed_set_final <- read.csv("Rmd/Data/sial_seed_set_final.csv")
sial_seed_set_final=na.omit(sial_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=sial_seed_set_final, random=~1|Treatment.number)
summary(model2)
sial <- summary(model2)$tTable

#BROL, ERSA, PEIN, SOLY, SOME

#ERSA
ersa_seed_set_final <- read.csv("Rmd/Data/ersa_seed_set_final.csv")
ersa_seed_set_final=na.omit(ersa_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=ersa_seed_set_final, random=~1|Treatment.number)
summary(model2)
ersa <- summary(model2)$tTable

#BRRA, CAAN, PEIN, IPPU, SIAL, SOLY, SOME

#IPPU
ippu_seed_set_final <- read.csv("Rmd/Data/ippu_seed_set_final.csv")
str(ippu_seed_set_final)
ippu_seed_set_final$Family<- "family"
ippu_seed_set_final=na.omit(ippu_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=ippu_seed_set_final, random=~1|Treatment.number)
summary(model2)
ippu <- summary(model2)$tTable

#SOME,BRRA, PEIN,IPAQ,SIAL,BROL

#IPAQ
ipaq_seed_set_final <- read.csv("Rmd/Data/ipaq_seed_set_final.csv")
ipaq_seed_set_final$Family<- "family"

ipaq_seed_set_final=na.omit(ipaq_seed_set_final)

model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=ipaq_seed_set_final, random=~1|Treatment.number)
summary(model2)
ipaq <- summary(model2)$tTable

#saverds files to load them in other script

saveRDS(pein, "Data/pein.RData")
saveRDS(soly, "Data/soly.RData")
saveRDS(caan, "Data/caan.RData")
saveRDS(some, "Data/some.RData")
saveRDS(brol, "Data/brol.RData")
saveRDS(brra, "Data/brra.RData")
saveRDS(ersa, "Data/ersa.RData")
saveRDS(sial, "Data/sial.RData")
saveRDS(ippu, "Data/ippu.RData")
saveRDS(ipaq, "Data/ipaq.RData")








#ALL

#100% heterospecific pollen analysis respect cross

#SOLANACEAE
#PEIN
pein_seed_set_100 <- read.csv("Rmd/Data/pein_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=pein_seed_set_100)
summary(mod1)
#No seeds produced with 100% treatments. For all p<0.05 and 0.01
#No fruits

#SOLY
soly_seed_set_100 <- read.csv("Rmd/Data/soly_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=soly_seed_set_100)
summary(mod1)
#3 seeds produced with SIAL 100%. All differ significantly p<0.05 and 0.01
#Check for fruits!

#SOME
some_seed_set_100 <- read.csv("Rmd/Data/some_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=some_seed_set_100)
summary(mod1)
#36 seeds produced with PEIN 100% , control 0 important to remark.
#All differ significantly p<0.05 and 0.01
#Check for fruits

#CAAN
caan_seed_set_100 <- read.csv("Rmd/Data/caan_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=caan_seed_set_100)
summary(mod1)
#All differ significantly p<0.05 and 0.01
#127 seeds with SIAL 100% and ersa 100% 3 seeds
#No seeds in control (emasculated flowers)

#BRASSICACEAE
#BROL
brol_seed_set_100 <- read.csv("Rmd/Data/brol_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=brol_seed_set_100)
summary(mod1)
#All differ significantly p<0.5 and 0.01
#No seeds produced

#BRRA
brra_seed_set_100 <- read.csv("Rmd/Data/brra_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=brra_seed_set_100)
summary(mod1)
#All differ significantly p<0.5 and 0.01
#BROL 100% 2 and 13 seeds produced for two treatments. Hybridization?

#SIAL
sial_seed_set_100 <- read.csv("Rmd/Data/sial_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=sial_seed_set_100)
summary(mod1)
#BROL 100% 3 TREATMENTS PRODUCED SEEDS. 7,5,7. Hybridization?
#CAAN 100% 2 TREATMENTS PRODUCED SEEDS. 1 and 6.
#SOLY 100% 1 seed with one treatment. 
#significant differences for all but not with BROL

#ERSA
ersa_seed_set_100 <- read.csv("Rmd/Data/ersa_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ersa_seed_set_100)
summary(mod1)
#significant differences for all
#2 seeds produced with one treatment of pein

#CONVOLVULACEAE
#IPPU
ippu_seed_set_100 <- read.csv("Rmd/Data/ippu_seed_set_100.csv")
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="Cross"),data=ippu_seed_set_100)
summary(mod1)
#significant differences for all
#No seeds produced with 100% treatments

#IPAQ
ipaq_seed_set_100 <- read.csv("Rmd/Data/ipaq_seed_set_100.csv")
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

scaled_seed <- rbind(pein_seed_set_final,soly_seed_set_final)
str(scaled_seed)
scaled_seed$Family=as.character(scaled_seed$Family)
scaled_seed<- subset(scaled_seed, Family!="other")

library(lme4)

hist(scaled_seed$scaled)

model_1<- lmer(scaled ~ Treatment + (1|Treatment.number),data=scaled_seed)
summary(model_1)
print(model_1, corr=F)


library(nlme)
model_1_1 <- lme(scaled~Treatment,random=~1|Treatment.number,data=scaled_seed)
summary(model_1_1)
anova(model_1_1)

#Let's try to approach in another way (Romina suggestion)

seed_set <- read.csv("Data/species_seed_set.csv")
head(seed_set)
seed_set=na.omit(seed_set)
model_1_1 <- lme(Scaled_seed_set~Treatment,random=~1|Treatment.number,data=seed_set)
summary(model_1_1)

seed_set_focal <- dcast(Species + Treatment ~ ., value.var = "Scaled_seed_set", fun.aggregate = mean, data = seed_set, na.rm= TRUE)
seed_set_focal=subset(seed_set_focal, Treatment!="Control"& Treatment!="Cross"& Treatment!="Self" & Treatment!="Flower control")
colnames(seed_set_focal)[3] <- "scaled_seed_set"

seed_set_focal_mean <- dcast(Species  ~ ., value.var = "scaled_seed_set", fun.aggregate = mean, data = seed_set_focal, na.rm= TRUE)
colnames(seed_set_focal_mean)[2] <- "mean"

seed_set_focal_sd <- dcast(Species  ~ ., value.var = "scaled_seed_set", fun.aggregate = sd, data = seed_set_focal, na.rm= TRUE)
colnames(seed_set_focal_sd)[2] <- "sd"

seed_set_focal=merge(seed_set_focal_mean, seed_set_focal_sd, by="Species")
#This is the average effect for the 10 spp as a recipient 
# Make the graph with sd

Recipient=ggplot(seed_set_focal, aes(x=Species, y=mean, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(shape=21, size=3, fill="white")+ggtitle("Recipient average effect")
print(Recipient + labs(title= "Recipient average effect",
                   y="Mean +/- sd", x = "Species"))
mynamestheme <- theme(axis.text = element_text(family = "Courier", colour = "black", size = (6)))
print(Recipient +mynamestheme+ labs(title= "Recipient average seed set",
                                y="Mean +/- sd", x = "Species"))

seed_set_non_focal=subset(seed_set, Treatment!="Control"& Treatment!="Cross"& Treatment!="Self" & Treatment!="Flower control")

seed_set_non_focal_mean <- dcast(Treatment  ~ ., value.var = "Scaled_seed_set", fun.aggregate = mean, data = seed_set_non_focal, na.rm= TRUE)
colnames(seed_set_non_focal_mean)[2] <- "mean"

seed_set_non_focal_sd <- dcast(Treatment  ~ ., value.var = "Scaled_seed_set", fun.aggregate = sd, data = seed_set_non_focal, na.rm= TRUE)
colnames(seed_set_non_focal_sd)[2] <- "sd"

seed_set_non_focal=merge(seed_set_non_focal_mean, seed_set_non_focal_sd, by="Treatment")

Donor=ggplot(seed_set_non_focal, aes(x=Treatment, y=mean, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=mean-sd, ymax=mean+sd)) +
  geom_point(shape=21, size=3, fill="white")+ggtitle("Donor average seed set")
mynamestheme <- theme(axis.text = element_text(family = "Courier", colour = "black", size = (6)))
print(Donor +mynamestheme+ labs(title= "Donor average effect",
                      y="Mean +/- sd", x = "Species"))



