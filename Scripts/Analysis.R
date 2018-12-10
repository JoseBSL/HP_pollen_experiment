
#Statistical differences between seed set with treatment

library(dplyr)
library(nlme)
library(lme4)

#50-50% pollen analysis respect cross. At the moment seed set is not scaled for this case.

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
summary(model2)
#For Petunia BRRA, CAAN  and ERSA didn´t reduce seed set significatively 

#SOLY
soly_seed_set_final <- read.csv("Rmd/Data/soly_seed_set_final.csv")
soly_seed_set_final=na.omit(soly_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=soly_seed_set_final, random=~1|Treatment.number)
summary(model2)

#For tomato just BROL don´t produce a significant decrease in seed set p<0.05

#SOME
some_seed_set_final <- read.csv("Rmd/Data/some_seed_set_final.csv")
some_seed_set_final=na.omit(some_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=some_seed_set_final, random=~1|Treatment.number)
summary(model2)
#SOLY, PEIN, IPPU,IPAQ, BROL p<0.05

#CAAN
caan_seed_set_final <- read.csv("Rmd/Data/caan_seed_set_final.csv")
caan_seed_set_final=na.omit(caan_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="CROSS"), data=caan_seed_set_final, random=~1|Treatment.number)
summary(model2)
#Eruca versicaria is the only species that didn't reduce seed set p<0.05

#BROL
brol_seed_set_final <- read.csv("Rmd/Data/brol_seed_set_final.csv")
brol_seed_set_final=na.omit(brol_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=brol_seed_set_final, random=~1|Treatment.number)
summary(model2)
#All different from cross p<0.05

#BRRA
brra_seed_set_final <- read.csv("Rmd/Data/brra_seed_set_final.csv")
brra_seed_set_final=na.omit(brra_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=brra_seed_set_final, random=~1|Treatment.number)
summary(model2)
#al different from cross p<0.05

#SIAL
sial_seed_set_final <- read.csv("Rmd/Data/sial_seed_set_final.csv")
sial_seed_set_final=na.omit(sial_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=sial_seed_set_final, random=~1|Treatment.number)
summary(model2)
#BROL, ERSA, PEIN, SOLY, SOME

#ERSA
ersa_seed_set_final <- read.csv("Rmd/Data/ersa_seed_set_final.csv")
ersa_seed_set_final=na.omit(ersa_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=ersa_seed_set_final, random=~1|Treatment.number)
summary(model2)
#BRRA, CAAN, PEIN, IPPU, SIAL, SOLY, SOME

#IPPU
ippu_seed_set_final <- read.csv("Rmd/Data/ippu_seed_set_final.csv")
str(ippu_seed_set_final)
ippu_seed_set_final$Family<- "family"

ippu_seed_set_final=na.omit(ippu_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=ippu_seed_set_final, random=~1|Treatment.number)
summary(model2)
#SOME,BRRA, PEIN,IPAQ,SIAL,BROL

#IPAQ
ipaq_seed_set_final <- read.csv("Rmd/Data/ipaq_seed_set_final.csv")
ipaq_seed_set_final$Family<- "family"

ipaq_seed_set_final=na.omit(ipaq_seed_set_final)

model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=ipaq_seed_set_final, random=~1|Treatment.number)
summary(model2)
#SOME, BRRA, PEIN, IPAQ, SIAL, BROL

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
