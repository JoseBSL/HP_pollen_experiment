
#Statistical differences between seed set with treatment

library(dplyr)
library(nlme)
library(lme4)


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