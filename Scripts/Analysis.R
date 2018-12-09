pein_seed_set_final <- read.csv("Rmd/Data/pein_seed_set_final.csv")

#Statistical differences between seed set with treatment

library(dplyr)
library(nlme)
library(lme4)


#PEIN
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

#For tomato just BROL don´t produce a significant decrease in seed set

some_seed_set_final <- read.csv("Rmd/Data/some_seed_set_final.csv")
some_seed_set_final=na.omit(some_seed_set_final)
model2=lme(log(1+Seed.production)~relevel(Treatment,ref="Cross"), data=some_seed_set_final, random=~1|Treatment.number)
summary(model2)
