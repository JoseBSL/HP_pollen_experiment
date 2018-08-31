pein_seed_set_final <- read.csv("Rmd/Data/pein_seed_set_final.csv")

#Statistical differences between seed set with treatment

library(dplyr)

pein_seed_set_final_TEST <- subset(pein_seed_set_final, Treatment==c("CROSS") | Treatment==("Brassica oleracea"))

t.test(Seed.production~Treatment, data=pein_seed_set_final_TEST)
str(pein_seed_set_final)
mod1=lm(log(1+Seed.production)~relevel(Treatment,ref="CROSS"),data=pein_seed_set_final)
mod1
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
TukeyHSD(aov(mod1))
