#In this script I'm going to calculate the absolute effect of traits
#First I have to prepare the data in the desire format for the analysis

#load library
library(reshape2)
library(lme4)

#load effect sizes
matrix_effect_size <- readRDS("Data/matrix_effect_size.RData")
e_size <- melt(matrix_effect_size)
e_size <- as.data.frame(e_size, stringsAsFactors=FALSE)
#Structure of our data
library(ggplot2)
ggplot(e_size, aes(x = value)) + geom_density() 
#seems that a poisson could fit nicely
#load traits
traits <- read.csv("Data/traits_scinames.csv")
#Remove unuseful columns
traits=traits[,-c(1,2,4,10,14)]
#Now change spp name to code to be able to merge
spp <- c("BROL", "BRRA", "CAAN", "ERSA", "IPAQ", "IPPU", "PEIN", "SIAL", "SOLY", "SOME")
traits[,c(2:14)]=scale(traits[,c(2:14)])
traits$species<- spp
traits_recipient=traits
traits_donor=traits
colnames(traits_recipient) <- paste("Recipient", colnames(traits_recipient), sep = "_")
colnames(traits_donor) <- paste("Donor", colnames(traits_donor), sep = "_")
colnames(traits_recipient)[1]<- "Recipient"
colnames(e_size)[1]<- "Recipient"
colnames(e_size)[2]<- "Donor"
colnames(traits_donor)[1]<- "Donor"
effect_recipient <- merge(e_size, traits_recipient, by="Recipient")
str(effect_recipient)
effect_recipient$Donor<- as.character(effect_recipient$Donor)
#NOT WORKING TO MERGE DONORS
#FIX THE SPACE AFTER ALL THE VARIABLES
#DON'T KNOW WHY IS THERE...
effect_recipient$Donor[effect_recipient$Donor=="BROL "] <-"BROL"
effect_recipient$Donor[effect_recipient$Donor=="BRRA "] <-"BRRA"
effect_recipient$Donor[effect_recipient$Donor=="CAAN "] <-"CAAN"
effect_recipient$Donor[effect_recipient$Donor=="ERSA "] <-"ERSA"
effect_recipient$Donor[effect_recipient$Donor=="IPAQ "] <-"IPAQ"
effect_recipient$Donor[effect_recipient$Donor=="IPPU "] <-"IPPU"
effect_recipient$Donor[effect_recipient$Donor=="PEIN "] <-"PEIN"
effect_recipient$Donor[effect_recipient$Donor=="SIAL "] <-"SIAL"
effect_recipient$Donor[effect_recipient$Donor=="SOLY "] <-"SOLY"
effect_recipient$Donor[effect_recipient$Donor=="SOME "] <-"SOME"

mydata<- merge(effect_recipient, traits_donor, by="Donor")
mydata$D <- rep(1:10, each=10)
colnames(data)[3] <- "effect_size"
#ANALYSE
fit <- glm(value~Donor_pollen_size,data=mydata,family=gaussian)
summary(fit)
confint(fit)
exp(coef(fit))
predict(fit, type="response")
residuals(fit, type="deviance")


plot(Oats)
#following an example

model1<-lmer(value~1 + (1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)
model3<-lmer(value~Recipient_si_index + (1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)

model2<-lmer(value~Recipient_stigma_area*Donor_stigma_area+Recipient_style_length*Donor_style_length + (1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)
#How to obtain a p-value by comparing with a "null" model...
summary(model1)
summary(model2)
summary(model3)
anova(model1,model2)
anova(model1,model3)

coef(model1)
