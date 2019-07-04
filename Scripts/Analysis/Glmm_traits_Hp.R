#In this script I'm going to calculate the absolute effect of traits
#First I have to prepare the data in the desire format for the analysis

#load libraries
library(reshape2)
library(lme4)
library(ggplot2)
library(fBasics)
library(lmerTest)
library(jtools)
#
##
###DATA PREPARATION FOR ANALYSES
##
#

#load effect sizes
matrix_effect_size <- readRDS("Data/matrix_effect_size.RData")
e_size <- melt(matrix_effect_size)
e_size <- as.data.frame(e_size, stringsAsFactors=FALSE)
#Structure of our data
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

#Now the data is ready for analysis

#
##
#ANALYSES
##
#

#Perform a Gaussian glmm-> lmer
#Random effects are Donor and recipientdue due to they where not alway the same individuals

#MODEL 1
#DONOR POLLEN SIZE+RECIPIENT STYLE LENGTH+DONOR POLLEN SIZE * RECIPIENT STYLE LENGTH
model1<-lmer(value~Donor_pollen_size+Recipient_style_length+Recipient_style_length*Donor_pollen_size+(1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)
summary(model1)
summ(model1, confint = TRUE, digits = 3)
effect_plot(model1, pred = Donor_pollen_size, interval = TRUE, plot.points = TRUE)
effect_plot(model1, pred = Recipient_style_length, interval = TRUE, plot.points = TRUE)
#Analysing residuals
plot(model1)
qqnorm(residuals(model1))
jarqueberaTest(model1$df.resid)
#MODEL 2
#DONOR POLLEN SIZE+RECIPIENT STYLE LENGTH+DONOR POLLEN SIZE * RECIPIENT STYLE LENGTH+
# + POLLEN_OVULE RATIO
model2<-lmer(value~Donor_pollen_size+Recipient_style_length+Recipient_style_length*Donor_pollen_size+ Recipient_pollen_ovule_ratio+ (1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)
summary(model2)
summ(model2, confint = TRUE, digits = 3)
effect_plot(model2, pred = Donor_pollen_size, interval = TRUE, plot.points = TRUE)
effect_plot(model2, pred = Recipient_style_length, interval = TRUE, plot.points = TRUE)
effect_plot(model2, pred = Recipient_pollen_ovule_ratio, interval = TRUE, plot.points = TRUE)
plot(model2)
qqnorm(residuals(model2))
#MODEL 3
#DONOR POLLEN SIZE+RECIPIENT STYLE LENGTH+DONOR POLLEN SIZE * RECIPIENT STYLE LENGTH+
# + RECIPIENT SELF INCOMPATIBILITY INDEX
model3<-lmer(value~Donor_pollen_size+Recipient_style_length+Recipient_style_length*Donor_pollen_size+ Recipient_si_index+(1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)
summary(model3)
summ(model3, confint = TRUE, digits = 3)
effect_plot(model3, pred = Donor_pollen_size, interval = TRUE, plot.points = TRUE)
effect_plot(model3, pred = Recipient_style_length, interval = TRUE, plot.points = TRUE)
effect_plot(model3, pred = Recipient_si_index, interval = TRUE, plot.points = TRUE)
plot(model3)
qqnorm(residuals(model3))
#MODEL 4
#DONOR SELF INCOMPATIBILITY INDEX+RECIPIENT SELF INCOMPATIBILITY INDEX
model4<-lmer(value~Donor_si_index*Recipient_si_index+ (1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)
summary(model4)
summ(model4,  digits = 3)
effect_plot(model4, pred = Donor_si_index, interval = TRUE, plot.points = TRUE)
effect_plot(model4, pred = Recipient_si_index, interval = TRUE, plot.points = TRUE)
plot(model4)
qqnorm(residuals(model4))

model4<-lmer(value~Recipient_style_length*Donor_si_index+ (1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)
summary(model4)
summ(model4,  digits = 3)
effect_plot(model4, pred = Donor_si_index, interval = TRUE, plot.points = TRUE)
effect_plot(model4, pred = Recipient_si_index, interval = TRUE, plot.points = TRUE)
plot(model4)
qqnorm(residuals(model4))


#Interpreting outputs:
#Random effects, Recipient have a big standard deviation which means that great part of variability
#of our model can be explained by them. 
model1<-lmer(value~1 + (1|D),data=mydata,REML=FALSE)
model3<-lmer(value~Recipient_si_index + (1|Donor)+ (1|Recipient),data=mydata,REML=FALSE)

#How to obtain a p-value by comparing with a "null" model...

#Analysing residuals
plot(model2)
qqnorm(residuals(model2))
jarqueberaTest(model2$df.resid)

#Same analysis without Convolvulaceae

mydata
library(dplyr)
filter_data <- filter(mydata, Recipient != "IPPU" & Recipient != "IPAQ")
filter_data <- filter(filter_data, Donor != "IPPU" & Donor != "IPAQ")


model1<-lmer(value~Recipient_stigma_area*Donor_pollen_size+ Recipient_pollen_ovule_ratio+(1|Donor)+ (1|Recipient),data=filter_data,REML=FALSE)
summary(model1)

model1.1<-lmer(value~Recipient_stigma_area*Donor_pollen_size+(1|Donor)+ (1|Recipient),data=filter_data,REML=FALSE)
summary(model1.1)

model1<-lmer(value~Recipient_stigma_area*Donor_pollen_size+ (1|Donor)+ (1|Recipient),data=filter_data,REML=FALSE)
summary(model1)

model2<-lmer(value~Recipient_style_length*Donor_pollen_size+Recipient_pollen_ovule_ratio+ (1|Donor)+ (1|Recipient),data=filter_data,REML=FALSE)
summary(model2)

model3<-lmer(value~Donor_pollen_size*Recipient_pollen_size+Recipient_style_length+ (1|Donor)+ (1|Recipient),data=filter_data,REML=FALSE)
summary(model3)
