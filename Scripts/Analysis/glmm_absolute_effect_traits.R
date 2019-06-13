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


new <- merge(effect_recipient, traits_donor, by="Donor")

str(traits_recipient)
e_size$Donor=as.factor(e_size$Donor)
new <- merge(effect_recipient, traits_donor, by="Donor", all=TRUE)




# For the Donor is not working, when I check the levels there are some space that give uses
levels(e_size$Donor)
#Fix BRRA, CAAN, ERSA, IPAQ, IPPU, PEIN, SIAL, SOLY, SOME
# I fix them here
e_size$Donor=as.character(e_size$Donor)
e_size$Donor[e_size$Donor=="BROL "] <-"BROL"
e_size$Donor[e_size$Donor=="BRRA "] <-"BRRA"
e_size$Donor[e_size$Donor=="CAAN "] <-"CAAN"
e_size$Donor[e_size$Donor=="ERSA "] <-"ERSA"
e_size$Donor[e_size$Donor=="IPAQ "] <-"IPAQ"
e_size$Donor[e_size$Donor=="IPPU "] <-"IPPU"
e_size$Donor[e_size$Donor=="PEIN "] <-"PEIN"
e_size$Donor[e_size$Donor=="SIAL "] <-"SIAL"
e_size$Donor[e_size$Donor=="SOLY "] <-"SOLY"
e_size$Donor[e_size$Donor=="SOME "] <-"SOME"
effect_recipient <- merge(e_size, traits_recipient, by="Recipient")
e_size$Donor=as.factor(e_size$Donor)
effect_donor <- merge(e_size, traits_donor, by="Donor")
merge(effect_recipient, effect_donor, by="Recipient")
#Ok, now I'm able to merge
#Now 
all <- cbind(effect_recipient,effect_donor)


#Simplistic model to apply
owls.lmer <- lmer(SiblingNegotiation ~ FoodTreatment * SexParent + 
                    BroodSize + (1 | Nest), family = gaussian, data = owls)

#check residuals, homogeinity of the variance
plot(residuals(owls.lmer) ~ fitted(owls.lmer))
#check for non-normality
eblups <- as.vector(unlist(ranef(owls.lmer)))
qqnorm(eblups)
abline(0, sd(eblups))



