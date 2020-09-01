#################################################################################
#################################################################################
#################################################################################
########################## GENERAL MODEL OF TRAITS ##############################
#################################################################################
#################################################################################
#################################################################################

####################################
############ load libraries ########
####################################

library(reshape2)
library(DHARMa) #goodness of fit
library(ggplot2)
library(lme4)
library(lmerTest) #add pvalues to model
library(car) #check vif 
library(MuMIn) #Get R2
library(ggthemes)
library(ggsci)

#######################################
############ Data preparataion ########
#######################################

#load effect sizes
matrix_effect_size <- readRDS("Data/RData/matrix_effect_size.RData")
e_size <- melt(matrix_effect_size)
e_size <- as.data.frame(e_size, stringsAsFactors=FALSE)
#Structure of our data
ggplot(e_size, aes(x = value)) + geom_density() 
#seems that a poisson could fit nicely
#load traits
traits <- read.csv("Data/Csv/traits_scinames.csv")
#Remove unuseful columns
traits=traits[,-c(1,2,4,10,14)]
#Now change spp name to code to be able to merge
spp <- c("BROL", "BRRA", "CAAN", "ERSA", "IPAQ", "IPPU", "PEIN", "SIAL", "SOLY", "SOME")
#traits[,c(2:14)]=scale(traits[,c(2:14)])

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


#MERGE EFFECT SIZES AND TRAITS
mydata<- merge(effect_recipient, traits_donor, by="Donor")
mydata$D <- rep(1:10, each=10)
colnames(mydata)[3] <- "effect_size"
mydata$id_number <- seq(length(mydata$Recipient))

#FOR OTHER ANALYSIS
#ovule_number <- unique(mydata[,c(2,7)])
#saveRDS(ovule_number, "Data/RData/ovule_number.RData")

#ADD PHYLOGENETIC DISTANCE TO THE MODEL
evo_distance_its <- readRDS("Data/RDS/evo_distance_its.RDS")
evo_distance_rbcl <- readRDS("Data/RDS/evo_distance_rbcl.RDS")
combined_distance<- evo_distance_its+evo_distance_rbcl
mean_distance<-combined_distance/2
str(mean_distance)
mean_distance<- as.matrix(mean_distance)
mean_distance <- data.frame(rows=rownames(mean_distance)[row(mean_distance)], vars=colnames(mean_distance)[col(mean_distance)],
                            values=c(mean_distance))
colnames(mean_distance) <-  c("Recipient", "Donor", "Distance")
mydata1 <- merge(mean_distance, mydata)
model1<-lm(effect_size~Distance,data=mydata1)
summary(model1)
anova(model1)
library(sjPlot)
plot_model(model1)

#
##
###
####
#####
######
#######
########
########
######## ANALYSES
########
########
#######
######
#####
####
###
##
#


#SCALE ALL NUMERICAL VALUES BEFORE MODELLING 
z <-mydata1
z[,c(5:31)] <- scale(mydata1[,c(5:31)])
#Model with interaction between a donor and a recipient trait + trait
model1<-lm(effect_size~Recipient_stigma_length*Donor_pollen_size+Recipient_pollen_ovule_ratio,data=z)
summary(model1)
vif(model1)
r.squaredGLMM(model1)
simulationOutput <- simulateResiduals(fittedModel = model1, plot = T)

cor.test(mydata$Recipient_stigma_length, mydata$Recipient_style_length)



############################################################
############################################################
####### EFFECT SIZES ~ DONOR ID + RECIPIENT ID #############
############################################################
############################################################

#Now check if donors and recipients have a significant effect on effect sizes
model1<-lm(effect_size~Donor+Recipient,data=mydata)
summary(model1)
anova(model1)
#just recipient sopecies have a significant effect on effect sizes
plot_model(model1)
plot_model(model1, show.values = TRUE, value.offset = .3)


#################################################################
#################################################################
####### EFFECT SIZES ~ POLLEN QUANTITY/POLLEN RATIO #############
#################################################################
#################################################################

#Merge data with pollen
#Read data
p_data <-readRDS("Data/RData/pollen_non.RData")
p_data$donor <- c("IPPU", "IPPU", "IPPU", "IPPU", "IPAQ", "IPAQ", "IPAQ", "IPAQ",
                  "SOME", "SOME", "PEIN", "SOLY", "CAAN", "CAAN", "ERSA", "ERSA",
                  "BROL", "SIAL", "SIAL", "BRRA") 

p_data$recipient <- c("CAAN", "ERSA", "BROL", "BRRA", "SOME", "PEIN", "SOLY", "SIAL",
                      "IPPU", "SIAL", "ERSA", "BRRA", "IPAQ", "BROL", "IPPU", "SOME", 
                      "PEIN", "IPAQ", "CAAN", "SOLY")

#Recipient_donor paste in order to merge with the other datset
p_data$donor_recipient <- paste(p_data$recipient, p_data$donor,sep="_")
mydata$donor_recipient <- paste(mydata$Recipient, mydata$Donor,sep="_")
donor_recipient <- merge(p_data,mydata, by="donor_recipient")
#CHECK DIST
hist(donor_recipient$value)
#normal distribution


#Creating conspecific pollen variable
#In this data frame I have just total and hp pollen
cp_pollen <- donor_recipient$total_pollen - donor_recipient$hp_pollen
donor_recipient$cp_pollen<- cp_pollen


# MODEL EFFECT SIZES ~ HP POLLEN RATIO
m1 <-lm(effect_size~hp_ratio,data=donor_recipient)
summary(m1)
anova(m1)
#PLOT REGRESSION
ggplotRegression(lm(effect_size ~ hp_ratio, data = donor_recipient))
testDispersion(m1)
simulationOutput <- simulateResiduals(fittedModel = m1, plot = T)


# MODEL EFFECT SIZES ~ TOTAL POLLEN
m2 <-lm(effect_size~total_pollen, data=donor_recipient)
summary(m2)
anova(m2)
#total pollen
##PREPARE MODELS ACCORDINGLY AND ADD GOODNESS OF FIT TO THE MS
testDispersion(m2)
simulationOutput <- simulateResiduals(fittedModel = m2, plot = T)

# MODEL EFFECT SIZES ~ TOTAL POLLEN/STIGMA AREA
donor_recipient$pollen_per_area <- donor_recipient$total_pollen/donor_recipient$Recipient_stigma_area
m3 <-lm(effect_size~pollen_per_area, data=donor_recipient)
summary(m3)
anova(m3)
#total pollen
##PREPARE MODELS ACCORDINGLY AND ADD GOODNESS OF FIT TO THE MS
testDispersion(m3)
simulationOutput <- simulateResiduals(fittedModel = m3, plot = T)
