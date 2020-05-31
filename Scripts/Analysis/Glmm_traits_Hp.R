#In this script I'm going to calculate the absolute effect of traits
#First I have to prepare the data in the desire format for the analysis

#load libraries
library(reshape2)
library(lme4)
library(ggplot2)
library(fBasics)
library(lmerTest)
library(jtools)
library(ggplot2)
library(effects)
library(MASS)
library(snakecase)
library(sjPlot)

#Funtion to plot lm
ggplotRegression <- function (fit, jit=FALSE) {
  
  require(ggplot2)
  
  if(jit){
    fit$model[,1] <- jitter(fit$model[,1])
    fit$model[,2] <- jitter(fit$model[,2])
  }
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(anova(fit))))
}
#end of function



#
##
###DATA PREPARATION FOR ANALYSES
##
#

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
mydata$id_number <- seq(length(mydata$Recipient))

#Now the data is ready for analysis


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


####
####
####  PART 1 EFFECT SIZES ~ TRAITS
####
####

#First filter of variables
scope <- stepAIC(full, scope)
full<-lm(value~. -Recipient - Donor,data=mydata)
summary(full)
step<- stepAIC(full, trace=FALSE)
step$anova
#Almost all traits of donor drop as expected


#Model with interaction between a donor and a recipient trait + trait
model1<-lm(value~Recipient_stigma_length*Donor_pollen_size+Recipient_pollen_ovule_ratio,data=mydata)
summary(model1)
anova(model1)
plot_model(model1, type = "int",title="",axis.title=c(expression(paste("Stigmatic area (", mu,"m"^"2",")")),"Predicted effect size"), legend.title=expression(paste("Donor pollen size (", mu,"m)")),
           terms = c(Recipient_stigma_length,Donor_pollen_size), mdrt.values="minmax")+theme_sjplot()


#Model with just the iteraction between stigma size and pollen size
model1<-lm(value~Recipient_stigma_width*Donor_pollen_size,data=mydata)
effect_plot(model1, pred = Recipient_pollen_ovule_ratio, interval = TRUE, plot.points = TRUE)
#ggsave(filename = "rep_bio.pdf", rep_bio, width = 12, height = 5, units = "in",dpi = 1000)
rep_bio <- plot_model(model1, type = "int",title="",axis.title=c(expression(paste("Stigmatic area (", mu,"m"^"2",")")),"Predicted effect size"), legend.title=expression(paste("Donor pollen size (", mu,"m)")),
           terms = c(Recipient_stigma_length,Donor_pollen_size), mdrt.values="minmax")+theme_sjplot2()
#ggsave(filename = "rep_bio.pdf", rep_bio, width = 10, height = 5, units = "in",dpi = 1000)




#Add phylogetic distance to the model
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


#Same model but with phylogenetic distance included
model1<-lm(value~Recipient_stigma_length*Donor_pollen_size+Recipient_pollen_ovule_ratio+Distance,data=mydata1)
summary(model1)
anova(model1)
#plot(model1)
effect_plot(model1, pred = Distance, interval = TRUE, plot.points = TRUE)
model1<-lm(value~Distance,data=mydata1)
summary(model1)


####
####
####  PART 2 EFFECT SIZES ~ DONOR ID + RECIPIENT ID 
####
####

#Now check if donors and recipients have a significant effect on effect sizes
model1<-lm(value~Donor+Recipient,data=mydata)
summary(model1)
anova(model1)
#just recipient sopecies have a significant effect on effect sizes
plot_model(model1)
plot_model(model1, show.values = TRUE, value.offset = .3)



####
####
####  PART 3 EFFECT SIZES ~ POLLEN RATIOS/QUANTITY
####
####

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

hist(donor_recipient$value)
#normal distribution

####
####Supporting analysis to the methodology
####

#Creating conspecific pollen variable
#In this data frame I have just total and hp pollen
cp_pollen <- donor_recipient$total_pollen - donor_recipient$hp_pollen
donor_recipient$cp_pollen<- cp_pollen


# MODEL EFFECT SIZES ~ HP POLLEN RATIO
pollen_ratio <-lm(value~scale(hp_ratio),data=donor_recipient)
summary(pollen_ratio)
ggplotRegression(lm(value ~ hp_ratio, data = donor_recipient))


# MODEL EFFECT SIZES ~ HP POLLEN QUANTITY

total_hp_pollen <-lm(value~scale(hp_pollen),data=donor_recipient)
summary(total_hp_pollen)
ggplotRegression(lm(value ~ hp_pollen, data = donor_recipient))

# 3rd model, total cp pollen

total_cp_pollen <-lm(value~total_pollen, data=donor_recipient)
summary(total_cp_pollen)
ggplotRegression(lm(value ~ total_pollen, data = donor_recipient))
ggplotRegression(lm(value ~ Recipient_stigma_area, data = donor_recipient))


#Correlation test between cp pollen, with hp pollen and stigma type
cor.test(donor_recipient$hp_pollen, donor_recipient$total_pollen)
cor.test(donor_recipient$hp_pollen, donor_recipient$Recipient_stigma_width)
cor.test(donor_recipient$total_pollen, donor_recipient$Recipient_stigma_width)
cor.test(donor_recipient$total_pollen, donor_recipient$Recipient_stigma_area)
cor.test(donor_recipient$total_pollen, donor_recipient$hp_pollen)




#FIX VALUES

a <- as.data.frame(donor_recipient$hp_pollen)
a$type<- "d"
colnames(a)[1]<-"p"
b <- as.data.frame(donor_recipient$cp_pollen)
b$type<- "t"
colnames(b)[1]<-"p"

ab <-rbind(a,b)
library(dplyr)
mu <- ab %>% 
  group_by(type) %>%
  summarise(grp.mean = mean(p))
mu


ggplot(ab, aes(x = p)) + geom_density(aes(fill = type), alpha=0.5) + theme_light()+
  scale_color_manual(values = c("#EFC000FF", "lightblue"))+geom_vline(aes(xintercept = grp.mean, color = type),
data = mu, linetype = "dashed")+scale_fill_manual(values = c("#EFC000FF", "lightblue"))+ xlab("Pollen")

  geom_vline(aes(xintercept = mean(hp_pollen)), 
             linetype = "dashed", size = 0.6,
             color = "#EFC000FF")+geom_density(data = donor_recipient, aes(x = total_pollen, y = ..count..), fill = "lightblue",color="lightblue3", alpha = 0.4)+
  geom_vline(aes(xintercept = mean(total_pollen)), 
             linetype = "dashed", size = 0.6,
             color = "lightblue", alpha=0.8)+ 
scale_fill_manual(values = c('#e1b582', '#a2b285')) + 
  scale_color_manual(values = c('#e1b582', '#a2b285'))

  