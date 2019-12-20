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
#ANALYSES
##
#

#First filter of variables
scope <- stepAIC(full, scope)
full<-lm(value~. -Recipient - Donor,data=mydata)
summary(full)
step<- stepAIC(full, trace=FALSE)
step$anova
#Almost all traits of donor drop as expected

#Trying a very simple model
model1<-lm(value~Recipient_stigma_length*Donor_pollen_size+Recipient_pollen_ovule_ratio,data=mydata)
summary(model1)
anova(model1)
plot_model(model1, type = "int",title="",axis.title=c(expression(paste("Stigmatic area (", mu,"m"^"2",")")),"Predicted effect size"), legend.title=expression(paste("Donor pollen size (", mu,"m)")),
           terms = c(Recipient_stigma_length,Donor_pollen_size), mdrt.values="minmax")+theme_sjplot()

cor.test(mydata$Recipient_pollen_ovule_ratio,mydata$Recipient_si_index)
model1<-lm(value~Recipient_stigma_width*Donor_pollen_size,data=mydata)
effect_plot(model1, pred = Recipient_pollen_ovule_ratio, interval = TRUE, plot.points = TRUE)

ggsave(filename = "rep_bio.pdf", rep_bio, width = 12, height = 5, units = "in",dpi = 1000)

rep_bio <- plot_model(model1, type = "int",title="",axis.title=c(expression(paste("Stigmatic area (", mu,"m"^"2",")")),"Predicted effect size"), legend.title=expression(paste("Donor pollen size (", mu,"m)")),
           terms = c(Recipient_stigma_length,Donor_pollen_size), mdrt.values="minmax")+theme_sjplot2()

ggsave(filename = "rep_bio.pdf", rep_bio, width = 10, height = 5, units = "in",dpi = 1000)


#Add phylogetic distance to the model

evo_distance_its <- readRDS("Data/RDS/evo_distance_its.RDS")
evo_distance_rbcl <- readRDS("Data/RDS/evo_distance_rbcl.RDS")

combined_distance<- evo_distance_its+evo_distance_rbcl
mean_distance<-combined_distance/2
str(mean_distance)
mean_distance<- as.matrix(mean_distance)
library(reshape2)
mean_distance <- data.frame(rows=rownames(mean_distance)[row(mean_distance)], vars=colnames(mean_distance)[col(mean_distance)],
           values=c(mean_distance))

colnames(mean_distance) <-  c("Recipient", "Donor", "Distance")

mydata1 <- merge(mean_distance, mydata)

model1<-lm(value~Recipient_stigma_length*Donor_pollen_size+Recipient_pollen_ovule_ratio+Distance,data=mydata1)
summary(model1)
anova(model1)
plot(model1)
effect_plot(model1, pred = Distance, interval = TRUE, plot.points = TRUE)

cor.test(mydata1$Recipient_pollen_ovule_ratio,mydata1$Distance)

model1<-lm(value~Distance,data=mydata1)
effect_plot(model1, pred = Distance, interval = TRUE, plot.points = TRUE)



model1<-lm(value~Recipient_style_length^2,data=mydata)
plot(model1)
model1<-lm(log(value+20)~Recipient_stigma_length,data=mydata)
effect_plot(model1, pred = Recipient_stigma_length, interval = TRUE, plot.points = TRUE)

effect_plot(model1, pred = Recipient_style_length, interval = TRUE, plot.points = TRUE)
plot_model(model1, type = "pred", terms = c("Recipient_style_length"))
model1<-lm(value~Recipient_stigma_width,data=mydata)
effect_plot(model1, pred = Recipient_stigma_width, interval = TRUE, plot.points = TRUE)

model1<-lm(value~Recipient_Selfing_rate,data=mydata)















ggplot(mydata, aes(x = Recipient_style_length, y = value)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplot(mydata, aes(x = Recipient_stigma_width, y = log(value+20))) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

fit<-lm(value~Recipient,data=mydata)

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




anova(model1)
#plot(model1)
plot_model(model1, show.values = TRUE, value.offset = .3)
#Residuals vs fitted
#We do not have homogeinity of the variance
#Approximately normally distributed, see qqplot. 
#residual vs fitted, not homogeneous variance. Also shown by stand. residuals.

#Seems I have a better fit when I onsider an interaction in glm

#Before I compare the effect of donor and recipient on the effect sizes
#Recipient as fixed effect 
model1<-lm(value~factor(Recipient),data=mydata)
summary(model1)
anova(model1)
#check fit 
plot(model1)
#Donor as fixed effect
model2<-lm(value~factor(Donor),data=mydata)
plot(model1)
anova(model2) 


model2<-lm(value~Donor+Recipient,data=mydata)
summary(model2)
anova(model2)
plot_model(model2)
plot_model(model2, show.values = TRUE, value.offset = .3)
effect_plot(model2, pred = Recipient_style_length, interval = TRUE, plot.points = TRUE)

#Donors does not produce a significant change in effect sizes 
#Recipients do

scope <- stepAIC(full, scope)

full<-lm(value~. -Recipient - Donor,data=mydata)
summary(full)
step<- stepAIC(full, trace=FALSE)
step$anova

m1 <- lm(value ~ Recipient_Selfing_rate + Recipient_pollen_size + Recipient_mean_pollen_anther + 
     Recipient_mean_ovules + Recipient_pollen_ovule_ratio + Recipient_stigma_area + 
     Recipient_stigma_length + Recipient_stigma_width + Recipient_style_length + 
     Donor_pollen_size, data=mydata)
summary(m1)
anova(m1)
plot(m1)

m1 <- lm(value ~ Recipient_Selfing_rate + Recipient_pollen_size + 
           Recipient_mean_ovules + Recipient_pollen_ovule_ratio + Recipient_stigma_area + 
           Recipient_stigma_length + Recipient_stigma_width + Recipient_style_length + 
           Donor_pollen_size, data=mydata)
summary(m1)
anova(m1)

m2<- lm(value~Donor_pollen_size+Recipient_style_length+Recipient_pollen_ovule_ratio, data=mydata)

m2<- lm(value~Recipient_stigma_width+Recipient_mean_ovules+Recipient_Selfing_rate, data=mydata)
summary(m2)
anova(m2)
cor.test(mydata$Recipient_Selfing_rate,mydata$Recipient_mean_ovules)
cor.test(mydata$Recipient_stigma_area,mydata$Recipient_style_length)
cor.test(mydata$Recipient_pollen_ovule_ratio,mydata$Recipient_stigma_area)
cor.test(mydata$Recipient_mean_ovules,mydata$Recipient_Selfing_rate)


model1<-lm(value~Recipient_stigma_area*Donor_pollen_size+Recipient_Selfing_rate+Recipient_mean_ovules,data=mydata)



summary(model1)
anova(model1)

model1<-lmer(value~Recipient_stigma_area*Donor_pollen_size+(1|Recipient),data=mydata,REML=FALSE)


model1<-lmer(value~Recipient_stigma_area*Donor_pollen_size+Recipient_Selfing_rate+Recipient_mean_ovules+(1|Recipient),data=mydata)
summary(model1)
anova(model1)
#Perform a Gaussian glmm-> lmer
#Random effect recipient, due to a just consider few traits
#MODEL 1
#DONOR POLLEN SIZE+RECIPIENT STYLE LENGTH+DONOR POLLEN SIZE * RECIPIENT STYLE LENGTH
model1<-lmer(value~Recipient_stigma_area*Donor_pollen_size+(1|Recipient),data=mydata,REML=FALSE)

model1<-lm(value~Recipient_stigma_area*Donor_pollen_size,data=mydata)
summary(model1)
#
model1<-lmer(value~Recipient_stigma_area*Donor_pollen_size+(1|Recipient),data=mydata)
summary(model1)
summ(model1, confint = TRUE, digits = 3)
effect_plot(model1, pred = Donor_pollen_size, interval = TRUE, plot.points = TRUE)
effect_plot(model1, pred = Recipient_stigma_area, interval = TRUE, plot.points = TRUE)
plot(allEffects(model1), ask=FALSE, ticks=list(at=c(.5,.50,.95)))
par(mfrow = c(1,1))
plot(model1)
#Analysing residuals
plot(model1)
qqnorm(residuals(model1))
jarqueberaTest(model1$df.resid)
predict(model1, newdata=mydata, type="response")
max(mydata$Donor_pollen_size)

plot_model(model1, type = "int",title="",axis.title=c("Stigmatic area","Predicted effect size"), legend.title="Donor pollen size",
 terms = c(Recipient_stigma_area,Donor_pollen_size), mdrt.values="minmax")+theme_sjplot()

#MODEL 2
#DONOR POLLEN SIZE+RECIPIENT STYLE LENGTH+DONOR POLLEN SIZE * RECIPIENT STYLE LENGTH+
# + POLLEN_OVULE RATIO
model2<-lmer(value~Donor_pollen_size+Recipient_stigma_area+Recipient_stigma_area*Donor_pollen_size+ Recipient_pollen_ovule_ratio+ (1|Recipient),data=mydata,REML=FALSE)
summary(model2)
summ(model2, confint = TRUE, digits = 3)
effect_plot(model1, pred = c("Recipient_stigma_area"* "Donor_pollen_size"),interval = TRUE, plot.points = TRUE)
#Analysing residuals, 
effect_plot(model2, pred = Recipient_style_length, interval = TRUE, plot.points = TRUE)
effect_plot(model2, pred = Recipient_pollen_ovule_ratio, interval = TRUE, plot.points = TRUE)
plot(model2)
qqnorm(residuals(model2))


#MODEL 3
#DONOR POLLEN SIZE+RECIPIENT STYLE LENGTH+DONOR POLLEN SIZE * RECIPIENT STYLE LENGTH+
# + RECIPIENT SELF INCOMPATIBILITY INDEX
model3<-lmer(value~Donor_pollen_size+Recipient_style_length+Recipient_style_length*Donor_pollen_size+ Recipient_si_index+ (1|Recipient),data=mydata,REML=FALSE)
summary(model3)
summ(model3, confint = TRUE, digits = 3)
effect_plot(model3, pred = Donor_pollen_size, interval = TRUE, plot.points = TRUE)
effect_plot(model3, pred = Recipient_style_length, interval = TRUE, plot.points = TRUE)
effect_plot(model3, pred = Recipient_si_index, interval = TRUE, plot.points = TRUE)
plot(model3)
qqnorm(residuals(model3))
#MODEL 4
#DONOR SELF INCOMPATIBILITY INDEX+RECIPIENT SELF INCOMPATIBILITY INDEX
model4<-lmer(value~Donor_si_index*Recipient_si_index+ (1|Recipient),data=mydata,REML=FALSE)
summary(model4)
summ(model4,  digits = 3)
effect_plot(model4, pred = Donor_si_index, interval = TRUE, plot.points = TRUE)
effect_plot(model4, pred = Recipient_si_index, interval = TRUE, plot.points = TRUE)
plot(model4)
qqnorm(residuals(model4))
plot(allEffects(model4), ask=FALSE)
allEffects(model4)



model4<-lmer(value~Recipient_style_length*Donor_si_index+ (1|Recipient),data=mydata,REML=FALSE)
summary(model4)
summ(model4,  digits = 3)
effect_plot(model4, pred = Donor_si_index, interval = TRUE, plot.points = TRUE)
effect_plot(model4, pred = Recipient_si_index, interval = TRUE, plot.points = TRUE)
plot(model4)
qqnorm(residuals(model4))

eff.p1 <- effect("Recipient_style_length*Donor_si_index", model4, KR=T)
plot(eff.p1)

install.packages(sjPlot)
library(sjPlot)
plot_model(model4, type = "int", terms = c(Recipient_style_length,Donor_si_index), legend.title = "Condition",mdrt.values="quart"
,ci.lvl = 0.95)+theme_sjplot()
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

model1<-lmer(value~hp_ratio*Recipient_stigma_area+ (1|Recipient),data=donor_recipient)
summary(model1)
plot(model1)
model2<-lm(value~hp_ratio,data=donor_recipient)
summary(model2)
plot(model2)
anova(model2)

plot(model1)
model2<-lmer(value~Donor_pollen_size+Recipient_stigma_area+Recipient_stigma_area*Donor_pollen_size+ Recipient_pollen_ovule_ratio+ (1|Recipient),data=mydata,REML=FALSE)

model2<-lmer(value~Donor_pollen_size+Recipient_stigma_area+Recipient_stigma_area*Donor_pollen_size+ Recipient_pollen_ovule_ratio+ (1|Recipient),data=mydata,REML=FALSE)

