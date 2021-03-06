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


#HERE I RUN THE MODEL WITHOUT SCALING VARIABLES
#IN ORDERT TO PLOT THE INTERACTION BETWEEN POLLEN SIZE AND STIGMATIC AREA
model1<-lm(effect_size~Recipient_stigma_width*Donor_pollen_size,data=mydata)
summary(model1)
#Prepare data for plotting
#create 4 different dataframes for each pollen size (ab, ab_1,ab_2,ab_3)
a <- subset(mydata, Donor_pollen_size==22)
Donor_pollen_size <- a$Donor_pollen_size

b <- subset(mydata, Donor_pollen_size==22)
Recipient_stigma_width <- b$Recipient_stigma_width

ab <- as.data.frame(cbind(Recipient_stigma_width,Donor_pollen_size))
effect_size <- predict(model1,ab,interval = "confidence")
ab <- cbind(ab,effect_size)
colnames(ab)[3] <- "effect_size"

a <- subset(mydata, Donor_pollen_size==97.59)
Donor_pollen_size <- a$Donor_pollen_size

b <- subset(mydata, Donor_pollen_size==97.59)
Recipient_stigma_width <- b$Recipient_stigma_width

ab_1 <- as.data.frame(cbind(Recipient_stigma_width,Donor_pollen_size))
effect_size <- predict(model1,ab_1,interval = "confidence")
ab_1 <- cbind(ab_1,effect_size)
colnames(ab_1)[3] <- "effect_size"

a <- subset(mydata, Donor_pollen_size==70.10)
Donor_pollen_size <- a$Donor_pollen_size

b <- subset(mydata, Donor_pollen_size==70.10)
Recipient_stigma_width <- b$Recipient_stigma_width

ab_2 <- as.data.frame(cbind(Recipient_stigma_width,Donor_pollen_size))
effect_size <- predict(model1,ab_2,interval = "confidence")
ab_2 <- cbind(ab_2,effect_size)
colnames(ab_2)[3] <- "effect_size"


a <- subset(mydata, Donor_pollen_size==33.59)
Donor_pollen_size <- a$Donor_pollen_size

b <- subset(mydata, Donor_pollen_size==33.59)
Recipient_stigma_width <- b$Recipient_stigma_width

ab_3 <- as.data.frame(cbind(Recipient_stigma_width,Donor_pollen_size))
effect_size <- predict(model1,ab_3,interval = "confidence")
ab_3 <- cbind(ab_3,effect_size)
colnames(ab_3)[3] <- "effect_size"

#Prepare dataframe for adding dots
mydata2 <- subset(mydata, Donor_pollen_size==22|Donor_pollen_size==97.59|Donor_pollen_size==70.10|Donor_pollen_size==33.59)

#Add two decimals to pollen size on the legend
#function from:
#https://stackoverflow.com/questions/38722202/how-do-i-change-the-number-of-decimal-places-on-axis-labels-in-ggplot2
scaleFUN <- function(x) sprintf("%.2f", x)

# Theme for publication
theme_ms <- function(base_size=12, base_family="Helvetica") {
  library(grid)
  (theme_bw(base_size = base_size, base_family = base_family)+
      theme(text=element_text(color="black"),
            axis.title=element_text( size = rel(1.3)),
            axis.text=element_text(size = rel(1), color = "black"),
            legend.title=element_text(face="bold"),
            legend.text=element_text(),
            legend.background=element_rect(fill="transparent"),
            legend.key.size = unit(0.8, 'lines'),
            panel.border=element_rect(color="black",size=1),
            panel.grid.minor.x =element_line(),
            panel.grid.minor.y= element_blank()
      ))
}

#Create ggplot for publication
int_plot <- ggplot(mydata2,aes(y=effect_size,x=Recipient_stigma_width,color=factor(scaleFUN(Donor_pollen_size)),label=sprintf("%0.2f", round(Donor_pollen_size, digits = 2))))+geom_jitter(width = 0.3, height = 0.3)+
  geom_line(data=ab) + geom_ribbon(data=ab,aes(x = Recipient_stigma_width,ymin = lwr, ymax = upr),alpha = 0.1,fill="#D43F3AFF", colour=NA)+
  geom_ribbon(data=ab_1,aes(x = Recipient_stigma_width,ymin = lwr, ymax = upr),alpha = 0.1,colour=NA, fill="#EEA236FF") + geom_line(data=ab_1) + 
  labs(color=c(expression(paste("Pollen size (", mu,"m",")"))),y= "Predicted effect size",  x= c(expression(paste("Stigmatic area (", mu,"m"^"2",")")),"Predicted effect size")) +
  geom_line(data=ab_2) +  geom_line(data=ab_3) + theme_ms() +  scale_color_locuszoom() + geom_ribbon(data=ab_2,aes(x = Recipient_stigma_width,ymin = lwr, ymax = upr),alpha = 0.1,colour=NA, fill="#5CB85CFF") +
  geom_ribbon(data=ab_3,aes(x = Recipient_stigma_width,ymin = lwr, ymax = upr),alpha = 0.1,colour=NA, fill="#46B8DAFF")
ggsave(filename = "Fig_5.pdf", int_plot, width = 10, height = 5, units = "in",dpi = 1000)
