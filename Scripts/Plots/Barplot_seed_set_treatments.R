
#In this script I try to create a plot that summarizes the different treatments 
#that explains the reproductive biology of the different species


#load library
library(ggplot2)
library(datasets)
library(reshape2)

#load data
load("Manuscript_draft/Data/table_treatments.RData")

Species <- c("Brassica oleracea", "Brassica rapa", "Eruca versicaria", "Sinapis alba", "Ipomoea aquatica", "Ipomoea purpurea", "Capsicum annuum", "Petunia integrifolia", "Solanum lycopersicum", "Solanum melongena")
Hand_cross_pollination_mean <- cross_mean
Hand_self_pollination_mean <- self_mean
Natural_selfing_mean <- nat_self_mean
Apomixis_mean <- apo_mean
table<- data.frame(Species, Hand_cross_pollination_mean, Hand_self_pollination_mean, Natural_selfing_mean,Apomixis_mean)
Hand_cross_pollination_sd <- cross_sd
Hand_self_pollination_sd <- self_sd
Natural_selfing_sd <- nat_self_sd
Apomixis_sd <- apo_sd
table_1<- data.frame(Species, cross_sd, self_sd, nat_self_sd, apo_sd)


#I try a second way with ggplot
table_melt=melt(table) 
str(table_melt)
table_melt$value[table_melt$value==0] <- 0.5
table_melt$variable=as.character(table_melt$variable)
table_melt$variable[table_melt$variable=="Hand_cross_pollination"] <- "Hand cross pollination"
table_melt$variable[table_melt$variable=="Hand_self_pollination"] <- "Hand self pollination"
table_melt$variable[table_melt$variable=="Natural_selfing"] <- "Natural selfing"
table_melt$variable[table_melt$variable=="Apomixis"] <- "Apomixis"

table_melt_1=melt(table_1)
table_melt_1$value[table_melt_1$value==0] <- 0.5

table_melt$variable=as.character(table_melt$variable)
table_melt$variable[table_melt$variable=="Hand_cross_pollination"] <- "Hand cross pollination"
table_melt$variable[table_melt$variable=="Hand_self_pollination"] <- "Hand self pollination"
table_melt$variable[table_melt$variable=="Natural_selfing"] <- "Natural selfing"
table_melt$variable[table_melt$variable=="Apomixis"] <- "Apomixis"
#install.packages("extrafont")
library(extrafont)
library(forcats)

#font_import()
#fonts()

cross  <- subset(table_melt,variable=="Hand cross-pollination"  )
self  <- subset(table_melt, table_melt$variable=="Hand self-pollination" )
natural_s  <- subset(table_melt, table_melt$variable=="Natural selfing")
apomixis  <- subset(table_melt, table_melt$variable=="Apomixis")
bio$Species <- factor(bio$Species, levels = rev(unique(bio$Species)), ordered=TRUE)
bio$variable <- factor(bio$variable, levels = c("Hand cross pollination","Hand self pollination","Natural selfing","Apomixis"))

#save.image("Manuscript_draft/r_biology.RData")
load("Manuscript_draft/Data/r_biology.RData")

ggplot(bio,aes(x=Species,y=value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+scale_fill_grey(start = 0, end = 0.85,name="Treatments") + 
  theme_minimal()+xlab("Species")+ylab("Seed-setting rate (%)")+labs(title="Reproductive biology tests")+
  theme(axis.text.x = element_text(angle = 35, hjust = 1, face= "italic",family = "Arial Narrow"))+
  scale_y_continuous(minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10))

cbp1 <- c("#999999","#0072B2", "#E69F00", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

str(bio)
bio<-cbind(bio,table_melt_1)
bio=bio[,-c(4,5)]
colnames(bio)[3]<- "seed_set_mean"
colnames(bio)[4]<- "seed_set_sd"

bio$Species=as.character(bio$Species)
bio$Species[bio$Species=="Eruca versicaria"]<- "Eruca sativa"
bio$Species <- factor(bio$Species, levels = rev(unique(bio$Species)), ordered=TRUE)
ggplot(bio,aes(x=Species,y=seed_set_mean,fill=variable))+
  geom_bar(stat="identity",position="dodge")+scale_fill_grey(start = 0, end = 0.85,name="Treatments") + 
  theme_grey()+xlab("Species")+ylab("Seed-setting rate (%)")+labs(title="Spp Reproductive Biology")+
  theme(axis.text.y = element_text(angle = 0, hjust = 1, face= "italic",family = "Arial Narrow"))+
  scale_y_continuous(expand = c(0, 0),minor_breaks = seq(0 , 100, 5), breaks = seq(0, 100, 10))+
  scale_fill_manual(values = cbp1,name = "Treatments")+ 
  geom_errorbar(aes(ymin=seed_set_mean-seed_set_sd, ymax=seed_set_mean+seed_set_sd), width=.2, position=position_dodge(.9)) 
