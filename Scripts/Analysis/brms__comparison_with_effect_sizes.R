########################################################################################################################################################
#In this script I show that the brms output and the effect size do not differ much
#we have use effect sizes in our analysis because they allow a fast visual comparison across species
########################################################################################################################################################
#LOAD LIBRARIES
library(ape) #for phylogenetic distance
library(dplyr) #data processing
library(rtrees) #for phylogenetic distance
library(DHARMa)
library(brms)
library(cmdstanr)
library(ggplot2)
########################################################################################################################################################
#PEIN
#load data
pein_seed_set_final <- read.csv("Raw_data/pein_seed_set_final.csv", stringsAsFactors = T)
#filter data
pein_seed_set_final <- subset(pein_seed_set_final, Treatment!="FLOWER CONTROL" & Treatment!="CONTROL" & Treatment!="SELF")
#remove Na's
pein_seed_set_final=na.omit(pein_seed_set_final)
#fix names
pein_seed_set_final$Treatment <- as.character(pein_seed_set_final$Treatment)
pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="CROSS"] <- "Petunia integrifolia"
pein_seed_set_final$Treatment[pein_seed_set_final$Treatment=="Eruca vesicaria"] <- "Eruca sativa"

pein_seed_set_final$Treatment <- factor(pein_seed_set_final$Treatment, levels = c("Solanum melongena", "Solanum lycopersicum", "Petunia integrifolia", "Capsicum annuum", "Ipomoea purpurea", "Ipomoea aquatica",
                                                                                  "Sinapis alba", "Eruca sativa", "Brassica rapa", "Brassica oleracea"))

#5 clusters hclust
pein_brm <- brm(Seed.production ~ Treatment,
            data = pein_seed_set_final, family  = zero_inflated_negbinomial(), cores = 4,chains = 4, 
            sample_prior = TRUE, warmup = 500, iter = 1500,
            control = list(adapt_delta = 0.99)) 

#check model fit
pp_check(pein_brm)  +xlim(-50,200)+ylim(0,0.1)
#conditional effects
ce_1 <- conditional_effects(pein_brm,points=T) 
#prepare ggplot

#set order of levels


p1 <- ggplot(ce_1[[1]], aes(x = Treatment, y = Seed.production, colour = (Treatment))) +
  theme_bw()+ ylab("Seed production") + xlab("Species")+
  geom_point(data = pein_seed_set_final,aes(x = Treatment, y = (Seed.production)),size = 1.2, position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.5), alpha=0.3)+
  geom_errorbar(data=ce_1[[1]],mapping=aes(x=Treatment, ymin=lower__, ymax=upper__,colour = as.factor(Treatment), group = 
  as.factor(Treatment)), width=.6,alpha=0.8, size = 0.9,position = position_dodge(width = 0.8))+ geom_point(data = ce_1[[1]],aes(x = Treatment, y = (estimate__)),size = 1.2, position = position_dodge(width = 0.8), alpha=1)+
  coord_flip()+ geom_hline(yintercept = as.numeric(ce_1[[1]]$estimate__[ce_1[[1]]$Treatment=="Petunia integrifolia"]),linetype="dashed") + theme(legend.position = "none")+
  scale_color_manual("Floral visitors guilds",values=c(rep("#D55E00",2),rep("#E69F00",1),rep("#D55E00",1),rep("#009E73",2),rep("#287DAB",4) ))


########################################################################################################################################################
#prepare data
#In this script I'm going to calculate the effect sizes and plot them.

#loadlibrary
#install.packages("effsize")
library(effsize)
library(dplyr)
library(ggplot2)
library(ggpubr)

#LOAD DATA
load("Data/RData/seed_set&scaled_seed_set.RData")
colnames(ersa) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(brra) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(brol) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(ipaq) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(ippu) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(soly) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(some) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(caan) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(ersa) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")


#Reading correct file of SIAL and SOME is lacking one Treatment for both in the old file
sial <- read.csv("Raw_data/SIAL_seed_set_final.csv")
sial<- sial[,-c(1,6)]
colnames(sial) <- c("Species","Treatment", "Treatment_number", "Seed_set")
sial$Scale_seed <- scale(sial$Seed_set)

some <- read.csv("Raw_data/SOME_seed_set_final.csv")
some<- some[,-c(1,6)]
colnames(some) <- c("Species","Treatment", "Treatment_number", "Seed_set")
some$Scale_seed <- scale(some$Seed_set)

#FILTER COLUMNS AND ROWS OF INTEREST
#Preparing for loop to clean dataframe and select columns of interest
species_list <- list(soly, some, pein, caan, ersa, brra, sial, brol, ippu, ipaq)
i <- NULL
y <- NULL

for (i in species_list){
  colnames(i)<- c("Species", "Treatment", "Treatment_number", "Seed_set", "Scale_seed")
  i <- filter(i, Treatment!="RARA 50%", Treatment!="COSA 50%", Treatment!="Self", Treatment!="Control",Treatment!="Flower Control",
              Treatment!="FC", Treatment!="FLOWER CONTROL", Treatment!="control", Treatment!="flower control", Treatment!="self", Treatment!="SELF",
              Treatment!="CONTROL")
  i <-i[-grep("100", i$Treatment),] 
  
  y <- rbind(y, i)
}

########################################################################################################################################################
#calculate effect sizes for PEIN
#PEIN
pein_seeds <- subset(y, Species=="PEIN")
#We order alphabetically to be able to replicate exactly the same for all the species
pein_seeds <- pein_seeds[order(pein_seeds$Treatment, pein_seeds$Seed_set), ]
pein_cross <- subset(pein_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(pein_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL
for (i in species){
  a<-cohen.d(pein_seeds$Seed_set[pein_seeds$Treatment==i], pein_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}
#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "P. integrifolia", "E. sativa", "I. aquatica", "I. purpurea",
               "S. alba", "S. lycopersicum", "S. melongena")
#family names
Family <- c("B", "B", "S", "S", "B", "C", "C", "B", "S", "S")
pein_effect_size <- cbind( Species_1, Family, cohen_d,cbind(lower, upper))
pein_effect_size <- data.frame(pein_effect_size)
#colnames
colnames(pein_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")
str(pein_effect_size)
#reorganize structure of dataframe for plotting
pein_effect_size_s <- subset(pein_effect_size, Family == "S" )
pein_effect_size_b <- subset(pein_effect_size, Family == "B" )
pein_effect_size_c <- subset(pein_effect_size, Family == "C" )
#conert to characters
pein_effect_size_s$Family <- as.character(pein_effect_size_s$Family)
pein_effect_size_b$Family <- as.character(pein_effect_size_b$Family)
pein_effect_size_c$Family <- as.character(pein_effect_size_c$Family)
#descending alphatical order
pein_effect_size_s <- pein_effect_size_s %>% arrange(desc(Species_1))
pein_effect_size_b <- pein_effect_size_b %>% arrange(desc(Species_1))
pein_effect_size_c <- pein_effect_size_c %>% arrange(desc(Species_1))
#bind again and data almost ready
pein_effect_size<- rbind(pein_effect_size_s,pein_effect_size_c, pein_effect_size_b)
pein_effect_size$Species_1 <- factor(pein_effect_size$Species_1, levels = pein_effect_size$Species_1)
#convert to numeric
pein_effect_size$Cohen_d <- as.numeric(pein_effect_size$Cohen_d)
pein_effect_size$Lower  <- as.numeric(pein_effect_size$Lower )
pein_effect_size$Upper <- as.numeric(pein_effect_size$Upper)
#Plot
p2 <- ggplot(pein_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),plot.margin = unit(c(0,0,0,0), "cm"))+
  geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 
########################################################################################################################################################
#Now plot side by side eefct sizes and brms output (just one species) 
gridExtra::grid.arrange(p1, p2)



