########################################################################################################################################################
#PAIRWISE EFFECT SIZES
########################################################################################################################################################
#Load libraries
#install.packages("effsize")
#library(effsize)
#library(dplyr)
#library(ggplot2)
#library(ggpubr)
#NOTE: some functions have changed and I had to re-edit the script to keep it working
library(groundhog)
groundhog.day="2020-12-10"
groundhog.library('effsize', groundhog.day)
groundhog.library('dplyr', groundhog.day)
groundhog.library('ggplot2', groundhog.day)
groundhog.library('ggpubr', groundhog.day)
########################################################################################################################################################
#LOAD DATA
load("Data/RData/seed_set&scaled_seed_set.RData")
colnames(ersa) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(brra) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(brol) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(ipaq) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(ippu) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
colnames(soly) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
#colnames(some) <- c("Species","Treatment", "Treatment_number", "Seed_set", "Scale_seed")
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
########################################################################################################################################################
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

#SOME AND SIAL ARE NOT FILTERING WELL

some <- filter(some, Treatment!="RARA 50%", Treatment!="COSA 50%", Treatment!="Self", Treatment!="Control",Treatment!="Flower Control",Treatment!="Flower control",
       Treatment!="FC", Treatment!="FLOWER CONTROL", Treatment!="control", Treatment!="flower control", Treatment!="self", Treatment!="SELF",
       Treatment!="CONTROL")

sial <- filter(sial, Treatment!="RARA 50%", Treatment!="COSA 50%", Treatment!="Self", Treatment!="Control",Treatment!="Flower Control",Treatment!="Flower control",
               Treatment!="FC", Treatment!="FLOWER CONTROL", Treatment!="control", Treatment!="flower control", Treatment!="self", Treatment!="SELF",
               Treatment!="CONTROL")


########################################################################################################################################################
#NOW WE CALCULATE EFFECT SIZES PER RECIPIENT SPECIES
########################################################################################################################################################
#SOLY/ SOLANUM LYCOPERSICUM
########################################################################################################################################################
#First we subset the long data frame for our species of interest
soly_seeds <- subset(y, Species=="SOLY")
str(soly_seeds)
#We order alphabetically to be able to replicate exactly the same for all the species
soly_seeds <- soly_seeds[order(soly_seeds$Treatment, soly_seeds$Seed_set), ]
#Subset cross for Cohen's d on the loop
soly_cross <- subset(soly_seeds, Treatment=="CROSS")

#Just checking a first example of how it would be
soly_ippu <- subset(soly_seeds, Treatment=="IPPU 50%")
soly_ipaq <- subset(soly_seeds, Treatment=="IPAQ 50%")
a <- cohen.d(soly_ippu$Seed_set, soly_cross$Seed_set)
b <- cohen.d(soly_ipaq$Seed_set, soly_cross$Seed_set)

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(soly_seeds$Treatment))

#Again we sort alphabetically
cohen_d <- NULL
lower <- NULL
upper <- NULL
for (i in species){
  a<-cohen.d(soly_seeds$Seed_set[soly_seeds$Treatment==i], soly_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
}

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "S. lycopersicum", "E. sativa", "I. aquatica", "I. purpurea",
            "P. integrifolia", "S. alba", "S. melongena")

Family <- c("B", "B", "S", "S", "B", "C", "C", "S", "B", "S")
soly_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))
soly_effect_size <- data.frame(soly_effect_size)
#set colnames
colnames(soly_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")

#Reorganize structure of the dataframe for plotting
soly_effect_size_s <- subset(soly_effect_size, Family == "S" )
soly_effect_size_b <- subset(soly_effect_size, Family == "B" )
soly_effect_size_c <- subset(soly_effect_size, Family == "C" )
#convert to character
soly_effect_size_s$Family <- as.character(soly_effect_size_s$Family)
soly_effect_size_b$Family <- as.character(soly_effect_size_b$Family)
soly_effect_size_c$Family <- as.character(soly_effect_size_c$Family)
#descending alphabetical order
soly_effect_size_s <- soly_effect_size_s %>% arrange(desc(Species_1))
soly_effect_size_b <- soly_effect_size_b %>% arrange(desc(Species_1))
soly_effect_size_c <- soly_effect_size_c %>% arrange(desc(Species_1))
#bind dataframe
soly_effect_size<- rbind(soly_effect_size_s,soly_effect_size_c, soly_effect_size_b)
soly_effect_size$Species_1 <- factor(soly_effect_size$Species_1, levels = soly_effect_size$Species_1)
##convert to numeric
soly_effect_size$Cohen_d <- as.numeric(soly_effect_size$Cohen_d)
soly_effect_size$Lower  <- as.numeric(soly_effect_size$Lower )
soly_effect_size$Upper <- as.numeric(soly_effect_size$Upper)

#the colours are arranged properly in the rmd file
#Now I plot Cohen's d with lower and upper confidences intervals
ggplot(soly_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),plot.margin = unit(c(0,0,0,0), "cm"))+
geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 

#save.image("Manuscript_draft/effect_size_species/soly_effect_size.RData")

########################################################################################################################################################
#PEIN/PETUNIA INTEGRIFOLIA
########################################################################################################################################################
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
p2 <- ggplot(pein_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 

#save.image("Manuscript_draft/effect_size_species/pein_effect_size.RData")
########################################################################################################################################################
#CAAN/ CAPSICUM ANNUUM
########################################################################################################################################################
caan_seeds <- subset(y, Species=="CAAN")

#We order alphabetically to be able to replicate exactly the same for all the species
caan_seeds <- caan_seeds[order(caan_seeds$Treatment, caan_seeds$Seed_set), ]
caan_cross <- subset(caan_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(caan_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL
for (i in species){
  a<-cohen.d(caan_seeds$Seed_set[caan_seeds$Treatment==i], caan_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}


#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum", "S. melongena")

Family <- c("B", "B", "S", "B", "C", "C", "S", "B","S", "S")
caan_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))
#convert to dataframe
caan_effect_size <- data.frame(caan_effect_size)
#colnames
colnames(caan_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")
#reorganize structure of the dataframe for plotting
caan_effect_size_s <- subset(caan_effect_size, Family == "S" )
caan_effect_size_b <- subset(caan_effect_size, Family == "B" )
caan_effect_size_c <- subset(caan_effect_size, Family == "C" )

caan_effect_size_s$Family <- as.character(caan_effect_size_s$Family)
caan_effect_size_b$Family <- as.character(caan_effect_size_b$Family)
caan_effect_size_c$Family <- as.character(caan_effect_size_c$Family)

caan_effect_size_s <- caan_effect_size_s %>% arrange(desc(Species_1))
caan_effect_size_b <- caan_effect_size_b %>% arrange(desc(Species_1))
caan_effect_size_c <- caan_effect_size_c %>% arrange(desc(Species_1))

caan_effect_size<- rbind(caan_effect_size_s,caan_effect_size_c, caan_effect_size_b)
caan_effect_size$Species_1 <- factor(caan_effect_size$Species_1, levels = caan_effect_size$Species_1)
#convert to numeric
caan_effect_size$Cohen_d <- as.numeric(caan_effect_size$Cohen_d)
caan_effect_size$Lower  <- as.numeric(caan_effect_size$Lower )
caan_effect_size$Upper <- as.numeric(caan_effect_size$Upper)

#Now I plot Cohen's d with lower and upper confidences intervals
#colours are set up properly in the rmd file
ggplot(caan_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 
#save data
#save.image("Manuscript_draft/effect_size_species/caan_effect_size.RData")
########################################################################################################################################################
#SOME/ SOLANUM MELONGENA
########################################################################################################################################################
#load it differently because we wre loading an old dataset 
some_seeds <- some
levels(factor(some_seeds$Treatment))
some_seeds <- some_seeds[!is.na(some_seeds$Seed_set),]
#We order alphabetically to be able to replicate exactly the same for all the species
some_seeds <- some_seeds[order(some_seeds$Treatment, some_seeds$Seed_set), ]
some_cross <- subset(some_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(some_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL
for (i in species){
  a<-cohen.d(some_seeds$Seed_set[some_seeds$Treatment==i], some_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "S. melongena", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum")

Family <- c("B", "B", "S", "S", "B", "C","C", "S", "B","S")
some_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))

#convert to dataframe
some_effect_size <- data.frame(some_effect_size)
#colnames
colnames(some_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")

some_effect_size_s <- subset(some_effect_size, Family == "S" )
some_effect_size_b <- subset(some_effect_size, Family == "B" )
some_effect_size_c <- subset(some_effect_size, Family == "C" )

some_effect_size_s$Family <- as.character(some_effect_size_s$Family)
some_effect_size_b$Family <- as.character(some_effect_size_b$Family)
some_effect_size_c$Family <- as.character(some_effect_size_c$Family)

some_effect_size_s <- some_effect_size_s %>% arrange(desc(Species_1))
some_effect_size_b <- some_effect_size_b %>% arrange(desc(Species_1))
some_effect_size_c <- some_effect_size_c %>% arrange(desc(Species_1))

some_effect_size<- rbind(some_effect_size_s,some_effect_size_c, some_effect_size_b)
some_effect_size$Species_1 <- factor(some_effect_size$Species_1, levels = some_effect_size$Species_1)
#convert to numeric
some_effect_size$Cohen_d <- as.numeric(some_effect_size$Cohen_d)
some_effect_size$Lower  <- as.numeric(some_effect_size$Lower )
some_effect_size$Upper <- as.numeric(some_effect_size$Upper)

#Now 

#Now I plot Cohen's d with lower and upper confidences intervals

#Now I plot Cohen's d with lower and upper confidences intervals
#colours are set up properly in the rmd file
ggplot(some_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 

#save.image("Manuscript_draft/effect_size_species/some_effect_size.RData")

########################################################################################################################################################
#BRASSICACEAE
########################################################################################################################################################
#BROL/BRASSICA OLERACEA
########################################################################################################################################################
brol_seeds <- subset(y, Species=="BROL")

#We order alphabetically to be able to replicate exactly the same for all the species
brol_seeds <- brol_seeds[order(brol_seeds$Treatment, brol_seeds$Seed_set), ]
brol_cross <- subset(brol_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(brol_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL
for (i in species){
  a<-cohen.d(brol_seeds$Seed_set[brol_seeds$Treatment==i], brol_seeds$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}

#Adding species names and families (just initials)
Species_1 <-c ("B. rapa","C. annuum","B. oleracea", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum","S. melongena")

Family <- c("B", "S", "B", "B", "C","C", "S", "B","S", "S")
brol_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))
#convert to dataframe
brol_effect_size <- data.frame(brol_effect_size)
#colnames\
colnames(brol_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")
str(brol_effect_size)


brol_effect_size_s <- subset(brol_effect_size, Family == "S" )
brol_effect_size_b <- subset(brol_effect_size, Family == "B" )
brol_effect_size_c <- subset(brol_effect_size, Family == "C" )

brol_effect_size_s$Family <- as.character(brol_effect_size_s$Family)
brol_effect_size_b$Family <- as.character(brol_effect_size_b$Family)
brol_effect_size_c$Family <- as.character(brol_effect_size_c$Family)

brol_effect_size_s <- brol_effect_size_s %>% arrange(desc(Species_1))
brol_effect_size_b <- brol_effect_size_b %>% arrange(desc(Species_1))
brol_effect_size_c <- brol_effect_size_c %>% arrange(desc(Species_1))

brol_effect_size<- rbind(brol_effect_size_s,brol_effect_size_c, brol_effect_size_b)
brol_effect_size$Species_1 <- factor(brol_effect_size$Species_1, levels = brol_effect_size$Species_1)
#convert to numeric
brol_effect_size$Cohen_d <- as.numeric(brol_effect_size$Cohen_d)
brol_effect_size$Lower  <- as.numeric(brol_effect_size$Lower )
brol_effect_size$Upper <- as.numeric(brol_effect_size$Upper)

#Now 

#Now I plot Cohen's d with lower and upper confidences intervals

#colours are set up properly in the rmd file
ggplot(brol_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' B. oleracea')))) 

#save.image("Manuscript_draft/effect_size_species/brol_effect_size.RData")

########################################################################################################################################################
#BRRA
########################################################################################################################################################
brra_seeds <- subset(y, Species=="BRRA")

#We order alphabetically to be able to replicate exactly the same for all the species
brra_seeds <- brra_seeds[order(brra_seeds$Treatment, brra_seeds$Seed_set), ]
brra_cross <- subset(brra_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(brra_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL
for (i in species){
  a<-cohen.d(brra_seeds$Seed_set[brra_seeds$Treatment==i], brra_seeds$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}


#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","C. annuum","B. rapa", "E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. alba", "S. lycopersicum","S. melongena")

Family <- c("B", "S", "B", "B", "C","C", "S", "B","S", "S")
brra_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))
#convert to dataframe
brra_effect_size <- data.frame(brra_effect_size)
colnames(brra_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")
str(brra_effect_size)

brra_effect_size_s <- subset(brra_effect_size, Family == "S" )
brra_effect_size_b <- subset(brra_effect_size, Family == "B" )
brra_effect_size_c <- subset(brra_effect_size, Family == "C" )

brra_effect_size_s$Family <- as.character(brra_effect_size_s$Family)
brra_effect_size_b$Family <- as.character(brra_effect_size_b$Family)
brra_effect_size_c$Family <- as.character(brra_effect_size_c$Family)

brra_effect_size_s <- brra_effect_size_s %>% arrange(desc(Species_1))
brra_effect_size_b <- brra_effect_size_b %>% arrange(desc(Species_1))
brra_effect_size_c <- brra_effect_size_c %>% arrange(desc(Species_1))

brra_effect_size<- rbind(brra_effect_size_s,brra_effect_size_c, brra_effect_size_b)
brra_effect_size$Species_1 <- factor(brra_effect_size$Species_1, levels = brra_effect_size$Species_1)


#convert to numeric
brra_effect_size$Cohen_d <- as.numeric(brra_effect_size$Cohen_d)
brra_effect_size$Lower  <- as.numeric(brra_effect_size$Lower )
brra_effect_size$Upper <- as.numeric(brra_effect_size$Upper)

#Now 

#Now I plot Cohen's d with lower and upper confidences intervals

#colours are set up properly in the rmd file
ggplot(brra_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' B. rapa')))) 

#save.image("Manuscript_draft/effect_size_species/brra_effect_size.RData")

########################################################################################################################################################
#SIAL/ SINAPIS ALBA
########################################################################################################################################################
sial_seeds <- sial
levels(factor(sial_seeds$Treatment))
sial_seeds <- sial_seeds[!is.na(sial_seeds$Seed_set),]
#We order alphabetically to be able to replicate exactly the same for all the species
sial_seeds <- sial_seeds[order(sial_seeds$Treatment, sial_seeds$Seed_set), ]
sial_cross <- subset(sial_seeds, Treatment=="Cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(sial_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL
for (i in species){
  a<-cohen.d(sial_seeds$Seed_set[sial_seeds$Treatment==i], sial_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum", "S. alba","E. sativa", "I. aquatica","I. purpurea", "P. integrifolia",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "B","B", "C","C", "S","S", "S")
sial_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))

#convert to dataframe
sial_effect_size <- data.frame(sial_effect_size)
#colnames
colnames(sial_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")

sial_effect_size_s <- subset(sial_effect_size, Family == "S" )
sial_effect_size_b <- subset(sial_effect_size, Family == "B" )
sial_effect_size_c <- subset(sial_effect_size, Family == "C" )

sial_effect_size_s$Family <- as.character(sial_effect_size_s$Family)
sial_effect_size_b$Family <- as.character(sial_effect_size_b$Family)
sial_effect_size_c$Family <- as.character(sial_effect_size_c$Family)

sial_effect_size_s <- sial_effect_size_s %>% arrange(desc(Species_1))
sial_effect_size_b <- sial_effect_size_b %>% arrange(desc(Species_1))
sial_effect_size_c <- sial_effect_size_c %>% arrange(desc(Species_1))

sial_effect_size<- rbind(sial_effect_size_s,sial_effect_size_c, sial_effect_size_b)
sial_effect_size$Species_1 <- factor(sial_effect_size$Species_1, levels = sial_effect_size$Species_1)
#convert to numeric
sial_effect_size$Cohen_d <- as.numeric(sial_effect_size$Cohen_d)
sial_effect_size$Lower  <- as.numeric(sial_effect_size$Lower )
sial_effect_size$Upper <- as.numeric(sial_effect_size$Upper)

#Now 

#Now I plot Cohen's d with lower and upper confidences intervals

#Now I plot Cohen's d with lower and upper confidences intervals
#colours are set up properly in the rmd file
ggplot(sial_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 

#save.image("Manuscript_draft/effect_size_species/sial_effect_size.RData")

########################################################################################################################################################
#ERSA/ERUCA SATIVA
########################################################################################################################################################
ersa_seeds <- subset(y, Species=="ERSA")
#We order alphabetically to be able to replicate exactly the same for all the species
ersa_seeds <- ersa_seeds[order(ersa_seeds$Treatment, ersa_seeds$Seed_set), ]
ersa_cross <- subset(ersa_seeds, Treatment=="Cross")
#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(ersa_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL

for (i in species){
  a<-cohen.d(ersa_seeds$Seed_set[ersa_seeds$Treatment==i], ersa_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}


#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum","E. sativa", "I. aquatica","I. purpurea", "P. integrifolia","S. alba",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "B","C","C", "S","B","S", "S")
ersa_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))
#convert to dataframe
ersa_effect_size <- data.frame(ersa_effect_size)
#colnames
colnames(ersa_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(ersa_effect_size)

ersa_effect_size_s <- subset(ersa_effect_size, Family == "S" )
ersa_effect_size_b <- subset(ersa_effect_size, Family == "B" )
ersa_effect_size_c <- subset(ersa_effect_size, Family == "C" )

ersa_effect_size_s$Family <- as.character(ersa_effect_size_s$Family)
ersa_effect_size_b$Family <- as.character(ersa_effect_size_b$Family)
ersa_effect_size_c$Family <- as.character(ersa_effect_size_c$Family)

ersa_effect_size_s <- ersa_effect_size_s %>% arrange(desc(Species_1))
ersa_effect_size_b <- ersa_effect_size_b %>% arrange(desc(Species_1))
ersa_effect_size_c <- ersa_effect_size_c %>% arrange(desc(Species_1))

ersa_effect_size<- rbind(ersa_effect_size_s,ersa_effect_size_c, ersa_effect_size_b)
ersa_effect_size$Species_1 <- factor(ersa_effect_size$Species_1, levels = ersa_effect_size$Species_1)

#convert to numeric
ersa_effect_size$Cohen_d <- as.numeric(ersa_effect_size$Cohen_d)
ersa_effect_size$Lower  <- as.numeric(ersa_effect_size$Lower )
ersa_effect_size$Upper <- as.numeric(ersa_effect_size$Upper)

#Now 

#Now I plot Cohen's d with lower and upper confidences intervals

#Now I plot Cohen's d with lower and upper confidences intervals
#colours are set up properly in the rmd file
ggplot(ersa_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 


#save.image("Manuscript_draft/effect_size_species/ersa_effect_size.RData")


########################################################################################################################################################
####
#CONVOLVULACEAE
####
########################################################################################################################################################
#IPPU/IPOMOEA PURPUREA
########################################################################################################################################################
ippu_seeds <- subset(y, Species=="IPPU")

#We order alphabetically to be able to replicate exactly the same for all the species
ippu_seeds <- ippu_seeds[order(ippu_seeds$Treatment, ippu_seeds$Seed_set), ]
ippu_cross <- subset(ippu_seeds, Treatment=="cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(ippu_seeds$Treatment))

cohen_d <- NULL
lower <- NULL
upper <- NULL

for (i in species){
  a<-cohen.d(ippu_seeds$Seed_set[ippu_seeds$Treatment==i], ippu_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum","I. purpurea","E. sativa", "I. aquatica", "P. integrifolia","S. alba",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "C","B","C", "S","B","S", "S")
ippu_effect_size <- cbind(Species_1, Family, cohen_d,cbind(lower, upper))
#convert to dataframe
ippu_effect_size <- data.frame(ippu_effect_size)
#colnames
colnames(ippu_effect_size) <- c("Species_1","Family", "Cohen_d", "Lower", "Upper")
str(ippu_effect_size)
#organize structure of dataframe to plot it in the same way for all species
ippu_effect_size_s <- subset(ippu_effect_size, Family == "S" )
ippu_effect_size_b <- subset(ippu_effect_size, Family == "B" )
ippu_effect_size_c <- subset(ippu_effect_size, Family == "C" )
#conert to characters
ippu_effect_size_s$Family <- as.character(ippu_effect_size_s$Family)
ippu_effect_size_b$Family <- as.character(ippu_effect_size_b$Family)
ippu_effect_size_c$Family <- as.character(ippu_effect_size_c$Family)
#descending order
ippu_effect_size_s <- ippu_effect_size_s %>% arrange(desc(Species_1))
ippu_effect_size_b <- ippu_effect_size_b %>% arrange(desc(Species_1))
ippu_effect_size_c <- ippu_effect_size_c %>% arrange(desc(Species_1))
#bin 3 dataframes
ippu_effect_size<- rbind(ippu_effect_size_s,ippu_effect_size_c, ippu_effect_size_b)
ippu_effect_size$Species_1 <- factor(ippu_effect_size$Species_1, levels = ippu_effect_size$Species_1)


#convert to numeric
ippu_effect_size$Cohen_d <- as.numeric(ippu_effect_size$Cohen_d)
ippu_effect_size$Lower  <- as.numeric(ippu_effect_size$Lower )
ippu_effect_size$Upper <- as.numeric(ippu_effect_size$Upper)

#Now I plot Cohen's d with lower and upper confidences intervals
#colours are set up properly in the rmd file
ggplot(ippu_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 

#save.image("Manuscript_draft/effect_size_species/ippu_effect_size.RData")
########################################################################################################################################################
#IPAQ/ IPOMOEA AQUATICA
########################################################################################################################################################
#Load data
ipaq_seeds <- subset(y, Species=="IPAQ")

#We order alphabetically to be able to replicate exactly the same for all the species
ipaq_seeds <- ipaq_seeds[order(ipaq_seeds$Treatment, ipaq_seeds$Seed_set), ]
ipaq_cross <- subset(ipaq_seeds, Treatment=="cross")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(ipaq_seeds$Treatment))
cohen_d <- NULL
lower <- NULL
upper <- NULL

for (i in species){
  a<-cohen.d(ipaq_seeds$Seed_set[ipaq_seeds$Treatment==i], ipaq_cross$Seed_set,hedges.correction = TRUE)
  cohen_d <- rbind(cohen_d,a$estimate)
  lower <- rbind(lower, a$conf.int[1])
  upper <- rbind(upper, a$conf.int[2])
  
}

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa","C. annuum","I. aquatica","E. sativa", "I. purpurea", "P. integrifolia","S. alba",
               "S. lycopersicum","S. melongena")

Family <- c("B", "B", "S", "C","B","C", "S","B","S", "S")
ipaq_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

#convert to dataframe
ipaq_effect_size <- data.frame(ipaq_effect_size)

colnames(ipaq_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(ipaq_effect_size)

ipaq_effect_size_s <- subset(ipaq_effect_size, Family == "S" )
ipaq_effect_size_b <- subset(ipaq_effect_size, Family == "B" )
ipaq_effect_size_c <- subset(ipaq_effect_size, Family == "C" )

ipaq_effect_size_s$Family <- as.character(ipaq_effect_size_s$Family)
ipaq_effect_size_b$Family <- as.character(ipaq_effect_size_b$Family)
ipaq_effect_size_c$Family <- as.character(ipaq_effect_size_c$Family)

ipaq_effect_size_s <- ipaq_effect_size_s %>% arrange(desc(Species_1))
ipaq_effect_size_b <- ipaq_effect_size_b %>% arrange(desc(Species_1))
ipaq_effect_size_c <- ipaq_effect_size_c %>% arrange(desc(Species_1))

ipaq_effect_size<- rbind(ipaq_effect_size_s,ipaq_effect_size_c, ipaq_effect_size_b)
ipaq_effect_size$Species_1 <- factor(ipaq_effect_size$Species_1, levels = ipaq_effect_size$Species_1)



#convert to numeric
ipaq_effect_size$Cohen_d <- as.numeric(ipaq_effect_size$Cohen_d)
ipaq_effect_size$Lower  <- as.numeric(ipaq_effect_size$Lower )
ipaq_effect_size$Upper <- as.numeric(ipaq_effect_size$Upper)

#Now I plot Cohen's d with lower and upper confidences intervals
#colours are set up properly in the rmd file
ggplot(ipaq_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)+theme(plot.title = element_text(size = 10),
  plot.margin = unit(c(0,0,0,0), "cm"))+ geom_point(alpha=c(1,1,1,1,1,1,1,1,1,1),size=1.5,show.legend = FALSE,aes(color=factor(Species_1))) +
  geom_errorbar(alpha=0.5,size=0.9,show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper,color=factor(Species_1)))+
  scale_color_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  scale_fill_manual("Species_1",values=c(rep("#D55E00",2),"#E69F00","#D55E00", rep("#009E73",2), rep("#0072B2",4)))+
  xlab("Hedges'g") + ylab("Species") + ggpubr::rotate(ylim = c(-6, 2))+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+ggtitle(substitute(paste("H)",italic(' P. integrifolia')))) 

#save.image("Manuscript_draft/effect_size_species/ipaq_effect_size.RData")


#prepare plot with data frame for legend in Markdown
#sp_legend=ipaq_effect_size
#str(sp_legend)
#sp_legend$Family[sp_legend$Family=="S"]<- "Solanaceae"
#sp_legend$Family[sp_legend$Family=="C"]<- "Convolvulaceae"
#sp_legend$Family[sp_legend$Family=="B"]<- "Brassicaceae"
#sp_legend[1,3] <- "Focal species"
#saveRDS(sp_legend, "Manuscript_draft/Data/sp_legend.RData")

########################################################################################################################################################