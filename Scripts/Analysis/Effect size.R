#In this script I'm going to calculate the effect sizes and plot them.
#Moreover I will compare differences among species and among families

#loadlibrary
#install.packages("effsize")
library(effsize)
library(dplyr)
library(ggplot2)
library(ggpubr)

load("seed_set&scaled_seed_set.RData")

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

#Now I prepare effect sizes for SOLY

#SOLY

#First we subset the long data frame for our species of interest
soly_seeds <- subset(y, Species=="SOLY")
str(soly_seeds)

#We order alphabetically to be able to replicate exactly the same for all the species
soly_seeds <- soly_seeds[order(soly_seeds$Treatment, soly_seeds$Seed_set), ]

#Just checking a first example of how it would be
soly_cross <- subset(soly_seeds, Treatment=="CROSS")
soly_ippu <- subset(soly_seeds, Treatment=="IPPU 50%")
soly_ipaq <- subset(soly_seeds, Treatment=="IPAQ 50%")
a <- cohen.d(soly_ippu$Seed_set, soly_cross$Seed_set)
b <- cohen.d(soly_ipaq$Seed_set, soly_cross$Seed_set)

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
species<- sort(unique(soly_seeds$Treatment))

b <- NULL
x <- NULL
for (i in species){
  a<-cohen.d(soly_seeds$Seed_set[soly_seeds$Treatment==i], soly_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

#Now we convert the list to a data frame to plot it
lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

#Adding species names and families (just initials)
Species_1 <-c ("B. oleracea","B. rapa", "C. annuum", "S. lycopersicum.", "E. sativa", "I. aquatica", "I. purpurea",
            "P. integrifolia", "S. alba", "S. melongena")

Family <- c("B", "B", "S", "P", "B", "C", "C", "S", "B", "S")
soly_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(soly_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(soly_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(soly_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#PEIN

pein_seeds <- subset(y, Species=="PEIN")

pein_cross <- subset(pein_seeds, Treatment=="CROSS")
pein_ippu <- subset(pein_seeds, Treatment=="IPPU 50%")
pein_ipaq <- subset(pein_seeds, Treatment=="IPAQ 50%")

a <- cohen.d(pein_ippu$Seed_set, pein_cross$Seed_set)
b <- cohen.d(pein_ipaq$Seed_set, pein_cross$Seed_set)


species<- unique(pein_seeds$Treatment)
b <- NULL
x <- NULL
for (i in species){
  a<-cohen.d(pein_seeds$Seed_set[pein_seeds$Treatment==i], pein_cross$Seed_set)
  b <- rbind(b, a[3])
  x<- rbind(x, a[4])
}

lower<- lapply(x, `[[`, 1)
lower<- as.data.frame(unlist(lower))
upper<- lapply(x, `[[`, 2)
upper<- as.data.frame(unlist(upper))
cbind(lower, upper)

cohen_d<- lapply(b, `[[`, 1)
cohen_d<- as.data.frame(unlist(cohen_d))

Species_1 <-c ("S. lycopersicum.", "I. purpurea", "S. alba", "C. annuum", "S. melongena", "B. oleracea",
               "P. integrifolia", "B. rapa", "E. sativa", "I. aquatica")
Family <- c("poll.", "C", "B", "S", "S", "B", "S", "B", "B", "C")
pein_effect_size <- cbind(species, Species_1, Family, cohen_d,cbind(lower, upper))

colnames(pein_effect_size) <- c("Species","Species_1","Family", "Cohen_d", "Lower", "Upper")
str(pein_effect_size)

#Now I plot Cohen's d with lower and upper confidences intervals

p2<- ggplot(pein_effect_size, aes(Species_1,Cohen_d, size=10)) + theme_bw(base_size=10)
p2 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +geom_errorbar(show.legend=FALSE, aes(x = Species_1, ymin = Lower, ymax = Upper, size=2,color=factor(Family)),
                                                                              width = 0.2)+scale_color_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  scale_fill_manual("Family",values=c("#0072B2", "#009E73", "#E69F00", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")
