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

####
#SOLANACEAE
####
#SOLY

#First we subset the long data frame for our species of interest
soly_seeds <- subset(y, Species=="SOLY")
#Solanaceae
soly_seeds$Family[soly_seeds$Treatment=="CROSS"] <- "S. lycopersicum"
soly_seeds$Family[soly_seeds$Treatment=="SOME 50%"] <- "Solanaceae"
soly_seeds$Family[soly_seeds$Treatment=="CAAN 50%"] <- "Solanaceae"
soly_seeds$Family[soly_seeds$Treatment=="PEIN 50%"] <- "Solanaceae"

#Brassicaceae
soly_seeds$Family[soly_seeds$Treatment=="BRRA 50%"] <- "Brassicaceae"
soly_seeds$Family[soly_seeds$Treatment=="SIAL 50%"] <- "Brassicaceae"
soly_seeds$Family[soly_seeds$Treatment=="ERSA 50%"] <- "Brassicaceae"
soly_seeds$Family[soly_seeds$Treatment=="BROL 50%"] <- "Brassicaceae"

#Convolvulaceae
soly_seeds$Family[soly_seeds$Treatment=="IPAQ 50%"] <- "Convolvulaceae"
soly_seeds$Family[soly_seeds$Treatment=="IPPU 50%"] <- "Convolvulaceae"

soly_cross <- subset(soly_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(soly_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(soly_seeds$Seed_set[soly_seeds$Family==i], soly_cross$Seed_set)
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

soly_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(soly_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")

soly_effect_size_cross <- subset(soly_effect_size, Family=="S. lycopersicum")
soly_effect_size_b <-subset(soly_effect_size, Family=="Brassicaceae")
soly_effect_size_c <- subset(soly_effect_size, Family=="Convolvulaceae")
soly_effect_size_s <-subset(soly_effect_size, Family=="Solanaceae")

soly_effect_size <- rbind(soly_effect_size_cross,soly_effect_size_b,
                          soly_effect_size_c,soly_effect_size_s)

soly_effect_size$Family <- factor(soly_effect_size$Family, levels = soly_effect_size$Family)


p1<- ggplot(soly_effect_size, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
ymax = Upper, size=2,color=factor(Family)), width = 0.2)+
  scale_color_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#SOME

#First we subset the long data frame for our species of interest
some_seeds <- subset(y, Species=="SOME")
#Solanaceae
some_seeds$Family[some_seeds$Treatment=="CROSS"] <- "S. melongena"
some_seeds$Family[some_seeds$Treatment=="SOLY 50%"] <- "Solanaceae"
some_seeds$Family[some_seeds$Treatment=="CAAN 50%"] <- "Solanaceae"
some_seeds$Family[some_seeds$Treatment=="PEIN 50%"] <- "Solanaceae"

#Brassicaceae
some_seeds$Family[some_seeds$Treatment=="BRRA 50%"] <- "Brassicaceae"
some_seeds$Family[some_seeds$Treatment=="SIAL 50%"] <- "Brassicaceae"
some_seeds$Family[some_seeds$Treatment=="ERSA 50%"] <- "Brassicaceae"
some_seeds$Family[some_seeds$Treatment=="BROL 50%"] <- "Brassicaceae"

#Convolvulaceae
some_seeds$Family[some_seeds$Treatment=="IPAQ 50%"] <- "Convolvulaceae"
some_seeds$Family[some_seeds$Treatment=="IPPU 50%"] <- "Convolvulaceae"

some_cross <- subset(some_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(some_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(some_seeds$Seed_set[some_seeds$Family==i], some_cross$Seed_set)
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

some_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(some_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")

some_effect_size_cross <- subset(some_effect_size, Family=="S. melongena")
some_effect_size_b <-subset(some_effect_size, Family=="Brassicaceae")
some_effect_size_c <- subset(some_effect_size, Family=="Convolvulaceae")
some_effect_size_s <-subset(some_effect_size, Family=="Solanaceae")

some_effect_size <- rbind(some_effect_size_cross,some_effect_size_b,
                          some_effect_size_c,some_effect_size_s)

some_effect_size$Family <- factor(some_effect_size$Family, levels = some_effect_size$Family)


p1<- ggplot(some_effect_size, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(Family)), width = 0.2)+
  scale_color_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")



#PEIN

#First we subset the long data frame for our species of interest
pein_seeds <- subset(y, Species=="PEIN")
#Solanaceae
pein_seeds$Family[pein_seeds$Treatment=="CROSS"] <- "P. integrifolia"
pein_seeds$Family[pein_seeds$Treatment=="SOLY 50%"] <- "Solanaceae"
pein_seeds$Family[pein_seeds$Treatment=="CAAN 50%"] <- "Solanaceae"
pein_seeds$Family[pein_seeds$Treatment=="SOME 50%"] <- "Solanaceae"

#Brassicaceae
pein_seeds$Family[pein_seeds$Treatment=="BRRA 50%"] <- "Brassicaceae"
pein_seeds$Family[pein_seeds$Treatment=="SIAL 50%"] <- "Brassicaceae"
pein_seeds$Family[pein_seeds$Treatment=="ERSA 50%"] <- "Brassicaceae"
pein_seeds$Family[pein_seeds$Treatment=="BROL 50%"] <- "Brassicaceae"

#Convolvulaceae
pein_seeds$Family[pein_seeds$Treatment=="IPAQ 50%"] <- "Convolvulaceae"
pein_seeds$Family[pein_seeds$Treatment=="IPPU 50 %"] <- "Convolvulaceae"

pein_cross <- subset(pein_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(pein_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(pein_seeds$Seed_set[pein_seeds$Family==i], pein_cross$Seed_set)
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

pein_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(pein_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")

pein_effect_size_cross <- subset(pein_effect_size, Family=="P. integrifolia")
pein_effect_size_b <-subset(pein_effect_size, Family=="Brassicaceae")
pein_effect_size_c <- subset(pein_effect_size, Family=="Convolvulaceae")
pein_effect_size_s <-subset(pein_effect_size, Family=="Solanaceae")

pein_effect_size <- rbind(pein_effect_size_cross,pein_effect_size_b,
                          pein_effect_size_c,pein_effect_size_s)

pein_effect_size$Family <- factor(pein_effect_size$Family, levels = pein_effect_size$Family)


p1<- ggplot(pein_effect_size, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(Family)), width = 0.2)+
  scale_color_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")


#CAAN

#First we subset the long data frame for our species of interest
caan_seeds <- subset(y, Species=="CAAN")
#Solanaceae
caan_seeds$Family[caan_seeds$Treatment=="CROSS"] <- "C. annuum"
caan_seeds$Family[caan_seeds$Treatment=="SOLY 50%"] <- "Solanaceae"
caan_seeds$Family[caan_seeds$Treatment=="PEIN 50%"] <- "Solanaceae"
caan_seeds$Family[caan_seeds$Treatment=="SOME 50%"] <- "Solanaceae"

#Brassicaceae
caan_seeds$Family[caan_seeds$Treatment=="BRRA 50%"] <- "Brassicaceae"
caan_seeds$Family[caan_seeds$Treatment=="SIAL 50%"] <- "Brassicaceae"
caan_seeds$Family[caan_seeds$Treatment=="ERSA 50%"] <- "Brassicaceae"
caan_seeds$Family[caan_seeds$Treatment=="BROL 50%"] <- "Brassicaceae"

#Convolvulaceae
caan_seeds$Family[caan_seeds$Treatment=="IPAQ 50%"] <- "Convolvulaceae"
caan_seeds$Family[caan_seeds$Treatment=="IPPU 50%"] <- "Convolvulaceae"

caan_cross <- subset(caan_seeds, Treatment=="CROSS")

#Now we prepare a loop to do it fast for all the species
#Again we sort alphabetically
families<- sort(unique(caan_seeds$Family))

b <- NULL
x <- NULL
for (i in families){
  a<-cohen.d(caan_seeds$Seed_set[caan_seeds$Family==i], caan_cross$Seed_set)
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

caan_effect_size <- cbind( families, cohen_d,cbind(lower, upper))
colnames(caan_effect_size) <- c("Family", "Cohen_d", "Lower", "Upper")

caan_effect_size_cross <- subset(caan_effect_size, Family=="C. annuum")
caan_effect_size_b <-subset(caan_effect_size, Family=="Brassicaceae")
caan_effect_size_c <- subset(caan_effect_size, Family=="Convolvulaceae")
caan_effect_size_s <-subset(caan_effect_size, Family=="Solanaceae")

caan_effect_size <- rbind(caan_effect_size_cross,caan_effect_size_b,
                          caan_effect_size_c,caan_effect_size_s)

caan_effect_size$Family <- factor(caan_effect_size$Family, levels = caan_effect_size$Family)


p1<- ggplot(caan_effect_size, aes(Family,Cohen_d, size=10)) + theme_bw(base_size=10)
p1 + geom_point(show.legend = FALSE,aes(color=factor(Family))) +
  geom_errorbar(show.legend=FALSE, aes(x = Family, ymin = Lower, 
                                       ymax = Upper, size=2,color=factor(Family)), width = 0.2)+
  scale_color_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  scale_fill_manual("Family",values=c("#E69F00", "#0072B2", "#009E73", "#D55E00"))+
  xlab("Treatments") + ylab("Cohen's d") + rotate()+guides(fill=FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")




