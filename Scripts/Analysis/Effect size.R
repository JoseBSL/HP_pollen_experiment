#In this script I'm going to calculate the effect sizes and plot them.
#Moreover I will compare differences among species and among families

#loadlibrary
#install.packages("effsize")
library(effsize)

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

soly_seeds <- subset(y, Species=="SOLY")

soly_cross <- subset(soly_seeds, Treatment=="CROSS")
soly_ippu <- subset(soly_seeds, Treatment=="IPPU 50%")
soly_ipaq <- subset(soly_seeds, Treatment=="IPAQ 50%")

a <- cohen.d(soly_ippu$Seed_set, soly_cross$Seed_set)
b <- cohen.d(soly_ipaq$Seed_set, soly_cross$Seed_set)


species<- unique(soly_seeds$Treatment)
y <- NULL
x <- NULL
for (i in species){
  a<-cohen.d(soly_seeds$Seed_set[soly_seeds$Treatment==i], soly_cross$Seed_set)
  y <- rbind(y, a[3])
  x<- rbind(x, a[4])
}

soly_effect_size <- cbind(species,y,as.data.frame(x))

