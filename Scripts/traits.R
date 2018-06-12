# In this script I'm going to create a table with the different traits of the species

#Solanaceae
solanaceae <- c("Petunia integrifolia","Capsicum anuum","Solanum lycopersicum","Solanum melongera")
sol_fam <- rep("Solanaceae",4)
sol_genus <- c("Petunia","Capsicum","Solanum","Solanum")


#Brassicaceae
brassicaceae <- c("Brassica oleracea", "Brassica rapa" ,"Eruca sativa", "Sinapis alba")
bra_fam <- rep("Brassicaceae",4)
bra_genus <- c("Brassica", "Brassica" ,"Eruca", "Sinapis")

#Convolvulaceae
convolvulaceae <- c("Ipomoea aquatica", "Ipomoea purpurea")
con_fam <- c("Convolvulaceae","Convolvulaceae")
con_genus <- c("Ipomoea", "Ipomoea")

#linking vectors
species <- c(solanaceae,brassicaceae, convolvulaceae)
genus <- c(sol_genus,bra_genus,con_genus)
family <- c(sol_fam,bra_fam,con_fam)
stigma_type <- rep("",10)
compatibility_system <- rep("",10)
tab <- cbind(family,genus,species)
View(tab)

#Adding stigma type column 
tab<- cbind(tab,stigma_type, compatibility_system)
tab<- data.frame(tab, stringsAsFactors=FALSE)
class(tab)

#Stigma type based on Shivanna 1977 and my own
#Petunia Integrifolia stigma
tab[1,4]<- "wet"
#Capsicum anuum
tab[2,4]<- "wet"
#Solanum lycopersicum
tab[3,4]<- "wet"
#Solanum melongera
tab[4,4]<- "wet"
#Brassicas and convolvulaceas
tab[5:10,4]<- "dry"

View(tab)


pollen_size <- rep("",10)

#Total pollen per anther (average N=20)
mean_pollen_anther <- rep("",10)
sd_pollen_anther <- rep("",10)

#Total number of ovules, (average N=20)
mean_ovules <- rep("",10)
sd_ovules <- rep("",10)

#Pollen/ovule ratio
pollen_ovule_ratio <- rep("",10)

#Anthers
anthers <- rep("",10)

#Unifying new columns
bind <- cbind(pollen_size,mean_pollen_anther,sd_pollen_anther, mean_ovules, sd_ovules, pollen_ovule_ratio, anthers)
bind<- data.frame(bind, stringsAsFactors = F)
tab <- cbind(tab, bind)
tab<- data.frame(tab, stringsAsFactors=F)

#loading ovule dataset to fill columns
ovules <- read.csv("Data/Ovules.csv", sep = ";")
#Round: it does it to the closest number
#when it is 5, it does it to the nearest even number
#Capsicum ovules average
tab[2,9] <- round(mean(ovules$CAAN))
tab[2,10] <- round(sd(ovules$CAAN),2)
#S. lycopersicum ovules average
tab[3,9] <- round(mean(ovules$SOLY))
tab[3,10] <- round(sd(ovules$SOLY),2)
#S. melongera ovules average
tab[4,9] <- round(mean(ovules$SOME))
tab[4,10] <- round(sd(ovules$SOME),2)
#P. integrifolia ovules average
tab[1,9] <- round(mean(ovules$PEIN))
tab[1,10] <- round(sd(ovules$PEIN),2)
#B.oleracea ovules average
tab[5,9] <- round(mean(ovules$BROL))
tab[5,10] <- round(sd(ovules$BROL),2)
#B. rapa ovules average
tab[6,9] <- round(mean(ovules$BRRA))
tab[6,10] <- round(sd(ovules$BRRA),2)
#E. sativa ovules average
tab[7,9] <- round(mean(ovules$ERSA))
tab[7,10] <- round(sd(ovules$ERSA),2)
#S. alba ovules average
tab[8,9] <- round(mean(ovules$SIAL))
tab[8,10] <- round(sd(ovules$SIAL),2)
#I. aquatica ovules (fix number)
tab[9,9] <- 4
tab[9,10] <- NA
#I. purpurea ovules (fix number)
tab[10,9] <- 6
tab[10,10] <- NA

#loading pollen dataset to fill columns
pollen <- read.csv("data/Pollen.csv", sep = ";")
#Pollen counted with Neubaeur chamber (just multiplied by 1000)
#C=P.grains counted*10.000/Number of squares
#so C=P.grains*1000/9;1000 because was in 0.1ml

pollen <- pollen/9 

#P. integrifolian pollen average
tab[1,7] <- round(mean(pollen$PEIN))
tab[1,8] <- round(sd(pollen$PEIN),2)
#C. anuum pollen average
tab[2,7] <- round(mean(pollen$CAAN))
tab[2,8] <- round(sd(pollen$CAAN),2)
#S. lycopersicum pollen average
tab[3,7] <- round(mean(pollen$SOLY))
tab[3,8] <- round(sd(pollen$SOLY),2)
#S. melongera pollen average
tab[4,7] <- round(mean(pollen$SOME))
tab[4,8] <- round(sd(pollen$SOME),2)
#B. oleracea pollen average
tab[5,7] <- round(mean(pollen$BROL))
tab[5,8] <- round(sd(pollen$BROL),2)
#B. rapa pollen average
tab[6,7] <- round(mean(pollen$BRRA))
tab[6,8] <- round(sd(pollen$BRRA),2)
#E. sativa pollen average
tab[7,7] <- round(mean(pollen$ERSA))
tab[7,8] <- round(sd(pollen$ERSA),2)
#S. alba pollen average
tab[8,7] <- round(mean(pollen$SIAL))
tab[8,8] <- round(sd(pollen$SIAL),2)
#I. purpurea pollen average
tab[9,7] <- round(mean(pollen$IPAQ, na.rm = T))
tab[9,8] <- round(sd(pollen$IPAQ, na.rm = T),2)
#I. aquatica pollen average
tab[10,7] <- round(mean(pollen$IPPU))
tab[10,8] <- round(sd(pollen$IPPU,na.rm = T),2)

#Anthers
#P. integrifolia
tab[1,12] <- 5
#C. anuum
tab[2,12] <- 6
#S. lycopersicum
tab[3,12] <- 6
#S. melongera
tab[4,12] <- 6
#I.aquatica
tab[9,12] <- 5
#I.purpurea
tab[10,12] <- 5
#Brassicaceae
tab[5:8,12] <- 6
