# In this script I'm going to create a table with the different traits of the species

#Solanaceae
solanaceae <- c("Petunia","Capsicum","Tomato","Eggplant")
sol_fam <- rep("Solanaceae",4)
sol_genus <- c("Petunia","Capsicum","Solanum","Solanum")


#Brassicaceae
brassicaceae <- c("Pak choi", "Wild cabbage" ,"Rocket", "White mustard")
bra_fam <- rep("Brassicaceae",4)
bra_genus <- c("Brassica", "Brassica" ,"Eruca", "Sinapis")

#Convolvulaceae
convolvulaceae <- c("Water morning glory", "Morning glory")
con_fam <- c("Convolvulaceae","Convolvulaceae")
con_genus <- c("Ipomoea", "Ipomoea")

#linking vectors
species <- c(solanaceae,brassicaceae, convolvulaceae)
genus <- c(sol_genus,bra_genus,con_genus)
family <- c(sol_fam,bra_fam,con_fam)
stigma_type <- rep("",10)
self_incompatibility_system <- rep("",10)
self_incompatibility_system_ref <- rep("",10)

varieties <- rep("",10)


tab <- cbind(family,genus,species, varieties)
View(tab)

#Adding stigma type column 
tab<- cbind(tab,stigma_type, self_incompatibility_system, self_incompatibility_system_ref)
tab<- data.frame(tab, stringsAsFactors=FALSE)
class(tab)

#Stigma type based on Shivanna 1977 and my own
#Petunia Integrifolia stigma
tab[1,5]<- "wet"
#Capsicum anuum
tab[2,5]<- "wet"
#Solanum lycopersicum
tab[3,5]<- "wet"
#Solanum melongera
tab[4,5]<- "wet"
#Brassicas and convolvulaceas
tab[5:10,5]<- "dry"

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
tab[2,11] <- round(mean(ovules$CAAN))
tab[2,12] <- round(sd(ovules$CAAN),2)
#S. lycopersicum ovules average
tab[3,11] <- round(mean(ovules$SOLY))
tab[3,12] <- round(sd(ovules$SOLY),2)
#S. melongera ovules average
tab[4,11] <- round(mean(ovules$SOME))
tab[4,12] <- round(sd(ovules$SOME),2)
#P. integrifolia ovules average
tab[1,11] <- round(mean(ovules$PEIN))
tab[1,12] <- round(sd(ovules$PEIN),2)
#B.oleracea ovules average
tab[5,11] <- round(mean(ovules$BROL))
tab[5,12] <- round(sd(ovules$BROL),2)
#B. rapa ovules average
tab[6,11] <- round(mean(ovules$BRRA))
tab[6,12] <- round(sd(ovules$BRRA),2)
#E. sativa ovules average
tab[7,11] <- round(mean(ovules$ERSA))
tab[7,12] <- round(sd(ovules$ERSA),2)
#S. alba ovules average
tab[8,11] <- round(mean(ovules$SIAL))
tab[8,12] <- round(sd(ovules$SIAL),2)
#I. aquatica ovules (fix number)
tab[9,11] <- 4
tab[9,12] <- NA
#I. purpurea ovules (fix number)
tab[10,11] <- 6
tab[10,12] <- NA

#loading pollen dataset to fill columns
pollen <- read.csv("data/Pollen.csv", sep = ";")
#Pollen counted with Neubaeur chamber
#C=P.grains counted*10.000/Number of squares
#so C=P.grains*1000/9;1000 

pollen <- pollen/9 

#P. integrifolian pollen average
tab[1,9] <- round(mean(pollen$PEIN))
tab[1,10] <- round(sd(pollen$PEIN),2)
#C. anuum pollen average
tab[2,9] <- round(mean(pollen$CAAN))
tab[2,10] <- round(sd(pollen$CAAN),2)
#S. lycopersicum pollen average
tab[3,9] <- round(mean(pollen$SOLY))
tab[3,10] <- round(sd(pollen$SOLY),2)
#S. melongera pollen average
tab[4,9] <- round(mean(pollen$SOME))
tab[4,10] <- round(sd(pollen$SOME),2)
#B. oleracea pollen average
tab[5,9] <- round(mean(pollen$BROL))
tab[5,10] <- round(sd(pollen$BROL),2)
#B. rapa pollen average
tab[6,9] <- round(mean(pollen$BRRA))
tab[6,10] <- round(sd(pollen$BRRA),2)
#E. sativa pollen average
tab[7,9] <- round(mean(pollen$ERSA))
tab[7,10] <- round(sd(pollen$ERSA),2)
#S. alba pollen average
tab[8,9] <- round(mean(pollen$SIAL))
tab[8,10] <- round(sd(pollen$SIAL),2)
#I. purpurea pollen average
tab[9,9] <- round(mean(pollen$IPAQ, na.rm = T))
tab[9,10] <- round(sd(pollen$IPAQ, na.rm = T),2)
#I. aquatica pollen average
tab[10,9] <- round(mean(pollen$IPPU))
tab[10,10] <- round(sd(pollen$IPPU,na.rm = T),2)

#Anthers
#P. integrifolia
tab[1,14] <- 5
#C. anuum
tab[2,14] <- 6
#S. lycopersicum
tab[3,14] <- 6
#S. melongera
tab[4,14] <- 6
#I.aquatica
tab[9,14] <- 5
#I.purpurea
tab[10,14] <- 5
#Brassicaceae
tab[5:8,14] <- 6

#Pollen ovule ratio, total pollen per flower/n of ovules
tab$pollen_ovule_ratio <- round(as.numeric(tab[,9])*as.numeric(tab[,14])/as.numeric(tab[,11]),2)

#Varieties
tab[2,4] <- "California Wonder"
tab[3,4] <- "Tommy Toe"
tab[4,4] <- "Little fingers"
tab[5,4] <- "Capitata"
tab[6,4] <- "Chinensis"

#Pollen size (um) Equatorial diameter, almost all have rounded shape when hydratated
pollen_size <- read.csv("data/pollen_size.csv", sep = ";")

#Fixing number of decimals per column
tab[,10] <- format(as.numeric(tab[,10]),nsmall = 2)

#PEIN
tab[1,8] <- mean(pollen_size$PEIN)
#CAAN
tab[2,8] <- mean(pollen_size$CAAN)
#SOLY
tab[3,8] <- mean(pollen_size$SOLY)
#SOME
tab[4,8] <- mean(pollen_size$SOME)
#BROL
tab[5,8] <- mean(pollen_size$BROL)
#BRRA
tab[6,8] <- mean(pollen_size$BRRA)
#ERSA
tab[7,8] <- mean(pollen_size$ERSA)
#SIAL
tab[8,8] <- mean(pollen_size$SIAL)
#IPAQ
tab[9,8] <- mean(pollen_size$IPAQ)
#IPPU
tab[10,8] <- mean(pollen_size$IPPU)


#Fixing number of decimals per column
tab[,8] <- as.numeric(tab[,8])
tab[,8] <- round(tab[,8],2)

#Adding compatibility system
#Petunia SI
tab[1,6] <- "Gametophytic self-incompatibility"
tab[1,7] <- "(Sims & Robins, 2009); DOI: 10.1007/978-0-387-84796-2 5"
#Capsicum
tab[2,6] <- "Self-compatible"
tab[2,7] <- "(Onus & Pickersgill, 2004)"
#S. lycopersicum  
tab[3,6] <-  "Self-compatible"
tab[3,7] <-  "(Whalen & Anderson, 1981); DOI: 10.2307/1220077"
#S. melongera
tab[4,6] <-  "Self-compatible"
tab[4,7] <-  "(Whalen & Anderson, 1981); DOI: 10.2307/1220077"
#B. oleracea
tab[5,6] <- "Sporophytic self-incompatibility"
tab[5,7] <- "(Kitashiba & Nasrallah, 2014); DOI:10.1270/jsbbs.64.23"
#B. rapa 
tab[6,6] <- "Sporophytic self-incompatibility"
tab[6,7] <- "(Kitashiba & Nasrallah, 2014); DOI:10.1270/jsbbs.64.23"
#E. sativa
tab[7,6] <- "Sporophytic self-incompatibility"
tab[7,7] <- "(Verma et al., 1977); DOI:10.1098/rspb.1977.0034"
#S. alba
#Different cultivars have different compatibility
tab[8,6] <- "Sporophytic self-(in)compatibility"
tab[8,7] <-  "(Zeng & Cheng, 2014); DOI: 10.1007/s11032-013-9943-8" 
#I. aquatica
tab[9,6] <- "Self-compatible"
tab[9,7] <-  "(Les, 2017); DOI: 10.1201/9781315118116" 
#S. purpurea
#Different cultivars have different compatibility
tab[10,6] <- "Self-compatible"
tab[10,7] <-  "(Smith & Rausher, 2006); DOI: 10.1111/j.1469-8137.2006.01933.x" 

write.table(tab, file = "data/tab_common_names.csv")


barplot(tab$pollen_ovule_ratio, x.axis)
library(ggplot2)
library(colorspace)
p <- ggplot(data=tab, aes(x=species, y=pollen_ovule_ratio))+
  geom_bar(stat="identity", width=0.5)+coord_flip()
p + ggtitle("Pollen/ovule ratio per species")

colours <- rainbow_hcl(4, start = 30, end = 300)


p <- ggplot(data=tab, aes(x=reorder(species, pollen_ovule_ratio, colour = cut), y=pollen_ovule_ratio))+aes(fill=family)+
  geom_bar(stat="identity", width=0.5)+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_manual(values=colours)
p + ggtitle("Pollen/ovule ratio per species") + theme(plot.title = element_text(hjust = 0.5))+ labs(x = "Species")


