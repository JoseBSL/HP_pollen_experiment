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

tab <- cbind(tab, pollen_size)
