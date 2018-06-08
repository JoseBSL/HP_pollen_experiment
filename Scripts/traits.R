# In this script I'm going to create a table with the different traits of the species

#Solanaceae
solanaceae <- c("Petunia integrifolia","Capsicum anuum","Solanum lycopersicum","Solanum melongera")
sol_fam <- rep("Solanaceae",4)
sol_genus <- c("Petunia","Capsicum","Solanum","Solanum")


#Brassicaceae
brassicaceae <- c("Brassica oleracea", "Brassica rapa" ,"Eruca sativa", "Sinapis alba")
bra_fam <- rep("Brassicaceae",4)
bra_genus <- c("Brassica", "Brassica" ,"Eruca", "Sinapis")


convolvulaceae <- c("Ipomoea aquatica", "Ipomoea purpurea")
con_fam <- c("Convolvulaceae","Convolvulaceae")
con_genus <- c("Ipomoea", "Ipomoea")


species <- c(solanaceae,brassicaceae, convolvulaceae)
genus <- c(sol_genus,bra_genus,con_genus)
family <- c(sol_fam,bra_fam,con_fam)
species<- data.frame(species)


tab <- cbind(family,genus,species)
View(tab)
