#Analysis mantel with effect sizes
#I have to load first all the script of effect size of analysis to run this one

soly_effect_size$Focal <- "SOLY"
soly_effect_size<- soly_effect_size[-2,]

some_effect_size$Focal <- "SOME"
some_effect_size<- some_effect_size[-1,]

pein_effect_size$Focal <- "PEIN"
pein_effect_size<- pein_effect_size[-3,]

caan_effect_size$Focal <- "CAAN"
caan_effect_size<- caan_effect_size[-4,]

brol_effect_size$Focal <- "BROL"
brol_effect_size<- brol_effect_size[-10,]

brra_effect_size$Focal <- "BRRA"
brra_effect_size<- brra_effect_size[-9,]

sial_effect_size$Focal <- "SIAL"
sial_effect_size<- sial_effect_size[-7,]

ersa_effect_size$Focal <- "ERSA"
ersa_effect_size<- ersa_effect_size[-8,]

ippu_effect_size$Focal <- "IPPU"
ippu_effect_size<- ippu_effect_size[-5,]

ipaq_effect_size$Focal <- "IPAQ"
ipaq_effect_size<- ipaq_effect_size[-6,]

e_size <- rbind(soly_effect_size,some_effect_size,pein_effect_size, caan_effect_size, brol_effect_size, brra_effect_size,
      sial_effect_size, ersa_effect_size, ippu_effect_size, ipaq_effect_size)

e_size <- e_size[,-c(2,3,5,6)]
str(e_size)
e_size$Species<- as.character(e_size$Species)


e_size$Species <- gsub("50%", "", e_size$Species)
e_size$le <- seq(1:90)
e_size[22,1] <- e_size[4,1]
e_size <- e_size[,-4]
matrix_effect_size <- tapply(e_size$Cohen_d, e_size[c("Focal", "Species")], mean)
matrix_effect_size <- matrix_effect_size[order(rownames(matrix_effect_size)), order(colnames(matrix_effect_size))] 
diag(matrix_effect_size) <- 0
matrix_effect_size[8,5]<- matrix_effect_size[8,6]
matrix_effect_size[10,5]<- matrix_effect_size[10,6]

saveRDS(matrix_effect_size, "Data/matrix_effect_size.RData")
