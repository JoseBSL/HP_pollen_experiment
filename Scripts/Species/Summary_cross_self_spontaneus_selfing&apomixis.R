#load seed sets

pein_seed_set_final <- read.csv("Rmd/Data/pein_seed_set_final.csv")
some_seed_set_final <- read.csv("Rmd/Data/some_seed_set_final.csv")
soly_seed_set_final <- read.csv("Rmd/Data/soly_seed_set_final.csv")
caan_seed_set_final <- read.csv("Rmd/Data/caan_seed_set_final.csv")

ipaq_seed_set_final <- read.csv("Rmd/Data/ipaq_seed_set_final.csv")
ippu_seed_set_final <- read.csv("Rmd/Data/ippu_seed_set_final.csv")

#In order to have the percentage of ssed set I need to know the total number ov ovules of each species and divide by it
#Now I load the table of traits that has ovule number
traits_all <- read.csv("Data/traits_all.csv", sep=",")
traits_all=traits_all[,c(2,7)]
str(traits_all)
traits_all$mean_ovules=as.numeric(traits_all$mean_ovules)




#1st species Brassica oleracea
brol_seed_set_final <- read.csv("Rmd/Data/brol_seed_set_final.csv")

brol_cross=brol_seed_set_final[brol_seed_set_final$Treatment=="Cross",]
brol_cross$Seed.production=as.numeric(brol_cross$Seed.production)
brol_cross$Seed.production=brol_cross$Seed.production/traits_all[traits_all$species=="Brassica oleracea","mean_ovules"]
brol_cross=mean(brol_cross$Seed.production)

brol_self=brol_seed_set_final[brol_seed_set_final$Treatment=="Self",]
brol_self$Seed.production=as.numeric(brol_self$Seed.production)
brol_self$Seed.production=brol_self$Seed.production/traits_all[traits_all$species=="Brassica oleracea","mean_ovules"]
brol_self=mean(brol_self$Seed.production)

brol_control=brol_seed_set_final[brol_seed_set_final$Treatment=="control",]
brol_control$Seed.production=as.numeric(brol_control$Seed.production)
brol_control$Seed.production=brol_control$Seed.production/traits_all[traits_all$species=="Brassica oleracea","mean_ovules"]
brol_control=mean(brol_control$Seed.production)

brol_FC=brol_seed_set_final[brol_seed_set_final$Treatment=="FC",]
brol_FC$Seed.production=as.numeric(brol_FC$Seed.production)
brol_FC$Seed.production=brol_FC$Seed.production/traits_all[traits_all$species=="Brassica oleracea","mean_ovules"]
brol_FC=mean(brol_FC$Seed.production)


#2nd species Brassica rapa
brra_seed_set_final <- read.csv("Rmd/Data/brra_seed_set_final.csv")

brra_cross=brra_seed_set_final[brra_seed_set_final$Treatment=="Cross",]
brra_cross$Seed.production=as.numeric(brra_cross$Seed.production)
brra_cross$Seed.production=brra_cross$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_cross=mean(brra_cross$Seed.production)

brra_self=brra_seed_set_final[brra_seed_set_final$Treatment=="Self",]
brra_self$Seed.production=as.numeric(brra_self$Seed.production)
brra_self$Seed.production=brra_self$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_self=mean(brra_self$Seed.production)

brra_control=brra_seed_set_final[brra_seed_set_final$Treatment=="Control",]
brra_control$Seed.production=as.numeric(brra_control$Seed.production)
brra_control$Seed.production=brra_control$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_control=mean(brra_control$Seed.production)

brra_FC=brra_seed_set_final[brra_seed_set_final$Treatment=="Flower Control",]
brra_FC$Seed.production=as.numeric(brra_FC$Seed.production)
brra_FC$Seed.production=brra_FC$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_FC=mean(brra_FC$Seed.production)

#3rd species Eruca versicaria
ersa_seed_set_final <- read.csv("Rmd/Data/ersa_seed_set_final.csv")

ersa_cross=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Cross",]
ersa_cross$Seed.production=as.numeric(ersa_cross$Seed.production)
ersa_cross$Seed.production=ersa_cross$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_cross=mean(ersa_cross$Seed.production)

ersa_self=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Self",]
ersa_self$Seed.production=as.numeric(ersa_self$Seed.production)
ersa_self$Seed.production=ersa_self$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_self=mean(ersa_self$Seed.production)

ersa_control=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Control",]
ersa_control$Seed.production=as.numeric(ersa_control$Seed.production)
ersa_control$Seed.production=ersa_control$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_control=mean(ersa_control$Seed.production)

ersa_FC=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Flower Control",]
ersa_FC$Seed.production=as.numeric(ersa_FC$Seed.production)
ersa_FC$Seed.production=ersa_FC$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_FC=mean(ersa_FC$Seed.production)

#4th species sinapis alba
sial_seed_set_final <- read.csv("Rmd/Data/sial_seed_set_final.csv")

sial_cross=sial_seed_set_final[sial_seed_set_final$Treatment=="Cross",]
sial_cross$Seed.production=as.numeric(sial_cross$Seed.production)
sial_cross$Seed.production=sial_cross$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_cross=mean(sial_cross$Seed.production)

sial_self=sial_seed_set_final[sial_seed_set_final$Treatment=="Self",]
sial_self$Seed.production=as.numeric(sial_self$Seed.production)
sial_self$Seed.production=sial_self$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_self=mean(sial_self$Seed.production)

sial_control=sial_seed_set_final[sial_seed_set_final$Treatment=="Control",]
sial_control$Seed.production=as.numeric(sial_control$Seed.production)
sial_control$Seed.production=sial_control$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_control=mean(sial_control$Seed.production)

sial_FC=sial_seed_set_final[sial_seed_set_final$Treatment=="Flower Control",]
sial_FC$Seed.production=as.numeric(sial_FC$Seed.production)
sial_FC$Seed.production=sial_FC$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_FC=mean(sial_FC$Seed.production)