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
brol_cross_mean=mean(brol_cross$Seed.production)
brol_cross_sd=sd(brol_cross$Seed.production)


brol_self=brol_seed_set_final[brol_seed_set_final$Treatment=="Self",]
brol_self$Seed.production=as.numeric(brol_self$Seed.production)
brol_self$Seed.production=brol_self$Seed.production/traits_all[traits_all$species=="Brassica oleracea","mean_ovules"]
brol_self_mean=mean(brol_self$Seed.production)
brol_self_sd=sd(brol_self$Seed.production)

brol_control=brol_seed_set_final[brol_seed_set_final$Treatment=="Control",]
brol_control$Seed.production=as.numeric(brol_control$Seed.production)
brol_control$Seed.production=brol_control$Seed.production/traits_all[traits_all$species=="Brassica oleracea","mean_ovules"]
brol_control_mean=mean(brol_control$Seed.production)
brol_control_sd=sd(brol_control$Seed.production)

brol_FC=brol_seed_set_final[brol_seed_set_final$Treatment=="FC",]
brol_FC$Seed.production=as.numeric(brol_FC$Seed.production)
brol_FC$Seed.production=brol_FC$Seed.production/traits_all[traits_all$species=="Brassica oleracea","mean_ovules"]
brol_FC_mean=mean(brol_FC$Seed.production)
brol_FC_sd=sd(brol_FC$Seed.production)

#2nd species Brassica rapa
brra_seed_set_final <- read.csv("Rmd/Data/brra_seed_set_final.csv")

brra_cross=brra_seed_set_final[brra_seed_set_final$Treatment=="Cross",]
brra_cross$Seed.production=as.numeric(brra_cross$Seed.production)
brra_cross$Seed.production=brra_cross$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_cross_mean=mean(brra_cross$Seed.production)
brra_cross_sd=sd(brra_cross$Seed.production)

brra_self=brra_seed_set_final[brra_seed_set_final$Treatment=="Self",]
brra_self$Seed.production=as.numeric(brra_self$Seed.production)
brra_self$Seed.production=brra_self$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_self_mean=mean(brra_self$Seed.production)
brra_self_sd=sd(brra_self$Seed.production)


brra_control=brra_seed_set_final[brra_seed_set_final$Treatment=="Control",]
brra_control$Seed.production=as.numeric(brra_control$Seed.production)
brra_control$Seed.production=brra_control$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_control_mean=mean(brra_control$Seed.production)
brra_control_sd=sd(brra_control$Seed.production)


brra_FC=brra_seed_set_final[brra_seed_set_final$Treatment=="Flower Control",]
brra_FC$Seed.production=as.numeric(brra_FC$Seed.production)
brra_FC$Seed.production=brra_FC$Seed.production/traits_all[traits_all$species=="Brassica rapa","mean_ovules"]
brra_FC_mean=mean(brra_FC$Seed.production)
brra_FC_sd=sd(brra_FC$Seed.production)

#3rd species Eruca versicaria
ersa_seed_set_final <- read.csv("Rmd/Data/ersa_seed_set_final.csv")

ersa_cross=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Cross",]
ersa_cross$Seed.production=as.numeric(ersa_cross$Seed.production)
ersa_cross$Seed.production=ersa_cross$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_cross_mean=mean(ersa_cross$Seed.production)
ersa_cross_sd=sd(ersa_cross$Seed.production)


ersa_self=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Self",]
ersa_self$Seed.production=as.numeric(ersa_self$Seed.production)
ersa_self$Seed.production=ersa_self$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_self_mean=mean(ersa_self$Seed.production)
ersa_self_sd=sd(ersa_self$Seed.production)


ersa_control=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Control",]
ersa_control$Seed.production=as.numeric(ersa_control$Seed.production)
ersa_control$Seed.production=ersa_control$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_control_mean=mean(ersa_control$Seed.production)
ersa_control_sd=sd(ersa_control$Seed.production)


ersa_FC=ersa_seed_set_final[ersa_seed_set_final$Treatment=="Flower Control",]
ersa_FC$Seed.production=as.numeric(ersa_FC$Seed.production)
ersa_FC$Seed.production=ersa_FC$Seed.production/traits_all[traits_all$species=="Eruca versicaria","mean_ovules"]
ersa_FC_mean=mean(ersa_FC$Seed.production)
ersa_FC_sd=sd(ersa_FC$Seed.production)


#4th species sinapis alba
sial_seed_set_final <- read.csv("Rmd/Data/sial_seed_set_final.csv")

sial_cross=sial_seed_set_final[sial_seed_set_final$Treatment=="Cross",]
sial_cross$Seed.production=as.numeric(sial_cross$Seed.production)
sial_cross$Seed.production=sial_cross$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_cross_mean=mean(sial_cross$Seed.production)
sial_cross_sd=sd(sial_cross$Seed.production)


sial_self=sial_seed_set_final[sial_seed_set_final$Treatment=="Self",]
sial_self$Seed.production=as.numeric(sial_self$Seed.production)
sial_self$Seed.production=sial_self$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_self_mean=mean(sial_self$Seed.production)
sial_self_sd=sd(sial_self$Seed.production)


sial_control=sial_seed_set_final[sial_seed_set_final$Treatment=="Control",]
sial_control$Seed.production=as.numeric(sial_control$Seed.production)
sial_control$Seed.production=sial_control$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_control_mean=mean(sial_control$Seed.production)
sial_control_sd=sd(sial_control$Seed.production)


sial_FC=sial_seed_set_final[sial_seed_set_final$Treatment=="Flower Control",]
sial_FC$Seed.production=as.numeric(sial_FC$Seed.production)
sial_FC$Seed.production=sial_FC$Seed.production/traits_all[traits_all$species=="Sinapis alba","mean_ovules"]
sial_FC_mean=mean(sial_FC$Seed.production)
sial_FC_sd=sd(sial_FC$Seed.production)

#5th species capsicum annuum

caan_seed_set_final <- read.csv("Rmd/Data/caan_seed_set_final.csv")

caan_cross=caan_seed_set_final[caan_seed_set_final$Treatment=="CROSS",]
caan_cross$Seed.production=as.numeric(caan_cross$Seed.production)
caan_cross$Seed.production=caan_cross$Seed.production/traits_all[traits_all$species=="Capsicum annuum","mean_ovules"]
caan_cross_mean=mean(caan_cross$Seed.production)
caan_cross_sd=sd(caan_cross$Seed.production)


caan_self=caan_seed_set_final[caan_seed_set_final$Treatment=="SELF",]
caan_self$Seed.production=as.numeric(caan_self$Seed.production)
caan_self$Seed.production=caan_self$Seed.production/traits_all[traits_all$species=="Capsicum annuum","mean_ovules"]
caan_self_mean=mean(caan_self$Seed.production)
caan_self_sd=sd(caan_self$Seed.production)

caan_control=caan_seed_set_final[caan_seed_set_final$Treatment=="CONTROL",]
caan_control$Seed.production=as.numeric(caan_control$Seed.production)
caan_control$Seed.production=caan_control$Seed.production/traits_all[traits_all$species=="Capsicum annuum","mean_ovules"]
caan_control_mean=mean(caan_control$Seed.production)
caan_control_sd=sd(caan_control$Seed.production)


caan_FC=caan_seed_set_final[caan_seed_set_final$Treatment=="FLOWER CONTROL",]
caan_FC$Seed.production=as.numeric(caan_FC$Seed.production)
caan_FC$Seed.production=caan_FC$Seed.production/traits_all[traits_all$species=="Capsicum annuum","mean_ovules"]
caan_FC_mean=mean(caan_FC$Seed.production)
caan_FC_sd=sd(caan_FC$Seed.production)

#6th species petunia integrifolia
pein_seed_set_final <- read.csv("Rmd/Data/pein_seed_set_final.csv")

pein_cross=pein_seed_set_final[pein_seed_set_final$Treatment=="CROSS",]
pein_cross$Seed.production=as.numeric(pein_cross$Seed.production)
pein_cross$Seed.production=pein_cross$Seed.production/traits_all[traits_all$species=="Petunia integrifolia","mean_ovules"]
pein_cross_mean=mean(pein_cross$Seed.production)
pein_cross_sd=sd(pein_cross$Seed.production)

pein_self=pein_seed_set_final[pein_seed_set_final$Treatment=="SELF",]
pein_self$Seed.production=as.numeric(pein_self$Seed.production)
pein_self$Seed.production=pein_self$Seed.production/traits_all[traits_all$species=="Petunia integrifolia","mean_ovules"]
pein_self_mean=mean(pein_self$Seed.production)
pein_self_sd=sd(pein_self$Seed.production)

pein_control=pein_seed_set_final[pein_seed_set_final$Treatment=="CONTROL",]
pein_control$Seed.production=as.numeric(pein_control$Seed.production)
pein_control$Seed.production=pein_control$Seed.production/traits_all[traits_all$species=="Petunia integrifolia","mean_ovules"]
pein_control_mean=mean(pein_control$Seed.production)
pein_control_sd=sd(pein_control$Seed.production)

pein_FC=pein_seed_set_final[pein_seed_set_final$Treatment=="FLOWER CONTROL",]
pein_FC$Seed.production=as.numeric(pein_FC$Seed.production)
pein_FC$Seed.production=pein_FC$Seed.production/traits_all[traits_all$species=="Petunia integrifolia","mean_ovules"]
pein_FC_mean=mean(pein_FC$Seed.production)
pein_FC_sd=sd(pein_FC$Seed.production)

#7th species solanum lycopersicum

soly_seed_set_final <- read.csv("Rmd/Data/soly_seed_set_final.csv")

soly_cross=soly_seed_set_final[soly_seed_set_final$Treatment=="Cross",]
soly_cross$Seed.production=as.numeric(soly_cross$Seed.production)
soly_cross$Seed.production=soly_cross$Seed.production/traits_all[traits_all$species=="Solanum lycopersicum","mean_ovules"]
soly_cross_mean=mean(soly_cross$Seed.production)
soly_cross_sd=sd(soly_cross$Seed.production)

soly_self=soly_seed_set_final[soly_seed_set_final$Treatment=="Self",]
soly_self$Seed.production=as.numeric(soly_self$Seed.production)
soly_self$Seed.production=soly_self$Seed.production/traits_all[traits_all$species=="Solanum lycopersicum","mean_ovules"]
soly_self_mean=mean(soly_self$Seed.production)
soly_self_sd=sd(soly_self$Seed.production)

soly_control=soly_seed_set_final[soly_seed_set_final$Treatment=="Control",]
soly_control$Seed.production=as.numeric(soly_control$Seed.production)
soly_control$Seed.production=soly_control$Seed.production/traits_all[traits_all$species=="Solanum lycopersicum","mean_ovules"]
soly_control_mean=mean(soly_control$Seed.production)
soly_control_sd=sd(soly_control$Seed.production)

soly_FC=soly_seed_set_final[soly_seed_set_final$Treatment=="Flower control",]
soly_FC$Seed.production=as.numeric(soly_FC$Seed.production)
soly_FC$Seed.production=soly_FC$Seed.production/traits_all[traits_all$species=="Solanum lycopersicum","mean_ovules"]
soly_FC_mean=mean(soly_FC$Seed.production)
soly_FC_sd=sd(soly_FC$Seed.production)

#8th species solanum melongena

some_seed_set_final <- read.csv("Rmd/Data/some_seed_set_final.csv")

some_cross=some_seed_set_final[some_seed_set_final$Treatment=="Cross",]
some_cross$Seed.production=as.numeric(some_cross$Seed.production)
some_cross$Seed.production=some_cross$Seed.production/traits_all[traits_all$species=="Solanum melongena","mean_ovules"]
some_cross_mean=mean(some_cross$Seed.production)
some_cross_sd=sd(some_cross$Seed.production)

some_self=some_seed_set_final[some_seed_set_final$Treatment=="Self",]
some_self$Seed.production=as.numeric(some_self$Seed.production)
some_self$Seed.production=some_self$Seed.production/traits_all[traits_all$species=="Solanum melongena","mean_ovules"]
some_self_mean=mean(some_self$Seed.production)
some_self_sd=sd(some_self$Seed.production)

some_control=some_seed_set_final[some_seed_set_final$Treatment=="Control",]
some_control$Seed.production=as.numeric(some_control$Seed.production)
some_control$Seed.production=some_control$Seed.production/traits_all[traits_all$species=="Solanum melongena","mean_ovules"]
some_control_mean=mean(some_control$Seed.production)
some_control_sd=sd(some_control$Seed.production)

some_FC=some_seed_set_final[some_seed_set_final$Treatment=="Flower control",]
some_FC$Seed.production=as.numeric(some_FC$Seed.production)
some_FC$Seed.production=some_FC$Seed.production/traits_all[traits_all$species=="Solanum melongena","mean_ovules"]
some_FC_mean=mean(some_FC$Seed.production)
some_FC_sd=sd(some_FC$Seed.production)

#9th species Ipomoea aquatica
ipaq_seed_set_final <- read.csv("Rmd/Data/ipaq_seed_set_final.csv")

ipaq_cross=ipaq_seed_set_final[ipaq_seed_set_final$Treatment=="Cross",]
ipaq_cross$Seed.production=as.numeric(ipaq_cross$Seed.production)
ipaq_cross$Seed.production=ipaq_cross$Seed.production/traits_all[traits_all$species=="Ipomoea aquatica","mean_ovules"]
ipaq_cross_mean=mean(ipaq_cross$Seed.production)
ipaq_cross_sd=sd(ipaq_cross$Seed.production)

ipaq_self=ipaq_seed_set_final[ipaq_seed_set_final$Treatment=="Self",]
ipaq_self$Seed.production=as.numeric(ipaq_self$Seed.production)
ipaq_self$Seed.production=ipaq_self$Seed.production/traits_all[traits_all$species=="Ipomoea aquatica","mean_ovules"]
ipaq_self_mean=mean(ipaq_self$Seed.production)
ipaq_self_sd=sd(ipaq_self$Seed.production)

ipaq_control=ipaq_seed_set_final[ipaq_seed_set_final$Treatment=="Control",]
ipaq_control$Seed.production=as.numeric(ipaq_control$Seed.production)
ipaq_control$Seed.production=ipaq_control$Seed.production/traits_all[traits_all$species=="Ipomoea aquatica","mean_ovules"]
ipaq_control_mean=mean(ipaq_control$Seed.production)
ipaq_control_sd=sd(ipaq_control$Seed.production)


ipaq_FC=ipaq_seed_set_final[ipaq_seed_set_final$Treatment=="Flower control",]
ipaq_FC$Seed.production=as.numeric(ipaq_FC$Seed.production)
ipaq_FC$Seed.production=ipaq_FC$Seed.production/traits_all[traits_all$species=="Ipomoea aquatica","mean_ovules"]
ipaq_FC_mean=mean(ipaq_FC$Seed.production)
ipaq_FC_sd=sd(ipaq_FC$Seed.production)

#10th species Ipomoea purpurea
ippu_seed_set_final <- read.csv("Rmd/Data/ippu_seed_set_final.csv")

ippu_cross=ippu_seed_set_final[ippu_seed_set_final$Treatment=="Cross",]
ippu_cross$Seed.production=as.numeric(ippu_cross$Seed.production)
ippu_cross$Seed.production=ippu_cross$Seed.production/traits_all[traits_all$species=="Ipomoea purpurea","mean_ovules"]
ippu_cross_mean=mean(ippu_cross$Seed.production)
ippu_cross_sd=sd(ippu_cross$Seed.production)

ippu_self=ippu_seed_set_final[ippu_seed_set_final$Treatment=="Self",]
ippu_self$Seed.production=as.numeric(ippu_self$Seed.production)
ippu_self$Seed.production=ippu_self$Seed.production/traits_all[traits_all$species=="Ipomoea purpurea","mean_ovules"]
ippu_self_mean=mean(ippu_self$Seed.production)
ippu_self_sd=sd(ippu_self$Seed.production)

ippu_control=ippu_seed_set_final[ippu_seed_set_final$Treatment=="Control",]
ippu_control$Seed.production=as.numeric(ippu_control$Seed.production)
ippu_control$Seed.production=ippu_control$Seed.production/traits_all[traits_all$species=="Ipomoea purpurea","mean_ovules"]
ippu_control_mean=mean(ippu_control$Seed.production)
ippu_control_sd=sd(ippu_control$Seed.production)

ippu_FC=ippu_seed_set_final[ippu_seed_set_final$Treatment=="Flower control",]
ippu_FC$Seed.production=as.numeric(ippu_FC$Seed.production)
ippu_FC$Seed.production=ippu_FC$Seed.production/traits_all[traits_all$species=="Ipomoea purpurea","mean_ovules"]
ippu_FC_mean=mean(ippu_FC$Seed.production)
ippu_FC_sd=sd(ippu_FC$Seed.production)

#% seed set for the species for the different 4 Treatments
cross_mean <- c(brol_cross_mean, brra_cross_mean, ersa_cross_mean, 
           sial_cross_mean, ipaq_cross_mean, ippu_cross_mean, 
           caan_cross_mean, pein_cross_mean,soly_cross_mean, some_cross_mean)
cross_mean <- cross_mean*100

cross_mean=replace(cross_mean, cross_mean>100, 100)

cross_sd <- c(brol_cross_sd, brra_cross_sd, ersa_cross_sd, 
                sial_cross_sd, ipaq_cross_sd, ippu_cross_sd, 
                caan_cross_sd, pein_cross_sd,soly_cross_sd, some_cross_sd)
cross_sd <- cross_sd*100

self_mean <- c(brol_self_mean, brra_self_mean, ersa_self_mean, 
          sial_self_mean, ipaq_self_mean, ippu_self_mean, 
          caan_self_mean, pein_self_mean,soly_self_mean, some_self_mean)
self_mean <-self_mean*100

self_sd <- c(brol_self_sd, brra_self_sd, ersa_self_sd, 
               sial_self_sd, ipaq_self_sd, ippu_self_sd, 
               caan_self_sd, pein_self_sd,soly_self_sd, some_self_sd)
self_sd <-self_sd*100

nat_self_mean <- c(brol_FC_mean, brra_FC_mean, ersa_FC_mean, 
              sial_FC_mean, ipaq_FC_mean, ippu_FC_mean,
              caan_FC_mean, pein_FC_mean,soly_FC_mean, some_FC_mean)
nat_self_mean <- nat_self_mean*100

nat_self_sd <- c(brol_FC_sd, brra_FC_sd, ersa_FC_sd, 
                   sial_FC_sd, ipaq_FC_sd, ippu_FC_sd,
                   caan_FC_sd, pein_FC_sd,soly_FC_sd, some_FC_sd)
nat_self_sd <- nat_self_sd*100

apo_mean <- c(brol_control_mean, brra_control_mean, ersa_control_mean, 
         sial_control_mean, ipaq_control_mean, ippu_control_mean, 
         caan_control_mean, pein_control_mean,soly_control_mean, some_control_mean)

apo_sd <- c(brol_control_sd, brra_control_sd, ersa_control_sd, 
              sial_control_sd, ipaq_control_sd, ippu_control_sd, 
              caan_control_sd, pein_control_sd,soly_control_sd, some_control_sd)

apo_sd <-apo_sd*100

save.image(file='Manuscript_draft/table_treatments.RData')



