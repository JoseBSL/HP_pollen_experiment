#Script to plot the reproductive biology tests
#load libraries
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")

brol <- read.csv("Rmd/Data/brol_seed_set_final.csv")
brra <- read.csv("Rmd/Data/brra_seed_set_final.csv")
ersa <- read.csv("Rmd/Data/ersa_seed_set_final.csv")
sial <- read.csv("Rmd/Data/sial_seed_set_final.csv")
ipaq <- read.csv("Rmd/Data/ipaq_seed_set_final.csv")
ippu <- read.csv("Rmd/Data/ippu_seed_set_final.csv")
caan <- read.csv("Rmd/Data/caan_seed_set_final.csv")
pein <- read.csv("Rmd/Data/pein_seed_set_final.csv")
soly <- read.csv("Rmd/Data/soly_seed_set_final.csv")
some <- read.csv("Rmd/Data/some_seed_set_final.csv")
#loading traits to divide by number of ovules the seed set 
traits <- read.csv("Data/traits_scinames.csv")


brol_r <- subset(brol, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="FC")
brol_r$Seed_set <- brol_r$Seed.production/traits[1,8]*100
brra_r <- subset(brra, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="Flower Control")
brra_r$Seed_set <- brra_r$Seed.production/traits[2,8]*100
ersa_r <- subset(ersa, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="Flower Control")
ersa_r$Seed_set <- ersa_r$Seed.production/traits[4,8]*100
sial_r <- subset(sial, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="Flower Control")
sial_r$Seed_set <- sial_r$Seed.production/traits[8,8]*100
ipaq_r <- subset(ipaq, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="Flower control")
ipaq_r$Seed_set <- ipaq_r$Seed.production/traits[5,8]*100
ippu_r <- subset(ippu, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="Flower control")
ippu_r$Seed_set <- ippu_r$Seed.production/traits[6,8]*100
caan_r <- subset(caan, Treatment=="CROSS"|Treatment=="SELF"|Treatment=="CONTROL"|Treatment=="FLOWER CONTROL")
caan_r$Seed_set <- caan_r$Seed.production/traits[3,8]*100
pein_r <- subset(pein, Treatment=="CROSS"|Treatment=="SELF"|Treatment=="CONTROL"|Treatment=="FLOWER CONTROL")
pein_r$Seed_set <- pein_r$Seed.production/traits[7,8]*100
soly_r <- subset(soly, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="Flower control")
soly_r$Seed_set <- soly_r$Seed.production/traits[9,8]*100
some_r <- subset(some, Treatment=="Cross"|Treatment=="Self"|Treatment=="Control"|Treatment=="Flower control")
some_r$Seed_set <- some_r$Seed.production/traits[10,8]*100

d <- rbind(brol_r,brra_r,ersa_r,sial_r,ipaq_r,ippu_r,caan_r,pein_r,soly_r,some_r)
d$Seed_set[d$Seed_set>100]<-100

levels(d$Treatment)
d$Treatment=as.character(d$Treatment)
d$Treatment[d$Treatment=="Cross"]<-"Hand cross-pollination"
d$Treatment[d$Treatment=="CROSS"]<-"Hand cross-pollination"
d$Treatment[d$Treatment=="Self"]<-"Hand self-pollination"
d$Treatment[d$Treatment=="SELF"]<-"Hand self-pollination"
d$Treatment[d$Treatment=="SELF"]<-"Hand self-pollination"
d$Treatment[d$Treatment=="Control"]<-"Apomixis"
d$Treatment[d$Treatment=="CONTROL"]<-"Apomixis"
d$Treatment[d$Treatment=="FC"]<-"Natural selfing"
d$Treatment[d$Treatment=="Flower control"]<-"Natural selfing"
d$Treatment[d$Treatment=="FLOWER CONTROL"]<-"Natural selfing"
d$Treatment[d$Treatment=="Flower Control"]<-"Natural selfing"

d$Species=as.character(d$Species)
d$Species[d$Family=="BROL"]<-"B"
d$Species[d$Family=="BRRA"]<-"B"
d$Species[d$Family=="ERSA"]<-"B"
d$Species[d$Family=="SIAL"]<-"B"
d$Species[d$Family=="IPPU"]<-"C"
d$Species[d$Family=="IPAQ"]<-"C"
d$Species[d$Family=="SOME"]<-"S"
d$Species[d$Family=="SOLY"]<-"S"
d$Species[d$Family=="PEIN"]<-"S"
d$Species[d$Family=="CAAN"]<-"S"

d$Species[d$Species=="BROL"]<-"B. oleracea"
d$Species[d$Species=="BRRA"]<-"B. rapa"
d$Species[d$Species=="SIAL"]<-"S. alba"
d$Species[d$Species=="ERSA"]<-"E. sativa"
d$Species[d$Species=="IPPU"]<-"I. purpurea"
d$Species[d$Species=="IPAQ"]<-"I. aquatica"
d$Species[d$Species=="SOLY"]<-"S. lycopersicum"
d$Species[d$Species=="SOME"]<-"S. melongena"
d$Species[d$Species=="PEIN"]<-"P. integrifolia"
d$Species[d$Species=="CAAN"]<-"C. annuum"


ggplot(d, aes(x=Species, y=Seed_set, fill=Treatment)) + 
  geom_violin(scale = "width",draw_quantiles = c(0.5))

brewer.pal(n = 1, name = "Dark2")


cbPalette <- c( "#56B4E9","#E69F00", "#999999", "#009E73")

ggplot(d, aes(x=Species, y=Seed_set, fill=Treatment)) + 
  geom_violin(scale = "width",draw_quantiles = c(0.5))+
  geom_dotplot(binaxis='y', stackdir='center', 
               dotsize=0.3,position = position_jitterdodge(jitter.width = 0.1))+
  theme_minimal()+scale_fill_brewer(palette="RdBu")


ggplot(d, aes(x=Species, y=Seed_set, fill=Treatment)) + 
  geom_violin(scale = "width",draw_quantiles = c(0.5))+
  geom_dotplot(binaxis='y', 
               dotsize=0.15,stackdir='center',position = position_jitterdodge(jitter.width = 0.01))+
  theme_minimal()+labs(y="Seed set")+scale_fill_manual(values=c( "#D55E00","#009E73","#0072B2", "#E69F00"))+theme(legend.title = element_blank(),plot.title = element_text(face = "bold"),axis.text.x = element_text(face = "italic"))
