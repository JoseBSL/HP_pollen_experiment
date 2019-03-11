#In this script I'm going to perform GLM between Hp effect and the traits
library(vegan)
library(lme4)
library(nlme)
library(cowplot)
library(ggplot2)

#load data
matrix_scale_effect <- readRDS("Manuscript_draft/Data/matrix_scale_effect.Rda")
diag(matrix_scale_effect) <- NA

#(mean cross spp x1-mean HP effect spp x1)
effect <- melt(matrix_scale_effect)
effect=effect[complete.cases(effect), ]

#Now I'm using the HP effect calculated in Analysis_mantel_test
#But maybe I could use later effect sizes...

#Now I'm going to match all the traits with these data.frames
traits_all <- read.csv("Data/traits_all.csv", sep=",")
#I create a column with equal code of species in order to merge
traits_all$species <- c("BROL", "BRRA", "CAAN", "ERSA", "IPAQ", "IPPU", 
                        "PEIN", "SIAL", "SOLY", "SOME") 

#The column of selfing rate I'm going to add SI index values
#Seeds or fruits produced by self divided by cross
#REF (Becerra & Lloyd 1992)

#For this I load the data of seed set

#Load seed set data for 10 species
soly  <- read.csv("Data/species_seed_set/soly_seed_set.csv", sep=";", stringsAsFactors = F)
some  <- read.csv("Data/species_seed_set/some_seed_set.csv", sep=";", stringsAsFactors = F)
pein  <- read.csv("Data/species_seed_set/pein_seed_set.csv", sep=";", stringsAsFactors = F)
caan  <- read.csv("Data/species_seed_set/caan_seed_set.csv", sep=";", stringsAsFactors = F)
ersa  <- read.csv("Data/species_seed_set/ersa_seed_set.csv", sep=";", stringsAsFactors = F)
brra  <- read.csv("Data/species_seed_set/brra_seed_set.csv", sep=";", stringsAsFactors = F)
sial  <- read.csv("Data/species_seed_set/sial_seed_set.csv", sep=";", stringsAsFactors = F)
brol  <- read.csv("Data/species_seed_set/brol_seed_set.csv", sep=";", stringsAsFactors = F)
ippu  <- read.csv("Data/species_seed_set/ippu_seed_set.csv", sep=";", stringsAsFactors = F)
ipaq  <- read.csv("Data/species_seed_set/ipaq_seed_set.csv", sep=";", stringsAsFactors = F)

a_ippu<- mean(ippu[ippu$treatment=="self", "seed.set"])
b_ippu<- mean(ippu[ippu$treatment=="cross", "seed.set"])
z_ippu <- (a_ippu/b_ippu) #[1] -173.6842 This means that selfin produced 173% more seeds

a_ipaq<- mean(ipaq[ipaq$treatment=="self", "seed_set"])
b_ipaq<- mean(ipaq[ipaq$treatment=="cross", "seed_set"])
z_ipaq <- (a_ipaq/b_ipaq) #

a_brol<- mean(brol[brol$Treatment=="Self", "Seed.production"])
b_brol<- mean(brol[brol$Treatment=="Cross", "Seed.production"])
z_brol <- (a_brol/b_brol) 

a_brra<- mean(brra[brra$Treatment=="Self", "Seed.production"])
b_brra<- mean(brra[brra$Treatment=="Cross", "Seed.production"])
z_brra <- (a_brra/b_brra) 

a_sial<- mean(sial[sial$Treatment=="Self", "Seed.production"])
b_sial<- mean(sial[sial$Treatment=="Cross", "Seed.production"])
z_sial <- (a_sial/b_sial) 

a_ersa<- mean(ersa[ersa$Treatment=="Self", "seed.production"])
b_ersa<- mean(ersa[ersa$Treatment=="Cross", "seed.production"])
z_ersa <- (a_ersa/b_ersa) 

a_soly<- mean(soly[soly$Treatment=="SELF", "seed_set"])
b_soly<- mean(soly[soly$Treatment=="CROSS", "seed_set"])
z_soly <- (a_soly/b_soly) 

a_some<- mean(some[some$Treatment=="SELF", "seed_set"])
b_some<- mean(some[some$Treatment=="CROSS", "seed_set"])
z_some <- (a_some/b_some)

a_pein<- mean(pein[pein$Treatment=="SELF", "Seed.production"])
b_pein<- mean(pein[pein$Treatment=="CROSS", "Seed.production"])
z_pein <- (a_pein/b_pein)


a_caan<- mean(caan[caan$treatment=="SELF", "seed_set"])
b_caan<- mean(caan[caan$treatment=="CROSS", "seed_set"])
z_caan <- (a_caan/b_caan)


traits_all$si_index <- c(z_brol, z_brra, z_caan, z_ersa, z_ipaq, z_ippu, 
                         z_pein, z_sial, z_soly, z_some) 
si_index <- c(z_brol, z_brra, z_caan, z_ersa, z_ipaq, z_ippu, 
              z_pein, z_sial, z_soly, z_some) 

#saveRDS(si_index, "Data/si_index.RData")
#saveRDS(si_index, "Data/si_index_1.RData")

#colname to merge
colnames(traits_all)[2] <- "Species"


traits_all$compatibility[traits_all$Species=="BROL"] <- 0
traits_all$compatibility[traits_all$Species=="BRRA"] <- 0
traits_all$compatibility[traits_all$Species=="SIAL"] <- 100
traits_all$compatibility[traits_all$Species=="ERSA"] <- 2
traits_all$compatibility[traits_all$Focal=="CAAN"] <- 64
traits_all$compatibility[traits_all$Focal=="SOLY"] <- 48
traits_all$compatibility[traits_all$Focal=="SOME"] <- 100
traits_all$compatibility[traits_all$Focal=="PEIN"] <- 26
traits_all$compatibility[traits_all$Focal=="IPAQ"] <- 75
traits_all$compatibility[traits_all$Focal=="IPPU"] <- 100

traits_all$compatibility=as.numeric(traits_all$compatibility)

#I use the mean effect for each treatment, later maybe I come back and use the other
hp_mean_sp <- dcast(Species ~ ., value.var = "value", fun.aggregate = mean, data = effect, na.rm= TRUE)
colnames(hp_mean_sp) <- c("Species","hp_effect")

data <- merge(hp_mean_sp, traits_all, by="Species")
data <- data[,-c(3)]
str(data)
geom_line(aes(y=predict(model2), group=hp_effect))
ggplot(data, aes(x=anthers, y=hp_effect)) + 
  geom_point()+
  geom_abline(aes(intercept=`(Intercept)`, slope=anthers), as.data.frame(t(fixef(model2))))+theme_cowplot()
model2=lme(hp_effect~stigma_area, data=data, random=~1|indv)
summary(model2)

geom_line(aes(y=predict(model2), group=hp_effect))
ggplot(data, aes(x=stigma_area, y=hp_effect)) + 
  geom_point()+
  geom_abline(aes(intercept=`(Intercept)`, slope=stigma_area), as.data.frame(t(fixef(model2))))+theme_cowplot()

#Stigma_type

stigma_type=lme(hp_effect~stigma_type, data=data, random=~1|indv)
summary(stigma_type)

geom_line(aes(y=predict(stigma_type), group=hp_effect))
ggplot(data, aes(x=stigma_type, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=stigma_type), as.data.frame(t(fixef(stigma_type))))+theme_cowplot()

aov_stigma<- aov(hp_effect ~ stigma_type, data = data)
summary(aov_stigma)
#Dry
stigma_type_1 <- subset(data, stigma_type=="1")
mean(stigma_type_1$hp_effect)
#Wet
stigma_type_0 <- subset(data, stigma_type=="0")
mean(stigma_type_0$hp_effect)

#Selfing_rate

Selfing_rate=lme(hp_effect~Selfing_rate, data=data, random=~1|indv)
summary(Selfing_rate)

geom_line(aes(y=predict(Selfing_rate), group=hp_effect))
ggplot(data, aes(x=Selfing_rate, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=Selfing_rate), as.data.frame(t(fixef(Selfing_rate))))+theme_cowplot()


#Pollen_size

pollen_size=lme(hp_effect~pollen_size, data=data, random=~1|indv)
summary(pollen_size)

geom_line(aes(y=predict(pollen_size), group=hp_effect))
ggplot(data, aes(x=pollen_size, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=pollen_size), as.data.frame(t(fixef(pollen_size))))+theme_cowplot()


#Mean_pollen_anther

mean_pollen_anther=lme(hp_effect~mean_pollen_anther, data=data, random=~1|indv)
summary(mean_pollen_anther)

geom_line(aes(y=predict(mean_pollen_anther), group=hp_effect))
ggplot(data, aes(x=mean_pollen_anther, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=mean_pollen_anther), as.data.frame(t(fixef(mean_pollen_anther))))+theme_cowplot()


str(data)

#Mean_ovules

mean_ovules=lme(hp_effect~mean_ovules, data=data, random=~1|indv)
summary(mean_ovules)

geom_line(aes(y=predict(mean_ovules), group=hp_effect))
ggplot(data, aes(x=mean_ovules, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=mean_ovules), as.data.frame(t(fixef(mean_ovules))))+theme_cowplot()

#pollen_ovule_ratio

pollen_ovule_ratio=lme(hp_effect~pollen_ovule_ratio, data=data, random=~1|indv)
summary(pollen_ovule_ratio)

geom_line(aes(y=predict(pollen_ovule_ratio), group=hp_effect))
ggplot(data, aes(x=pollen_ovule_ratio, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=pollen_ovule_ratio), as.data.frame(t(fixef(pollen_ovule_ratio))))+theme_cowplot()




#stigma_area

stigma_area=lme(hp_effect~stigma_area, data=data, random=~1|indv)
summary(stigma_area)

geom_line(aes(y=predict(stigma_area), group=hp_effect))
ggplot(data, aes(x=stigma_area, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=stigma_area), as.data.frame(t(fixef(stigma_area))))+theme_cowplot()



#stigma_length

stigma_length=lme(hp_effect~stigma_length, data=data, random=~1|indv)
summary(stigma_length)
str(data)

geom_line(aes(y=predict(stigma_length), group=hp_effect))
ggplot(data, aes(x=stigma_length, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=stigma_length), as.data.frame(t(fixef(stigma_length))))+theme_cowplot()




#stigma_width

stigma_width=lme(hp_effect~stigma_width, data=data, random=~1|indv)
summary(stigma_width)
geom_line(aes(y=predict(stigma_width), group=hp_effect))
ggplot(data, aes(x=stigma_width, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=stigma_width), as.data.frame(t(fixef(stigma_width))))+theme_cowplot()


#stigma_surface

stigma_surface=lme(hp_effect~stigma_surface, data=data, random=~1|indv)
summary(stigma_surface)

geom_line(aes(y=predict(stigma_surface), group=hp_effect))
ggplot(data, aes(x=stigma_surface, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=stigma_surface), as.data.frame(t(fixef(stigma_surface))))+theme_cowplot()


#style_length

style_length=lme(hp_effect~style_length, data=data, random=~1|indv)
summary(style_length)

geom_line(aes(y=predict(style_length), group=hp_effect))
ggplot(data, aes(x=style_length, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=style_length), as.data.frame(t(fixef(style_length))))+theme_cowplot()

#style_width

style_width=lme(hp_effect~style_width, data=data, random=~1|indv)
summary(style_width)

geom_line(aes(y=predict(style_width), group=hp_effect))
ggplot(data, aes(x=style_width, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=style_width), as.data.frame(t(fixef(style_width))))+theme_cowplot()


#ovary_width

ovary_width=lme(hp_effect~ovary_width, data=data, random=~1|indv)
summary(ovary_width)

geom_line(aes(y=predict(ovary_width), group=hp_effect))
ggplot(data, aes(x=ovary_width, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=ovary_width), as.data.frame(t(fixef(ovary_width))))+theme_cowplot()


#ovary_length

ovary_length=lme(hp_effect~ovary_length, data=data, random=~1|indv)
summary(ovary_length)

geom_line(aes(y=predict(ovary_length), group=hp_effect))
ggplot(data, aes(x=ovary_length, y=hp_effect)) + 
  geom_point()+geom_abline(aes(intercept=`(Intercept)`, slope=ovary_length), as.data.frame(t(fixef(ovary_length))))+theme_cowplot()


#si_index

si_index=lme(hp_effect~si_index, data=data, random=~1|indv)
summary(si_index)

geom_line(aes(y=predict(si_index), group=hp_effect))
ggplot(data, aes(x=si_index, y=hp_effect)) + 
  geom_point()+
  geom_abline(aes(intercept=`(Intercept)`, slope=si_index), as.data.frame(t(fixef(si_index))))+theme_cowplot()

#Now same things but instead with the Hp effect with effect sizes

#m_effect <- readRDS("Data/matrix_effect_size.RData")
#m_effect <- melt(m_effect)
effect_size <- readRDS("Data/effect_size.RDS")
colnames(effect_size)[1]<- "Non_focal"
colnames(effect_size)[5]<- "Species"

#colnames(m_effect) <- c("Species", "Non_focal", "hp_effect")
#data_effect_size <- merge(m_effect, traits_all, by="Species")
data_effect_size <- merge(effect_size, traits_all, by="Species")
#Check different effect through anova with stigma type

aov_effect_size_stigma <- aov(Cohen_d~stigma_type, data=data_effect_size)
summary(aov_effect_size_stigma)
stigma_type_1 <- subset(data_effect_size, stigma_type=="1")
stigma_type_0 <- subset(data_effect_size, stigma_type=="0")
mean(stigma_type_1$Cohen_d, na.rm=T)
mean(stigma_type_0$Cohen_d, na.rm=T)

maybe <- dcast(Species+stigma_type ~ ., value.var = "Cohen_d", fun.aggregate = mean, data = data_effect_size, na.rm= TRUE)
colnames(maybe)[3] <- "cohen_d"
aov_effect_size_stigma <- aov(cohen_d~stigma_type, data=maybe)
summary(aov_effect_size_stigma)

