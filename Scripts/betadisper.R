
#Load libraries
library(vegan)
library(qpcR)
library(dplyr)
#This PCA doesn't like missing values, fix them.
load("Data/Morpho_RData/traits.RData")
#Ovary width and O. length are find

#To check for the number of NA
#I start with stigma surface
sum(is.na(stigma_surface$um))
#[1] 2
#Fix them!!
stigma_surface$seq <- seq.int(nrow(stigma_surface))
stigma_surface[15,3] = stigma_surface[14,3]
stigma_surface[82,3] = stigma_surface[81,3]
stigma_surface[is.na(stigma_surface$um)==TRUE,]

#Check now for stigma length
stigma_length[is.na(stigma_length$um)==TRUE,]
sum(is.na(stigma_length$um))
stigma_length$seq <- seq.int(nrow(stigma_length))
stigma_length[10,3] = stigma_length[9,3]


#Now the style traits
sum(is.na(style_length))
sum(str_count(style_length$species, "SOLY"))

head(style_length)
#It has 3 rows less fix them
style_length$seq <- seq.int(nrow(style_length))
soly_mean=mean(style_length[style_length$species=="SOLY","um"])
style_length <- rbind(style_length,data.frame(species = "SOLY", measurement = "style _ length",
        um=soly_mean, seq= 14.1))

#Check for other missing values, fix IPPU
length(style_length[style_length=="IPPU","species"])
#Adding IPPU missing value with mean of IPPU Style_length
ippu_mean=mean(style_length[style_length$species=="IPPU","um"])
style_length <- rbind(style_length,data.frame(species = "IPPU", measurement = "style _ length",
                                              um=ippu_mean, seq= 15.1))
#Fix last missing value
length(style_length[style_length=="BROL","species"])
#Fix BROL missing value
brol_mean=mean(style_length[style_length$species=="BROL","um"])
style_length <- rbind(style_length,data.frame(species = "BROL", measurement = "style _ length",
                                              um=brol_mean, seq= 30.1))
style_length <- style_length[order(style_length$seq),]
#Now it is ordered! And read it to include it


#With style width, last morphological trait
length(style_width[style_width=="SOLY","species"])
length(style_width[style_width=="PEIN","species"])
length(style_width[style_width=="CAAN","species"])
length(style_width[style_width=="SOME","species"])












traits_all = cbind(ovary_length,ovary_width, stigma_surface, stigma_length, style_length)
traits=traits_all
head(traits)
traits=traits[,c(3,6,9,13,17)]
head(traits)
#traits=dist(traits)
str(traits)


#Try different index of distances

traits=vegdist(traits)

traits <- betadisper(traits, traits_all$species)
traits


labs <- paste("Dimension", 1:18, "(", 
              round(100*traits$eig / sum(traits$eig), 2), "%)")

#step3
anova(traits)


#Step4
permutest(traits)


plot(traits, cex=1,pch=10:21,
     main="Gynoecium:MDS coordinates", cex.lab=1.25,label.cex=0.5,
     xlab=labs[1], ylab=labs[2], ylim=c(-0.35,0.35),xlim=c(-0.35,0.35),
     hull=FALSE, ellipse=TRUE, conf=0.68, lwd=2)


myplotbetadisper(traits, cex=1,pch=10:21,
                 main="Ovary length and width: MDS coordinates",coltextrect=col.text.rect,
                 cex.lab=1.25,label.cex=0.4,fillrect=col.fill.rect,
                 xlab=labs[1], ylab=labs[2], ylim=c(-0.35,0.35),xlim=c(-0.35,0.35),
                 hull=FALSE, ellipse=TRUE, conf=0.68, lwd=2)

#Other way of plotting this
#Following this example https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
library(ggfortify)
library(ggplot2)
library(stats)
all_pca=traits_all[,c(3,6,9,13,17)]
df <- iris[c(1, 2, 3, 4)]
autoplot(prcomp(all_pca))
autoplot(prcomp(all_pca), data = traits_all, colour = 'species')
#It works, but I think I have some serious outliers. Fix them




#eXAMPLE
library(vegan)
# A dummy data generation process
set.seed(1)
n <- 100
df <- matrix(runif(13*n),nrow=13)

# Compute dissimilarity indices
dis <- vegdist(df)
groups <- factor(c(rep(1,8), rep(2,5)), labels = c("ABC","DEF")) 

# Analysis of multivariate homogeneity of group dispersions
mod <- betadisper(dis, groups)

source("Scripts/myplotbetadisp.r")
labPts <- LETTERS[1:13]
col.fill.rect <- addAlpha(col2rgb("gray65"), alpha=0.5)
col.text.rect <- apply(col2rgb(c("blue","darkgreen")), 2, addAlpha, alpha=0.5)
transp.centroids <- 0.7

myplotbetadisper(mod, ellipse = TRUE, hull = FALSE, 
                 fillrect=col.fill.rect, coltextrect=col.text.rect, 
                 alphaPoints=transp.centroids, labPoints=labPts,
                 main= "MultiVariate Permutation")
