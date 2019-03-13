library(ape)
library(phytools)
pollen_tree=read.tree("Data/pollen_tree.nwk")
#pollen_tree=read.tree("Data/pollen_tree_no_outgroup.nwk")
#pollen_tree=read.tree("Data/tree_neighbour.nwk")
png("phylo_image.png", units="px", width=1600, height=1600, res=300)

plot(pollen_tree)
add.scale.bar(x=0, y=1, length = 0.01)

#plot(tree_10)


library(png)
a <- readPNG("images/tomato.png")
b <- readPNG("images/bra_colour.png")
c <- readPNG("images/ipomoea.png")

rasterImage(image=a, xleft=0.14,ybottom=1,
            xright=0.16,ytop=4.1)
rasterImage(image=b, xleft=0.14,ybottom=7.1,xright=0.16,ytop=10.1)

rasterImage(image=c, xleft=0.14,ybottom=4.2,xright=0.16,ytop=6.8)

dev.off()


tree_10 <- read.newick("Data/pollen_tree_no_outgroup.nwk")
tree_10=as.phylo(tree_10)
plot.phylo(tree_10)
add.scale.bar(x=0, y=9)

#For row names of traits all
readRDS("Manuscript_draft/Data/matrix_scale_effect.Rda")


#From here I start working with the traits
traits_all <- read.csv("Data/traits_all.csv", sep=",")
si_index <- readRDS("Data/si_index.RData")
traits_all$si_index <- si_index
rownames(traits_all) <- rownames(matrix_scale_effect)
traits_all <- traits_all[,-c(1,2)]
traits_all_scaled <- scale(traits_all)



#Phylo signal for stigma type
stigma_type <-as.data.frame(traits_all[,c("stigma_type")])
rownames(stigma_type) <- tree_10[[3]]
stigma_type <- as.matrix((stigma_type))[,1]
stigma_type <- phylosig(tree=tree_10,x=stigma_type,method="lambda",test=TRUE)


#Phylo signal for selfing rate
selfing <-as.data.frame(traits_all[,c("Selfing_rate")])
rownames(selfing) <- tree_10[[3]]
selfing <- as.matrix((selfing))[,1]
selfing <- phylosig(tree=tree_10,x=selfing,method="lambda",test=TRUE)
#$`lambda`[1] 0.9467709
#Checking position of results to call them in Markdown
#a <- phylosig(tree=tree_10,x=selfing,method="lambda",test=TRUE)
#a[1]
#a[4]


#Phylo signal for pollen size
pollen_size <-as.data.frame(traits_all[,c("pollen_size")])
rownames(pollen_size) <- tree_10[[3]]
pollen_size <- as.matrix((pollen_size))[,1]
pollen_size <- phylosig(tree=tree_10,x=pollen_size,method="lambda",test=TRUE)
#$`lambda`[1] 0.9971989

#Phylo signal for mean pollen per anther
mean_pollen_anther <-as.data.frame(traits_all[,c("mean_pollen_anther")])
rownames(mean_pollen_anther) <- tree_10[[3]]
mean_pollen_anther <- as.matrix((mean_pollen_anther))[,1]
mean_pollen_anther <- phylosig(tree=tree_10,x=mean_pollen_anther,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal for mean ovules
mean_ovules <-as.data.frame(traits_all[,c("mean_ovules")])
rownames(mean_ovules) <- tree_10[[3]]
mean_ovules <- as.matrix((mean_ovules))[,1]
mean_ovules <- phylosig(tree=tree_10,x=mean_ovules,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal for pollen_ovule_ratio
pollen_ovule_ratio <-as.data.frame(traits_all[,c("pollen_ovule_ratio")])
rownames(pollen_ovule_ratio) <- tree_10[[3]]
pollen_ovule_ratio <- as.matrix((pollen_ovule_ratio))[,1]
pollen_ovule_ratio <- phylosig(tree=tree_10,x=pollen_ovule_ratio,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Anthers
anthers <-as.data.frame(traits_all[,c("anthers")])
rownames(anthers) <- tree_10[[3]]
anthers <- as.matrix((anthers))[,1]
anthers <- phylosig(tree=tree_10,x=anthers,method="lambda",test=TRUE)

#Stigma area
stigma_area <-as.data.frame(traits_all[,c("stigma_area")])
rownames(stigma_area) <- tree_10[[3]]
stigma_area <- as.matrix((stigma_area))[,1]
stigma_area <- phylosig(tree=tree_10,x=stigma_area,method="lambda",test=TRUE)
#$`lambda`[1] 0.8597021

#Stigma length
stigma_length <-as.data.frame(traits_all[,c("stigma_length")])
rownames(stigma_length) <- tree_10[[3]]
stigma_length <- as.matrix((stigma_length))[,1]
stigma_length <- phylosig(tree=tree_10,x=stigma_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.6961634

#Stigma_width
stigma_width <-as.data.frame(traits_all[,c("stigma_width")])
rownames(stigma_width) <- tree_10[[3]]
stigma_width <- as.matrix((stigma_width))[,1]
stigma_width <- phylosig(tree=tree_10,x=stigma_width,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Stigma_surface
stigma_surface <-as.data.frame(traits_all[,c("stigma_surface")])
rownames(stigma_surface) <- tree_10[[3]]
stigma_surface <- as.matrix((stigma_surface))[,1]
stigma_surface <- phylosig(tree=tree_10,x=stigma_surface,method="lambda",test=TRUE)
#$`lambda`[1] 0.7292784

#Phylo signal style length
style_length <-as.data.frame(traits_all[,c("style_length")])
rownames(style_length) <- tree_10[[3]]
style_length <- as.matrix((style_length))[,1]
style_length <- phylosig(tree=tree_10,x=style_length,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal style width
style_width <-as.data.frame(traits_all[,c("style_width")])
rownames(style_width) <- tree_10[[3]]
style_width <- as.matrix((style_width))[,1]
style_width <- phylosig(tree=tree_10,x=style_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05


#Phylo signal ovary width
ovary_width <-as.data.frame(traits_all[,c("ovary_width")])
rownames(ovary_width) <- tree_10[[3]]
ovary_width <- as.matrix((ovary_width))[,1]
ovary_width <- phylosig(tree=tree_10,x=ovary_width,method="lambda",test=TRUE)
#$`lambda`[1] 6.610696e-05

#Phylo signal ovary length
ovary_length <-as.data.frame(traits_all[,c("ovary_length")])
rownames(ovary_length) <- tree_10[[3]]
ovary_length <- as.matrix((ovary_length))[,1]
ovary_length <- phylosig(tree=tree_10,x=ovary_length,method="lambda",test=TRUE)
#$`lambda`[1] 0.4881212

#si_index
si_index <-as.data.frame(traits_all[,c("si_index")])
rownames(si_index) <- tree_10[[3]]
si_index <- as.matrix((si_index))[,1]
si_index <- phylosig(tree=tree_10,x=si_index,method="lambda",test=TRUE)
#$`lambda`[1] 0.4917088

effect_size_all <- readRDS( "Data/effect_size_all.RData")
effect_size_all$species <- c("SOME","SOLY","PEIN", "CAAN",
                               "IPPU", "IPAQ", "SIAL", "ERSA",
                               "BRRA", "BROL")
effect_size_all=effect_size_all[,-c(1,3,4)]
traits_all$species <- rownames(traits_all)
traits_all <- merge(effect_size_all, traits_all, by="species" )
Cohen_d <-as.data.frame(traits_all[,c("Cohen_d")])
rownames(Cohen_d) <- tree_10[[3]]
Cohen_d <- as.matrix((Cohen_d))[,1]
Cohen_d <- phylosig(tree=tree_10,x=Cohen_d,method="lambda",test=TRUE)
