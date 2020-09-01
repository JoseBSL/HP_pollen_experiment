#Plot phylogenetic tree

#Load library
library(ape)
library(phytools)

#Load netwick file
#see main text for the methods of how the tree was calculated
pollen_tree=read.tree("Data/Nwk/phylo_all_correct_names.nwk")
#save png for manuscript
#figure 1
png("Figure1.png", units="px", width=12000, height=12000, res=2500)
plot(pollen_tree, font= 3)
add.scale.bar(x=0, y=1, length = 0.01)
dev.off()

