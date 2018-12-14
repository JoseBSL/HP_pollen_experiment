#Theoretical prediction of heterospecific pollen effect

#Create empty plot

plot(NULL, xlim=c(0,1), ylim=c(0,1), ylab="Heterospecific pollen effect", xlab="Phylogenetic Relatedness", main="Theoretical effect") 
abline(h=0.9)
text(0.1,0.95, "1")
abline(v=0.28)
text(0.1,0.2, "Hybridization
     low effect",srt = 90)

text(0.35,0.95, "2")

text(0.5,0.5, "Trait-dependent effect
     highly variable")
text(0.5,0.8, "SI")
text(0.5,0.2, "SC")


