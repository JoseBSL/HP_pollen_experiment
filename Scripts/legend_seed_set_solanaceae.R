
library(ggplot2)
library(grid)
library(gridExtra)


bra <- rep("Brassicaceae",10)
con<- rep("Convolvulaceae",10)
sol <- rep("Solanaceae",10)
foc <- rep("Focal_treatments",10)

b <- seq(1:10)
dat_0<- data.frame(bra,b)
dat_1<- data.frame(con,b)
dat_2<- data.frame(sol,b)
dat_3<- data.frame(foc,b)

colnames(dat_0) <- c("tre","b")
colnames(dat_1) <- c("tre","b")
colnames(dat_2) <- c("tre","b")
colnames(dat_3) <- c("tre","b")

for_legend <- rbind(dat_0,dat_1,dat_2,dat_3)
cbPalette <- c( "#56B4E9","#E69F00","#009E73", "#999999")


for_leg <- ggplot(for_legend, aes(x = factor(tre, levels=unique(tre)), y = b)) +  geom_boxplot(outlier.shape = NA)+
  scale_fill_manual(values=cbPalette)+aes(fill=tre)+ 
  theme(legend.position = "bottom",legend.title=element_blank(),legend.key.size = unit(2, 'lines'),legend.key=element_blank()) 

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 
legend <- g_legend(for_leg) 
grid.newpage()
grid.draw(legend) 


save.image(file = "Rmd/legend.rda")
