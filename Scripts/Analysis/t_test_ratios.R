#'m going to compare the pollen proportions among families
#Read data
d <- read.csv("Data/pollen_ratios.csv")

d_pollen <- subset(d, variable=="non_focal_pollen")
#length(d_pollen$X)
#confirming length of the data.frame
#add_manually column with family
d_pollen$family_focal <- c("S","B","B","B","S", "S", "S", "B", "C", "B", "B", "B", "C", "B", "C",
                           "S", "S", "C","S","S")

d_pollen$family_non_focal <- c("C","C","C","C","C","C","C","C","S","S","S","S","S","S","B","B","B",
                               "B","B","B")
#select column of interest
d_pollen <- d_pollen[,c(4,7,8)]

c_pollen <- subset(d_pollen, family_non_focal=="C")
s_b_pollen <- subset(d_pollen, family_non_focal=="B"|family_non_focal=="S")
s_pollen <- subset(d_pollen, family_non_focal=="S")
b_pollen <- subset(d_pollen, family_non_focal=="B")
hist(c_pollen$ratio)
hist(s_b_pollen$ratio)

t.test(c_pollen$ratio,s_b_pollen$ratio,alternative = "two.sided", var.equal = FALSE)
#Showing differences between big pollen size and small pollen size families 
#Convolvulacea compared with solanaceae and brassicaceae
t.test(s_pollen$ratio,b_pollen$ratio,alternative = "two.sided", var.equal = FALSE)
t.test(c_pollen$ratio,b_pollen$ratio,alternative = "two.sided", var.equal = FALSE)
t.test(c_pollen$ratio,s_pollen$ratio,alternative = "two.sided", var.equal = FALSE)

t.test(s_b_pollen$ratio,c_pollen$ratio,alternative = "two.sided", var.equal = FALSE)
