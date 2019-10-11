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


#recipient species
c_pollen_focal <- subset(d_pollen, family_focal=="C")
s_b_pollen_focal <- subset(d_pollen, family_focal=="B"|family_focal=="S")
s_pollen_focal <- subset(d_pollen, family_focal=="S")
b_pollen_focal <- subset(d_pollen, family_focal=="B")
hist(c_pollen_focal$ratio)
hist(s_b_pollen_focal$ratio)

t.test(c_pollen_focal$ratio,s_b_pollen_focal$ratio,alternative = "two.sided", var.equal = FALSE)
#Showing differences between big pollen size and small pollen size families 
#Convolvulacea compared with solanaceae and brassicaceae
t.test(s_pollen_focal$ratio,b_pollen_focal$ratio,alternative = "two.sided", var.equal = FALSE)
t.test(c_pollen_focal$ratio,b_pollen_focal$ratio,alternative = "two.sided", var.equal = FALSE)
t.test(c_pollen_focal$ratio,s_pollen_focal$ratio,alternative = "two.sided", var.equal = FALSE)

t.test(s_b_pollen_focal$ratio,c_pollen_focal$ratio,alternative = "two.sided", var.equal = FALSE)

library(mosaic)
#Here I'm trying to show how our pollen samples differ statistically from a 50-50 % porportion
#one sample porportion test seems to be the way

total_pollen <- read.csv("Data/total_pollen.csv")

c_other_pollen <- mean(total_pollen[1:8,4])
c_pollen <- mean(total_pollen[21:28,4])

s_pollen_other <- mean(total_pollen[9:14,4])
s_pollen <- mean(total_pollen[29:34,4])

b_pollen_other <- mean(total_pollen[15:20,4])
b_pollen <- mean(total_pollen[35:40,4])

res <- prop.test(x = 110, n = 1106, p = 0.5, 
                 correct = FALSE)


 prop.test(x = s_pollen, n = (s_pollen+s_pollen_other), p = 0.5, 
                 correct = FALSE)
     
 prop.test(x = b_pollen_other, n = (b_pollen+b_pollen_other), p = 0.5, 
           correct = FALSE,alternative = "less")
 
             
 #alternative = "less"