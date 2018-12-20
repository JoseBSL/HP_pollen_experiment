#Assymetric matrices 
#Folowing the examples of the package asimmetry

#Heatmap doesn't seem very informative, coordinates plots yes,
#to ilustrate differences in effect among species

#Library
library(asymmetry)

#load matrix of effects Hp 

matrix <- as.matrix(read.csv("Data/matrix_scale_effect.csv", header = T, row.names = 1))

min(matrix)

matrix_positive<- matrix+abs(min(matrix))


t=asymscal(matrix_positive, ndim = 2, start = NULL, itmax = 10000, eps = 1e-10)

t$cweights
round(t$cweights, 3)

#This plot is cool
plot(t, plot.type = "confplot")
plot(t, plot.type = "bubbleplot")
plot(t, plot.type = "stressplot")

#heat map examples
hmap(matrix_positive, dominance = F)
data("Englishtowns")
hmap(Englishtowns, dominance = F)

#
data(studentmigration)
hmap(studentmigration, dominance = TRUE, col = c("red", "white", "blue"))
hmap(matrix_positive, dominance = TRUE, col = c("red", "white", "blue"))

#Other example
## Not run:
data("studentmigration")
mm<-matrix_positive
mm[mm==0]<-.5 # replace zeroes by a small number
mm <- -log(mm/sum(mm)) # convert similarities to dissimilarities
v<-mdsunique(mm, ndim = 2, itmax = 2100, verbose=FALSE, eps = .0000000001)
plot(v, yplus = 0)

#Other example

data("studentmigration")
mm<-matrix_positive
mm[mm==0]<- 0.5
mm <- -log(mm/sum(mm)) # convert similarities to dissimilarities
v<-mdsunique(mm,ndim = 2,itmax = 2100,verbose=TRUE, eps = .0000000001)
plot(v, yplus = 0)


#example
test <- as.matrix(dist(matrix_positive)) #extract data
v <- slidevector(test, ndim = 2, itmax = 250, eps = .001)
plot(v)

#example
data("Englishtowns")
Q <- skewsymmetry(matrix_positive)
# the skew-symmetric part
Q$A


#example
v <- slidevector(matrix_positive, weight = NULL, ndim = 2, verbose = FALSE, itmax = 125, eps = 1e-12)
plot(v)


#example
q <- skewsymmetry(matrix_positive)
summary(q)

#example
v <- slidevector(matrix_positive, ndim = 2, itmax = 250, eps = .001)
summary(v)

##
####
#####
#Package generalCorr examples
#####
####
##

library(generalCorr)

options(np.messages = FALSE)
set.seed(34);x=sample(1:10);y=sample(2:11)
bb=bootPairs0(cbind(x,y),n999=29)
apply(bb,2,summary) #gives summary stats for n999 bootstrap sum computations
bb=bootPairs0(airquality,n999=999);options(np.messages=FALSE)
apply(bb,2,summary) #gives summary stats for n999 bootstrap sum computations



library(MASS)
library(mva)
data(varespec)
vare.dist <- vegdist(wisconsin(varespec))
mds.null <- isoMDS(vare.dist)
## This was a good seed for me: your rng may vary.
## Reset your rng if you run this example.
set.seed(237)
mds.alt <- isoMDS(vare.dist, initMDS(vare.dist))
vare.proc <- procrustes(mds.alt$points, mds.null$points)
vare.proc
summary(vare.proc)
plot(vare.proc)
plot(vare.proc, kind=2)
residuals(vare.proc)
procrustes.results$RSS


isSymmetric(matrix_scale_effect)
isSymmetric(as.matrix(evo_distance_its_square_root))


#Other example
library(geomorph)
library(rgl)
data(plethodon) 
Y.gpa <- gpagen(plethodon$land,PrinAxes=FALSE)
summary(Y.gpa)
plot(Y.gpa)


