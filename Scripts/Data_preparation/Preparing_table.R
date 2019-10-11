#here I prepare the table of lm for publication
#read data

soly <- readRDS("Data/soly.RData")
caan <- readRDS("Data/caan.RData")
some <- readRDS("Data/some.RData")
brol <- readRDS("Data/brol.RData")
brra <- readRDS("Data/brra.RData")
ersa <- readRDS("Data/ersa.RData")
sial <- readRDS("Data/sial.RData")
ippu <- readRDS("Data/ippu.RData")
ipaq <- readRDS("Data/ipaq.RData")

#Convert everything to data.frame
soly <- as.data.frame(soly)
str(soly)
soly <- as.data.frame(soly)
write.csv(soly, "Data/soly.csv")


some <- as.data.frame(some)
str(some)
some <- as.data.frame(some)
write.csv(some, "Data/some.csv")

caan <- as.data.frame(caan)
str(caan)
caan <- as.data.frame(caan)
write.csv(caan, "Data/caan.csv")

pein <- as.data.frame(pein)
str(pein)
pein <- as.data.frame(pein)
write.csv(pein, "Data/pein.csv")

brra <- as.data.frame(brra)
str(brra)
brra <- as.data.frame(brra)
write.csv(brra, "Data/brra.csv")


brol <- as.data.frame(brol)
str(brol)
brol <- as.data.frame(brol)
write.csv(brol, "Data/brol.csv")

ersa <- as.data.frame(ersa)
str(ersa)
ersa <- as.data.frame(ersa)
write.csv(ersa, "Data/ersa.csv")

sial <- as.data.frame(sial)
str(sial)
sial <- as.data.frame(sial)
write.csv(sial, "Data/sial.csv")

ipaq <- as.data.frame(ipaq)
str(ipaq)
ipaq <- as.data.frame(ipaq)
write.csv(ipaq, "Data/ipaq.csv")

ippu <- as.data.frame(ippu)
str(ippu)
write.csv(ippu, "Data/ippu.csv")


ippu <- as.data.frame(ippu)

rownames(soly) <-("S.lycopersicum (intercept)")
