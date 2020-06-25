
#TABLE S1 PREPARED ON 
#TABLE S2
#Code to create table 
traits_all <- read.csv("Data/Csv/traits_scinames.csv")
head(traits_all)
traits <- traits_all[,-c(1,2,4,10,14)]
traits_edit_order <- traits[,-c(2,12,13,14)]
traits_edit_order$ovary_length <- traits_all$ovary_length
traits_edit_order$ovary_width <- traits_all$ovary_width
traits_edit_order$Selfing_rate <- traits_all$Selfing_rate
traits_edit_order$si_index <- traits_all$si_index
saveRDS(traits_edit_order, "Thesis_Chapter_1/Tables/traits_supplementary.rds")
#TABLE S3
rep_biology <- readRDS("Thesis_chapter_1/Tables/rep_treatments.rds")
#loadlibrary
library(reshape2)
rep_biology_1 <- dcast(Species   ~ Treatment+., value.var = "Seed_set", fun.aggregate = mean,
                      data = rep_biology, na.rm= TRUE)

rep_biology_1$Species <- c("Brassica oleracea", "Brassica rapa", "Capsicum annuum", "Eruca sativa", "Ipomoea aquatica",
   "Ipomoea purpurea", "Petunia integrifolia", "Sinapis alba", " Solanum lycopersicum", "Solanum melongena")
rep_biology_1$Apomixis_1 <- rep_biology_1$Apomixis
rep_biology_1 <- rep_biology_1[,-2]
colnames(rep_biology_1)[5] <- "Apomixis"
saveRDS(rep_biology_1, "Thesis_Chapter_1/Tables/rep_biology_supplementary.rds")




