

focal_pollen_percentage <- as.matrix(read.table("Data/pollen_ratios_matrix.csv", header=TRUE, sep = ";",
                              row.names = 1,
                              as.is=TRUE))
focal_pollen_percentage <- melt(focal_pollen_percentage)


non_focal_pollen_percentage <- as.matrix(read.table("Data/pollen_ratios_matrix_non_focal.csv", header=TRUE, sep = ";",
                                                row.names = 1,
                                                as.is=TRUE))

non_focal_pollen_percentage <- melt(non_focal_pollen_percentage)

pollen_ratios <- merge(non_focal_pollen_percentage, non_focal_pollen_percentage, by=c("Var1","Var2"))

colnames(pollen_ratios) <- c("focal","non_focal", "focal_percen", "non_focal_percen")



