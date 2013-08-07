library(data.table)
library(ggplot2)



load(file = "C:/Users/Nicolas Housset/Documents/rt_project1.RData")
# Working on identified peptides
identified <- rt_project1[(!is.na(modified_sequence))]

# Often the best solution is the easiest one. Here we go, with the unique function
setkey(identified, l_lcrunid, modified_sequence)
counts <- unique(identified)



counts[, sequence.f := factor(sequence)]
identified[, sequence.f := factor(modified_sequence)]

liste_peptides <- summary(counts[, sequence.f])

setkey(identified,sequence)

one_peptide <- identified[labels(liste_peptides)[2]]

# Keeping the most intense spectrum intensity information
setkey(one_peptide,l_lcrunid,modified_sequence,total_spectrum_intensity)
one_peptide_reduced <- one_peptide[one_peptide[,j=list(.I[which.max(total_spectrum_intensity)]), by=c("l_lcrunid","modified_sequence")][,j=V1]]


plot_test <- ggplot(one_peptide_reduced, aes(rtsec, modified_sequence))

plot_test + geom_point(alpha=1/3)

