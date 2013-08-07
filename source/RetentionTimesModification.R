# Parameter that can easily be changed : projects selected.

library(RMySQL);
con <- dbConnect(MySQL(), group="MSDB", dbname="projects");

# Selecting relevant information from the database
# Selection criteria are project numbers (to avoid huge database) and orbitrap instruments (id between 8 and 10)

rt_project <- dbGetQuery(con,
                            "SELECT scanid, number, spectrumid, l_lcrunid, l_projectid, l_instrumentid, identified, score, identitythreshold, confidence, DB
rtsec, total_spectrum_intensity, mass_to_charge, spectrum.charge, accession, start, end, sequence, modified_sequence FROM
(spectrum LEFT JOIN scan ON spectrum.spectrumid = scan.l_spectrumid 
LEFT JOIN identification ON spectrum.spectrumid = identification.l_spectrumid
LEFT JOIN spectrum_file ON spectrum.spectrumid = spectrum_file.l_spectrumid)
WHERE l_projectid BETWEEN 651 AND 750 AND l_instrumentid BETWEEN 8 AND 10;");


# First, making factors variables that need to be coded as factors

if(!is.factor(rt_project$l_projectid)) rt_project$l_projectid.f <- factor(rt_project$l_projectid)
if(!is.factor(rt_project$sequence)) rt_project$sequence.f <- factor(rt_project$sequence)
if(!is.factor(rt_project$accession)) rt_project$accession.f <- factor(rt_project$accession)
if(!is.factor(rt_project$modified_sequence)) rt_project$modified_sequence.f <- factor(rt_project$modified_sequence)

# This will conveniently return a list of the peptides encountered, starting with the most frequent ones
liste_peptide <- summary(rt_project$sequence.f)
liste_peptide_mod <- summary(rt_project$modified_sequence.f)

liste_peptide


# with(rt_project, numBox <<- (max(rtsec)-min(rtsec)) %/% 120 + 1)
# Hist(rt_project$rtsec, scale="frequency", breaks=numBox, col="darkgray")


with(rt_project, identified <<- rt_project[(!is.na(modified_sequence.f)),])


# Preallocating memory to increase efficiency
# length(liste_data) <- 50
# for(i in 1:50) liste_data[i] <- identified[0,]

one_peptide <- identified[identified$sequence.f==labels(liste_peptide)[1],]
for(i in 2:20){
  second_data <- identified[(identified$sequence.f==labels(liste_peptide)[i]),]
  one_peptide <<- rbind(one_peptide,second_data)
}

# First version of subsetting, without loop. Not convenient if selecting dozens of peptides
with(rt_project,one_peptide <<- rt_project[(!is.na(modified_sequence)) & (modified_sequence.f==labels(liste_peptide)[1]
                                                                 |modified_sequence.f==labels(liste_peptide)[2]
                                                                 |modified_sequence.f==labels(liste_peptide)[3]
                                                                 |modified_sequence.f==labels(liste_peptide)[4]
                                                                 |modified_sequence.f==labels(liste_peptide)[5]),])

one_peptide <- droplevels(one_peptide)

liste_modification <- summary(one_peptide$modified_sequence.f)

# The package plyr contains many useful functions for working with split datasets
library(plyr)

# The purpose is to remove grouped spectra where the distance between individuals is too high (here, max of 5)
one_peptide$diff <- NA
one_peptide_reduced <- ddply(one_peptide,.(spectrumid),function(x){x$diff[1] <- max(x$number)-min(x$number)
                                                                   x[as.numeric(x$diff[1]<=5),]})
if(!is.factor(one_peptide_reduced$diff)) one_peptide_reduced$diff <- factor(one_peptide_reduced$diff)


# One peptide can be identified many times in the same lcrun : we keep the information of the most intense ion
one_peptide_reduced_2 <- ddply(one_peptide_reduced,.(l_lcrunid,modified_sequence),function(x){local_max <- max(x$total_spectrum_intensity)
                                                                                    x[x$total_spectrum_intensity==local_max,]})

fix(one_peptide_reduced_2)



list_summaries <- dlply(one_peptide_reduced_2,.(modified_sequence.f),function(x){numSummary(x$rtsec, statistics=c("mean","sd"))})
list_summaries <- dlply(one_peptide_reduced_2,.(accession),function(x){numSummary(x$rtsec, statistics=c("mean","sd"))})

list_summaries

boxplot(rtsec ~ modified_sequence.f, data = one_peptide_reduced_2,
        notch=TRUE,
        varwidth=TRUE,
        col="red",
        main="Useless Boxplot",
        xlab="Sequence",
        ylab="Retention time");

library(sm);
sm.density.compare(one_peptide$rtsec, one_peptide$sequence, xlab="Retention time");
title(main="Retention time by modified sequence");

attach(one_peptide)
modified_sequence.f <- factor(modified_sequence)
liste_modification <- summary(modified_sequence.f)
liste_modification
vioplot(rtsec[modified_sequence==labels(liste_modification)[1]],
        rtsec[modified_sequence==labels(liste_modification)[2]],
        rtsec[modified_sequence==labels(liste_modification)[3]],
        rtsec[modified_sequence==labels(liste_modification)[4]],
        rtsec[modified_sequence==labels(liste_modification)[5]],
        names=c(labels(liste_modification[1:5])),
        col="gold")
detach(one_peptide)

liste_modification

