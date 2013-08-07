# Parameter that can easily be changed : projects selected.

library(RMySQL);
con <- dbConnect(MySQL(), group="MSDB", dbname="projects");

# Selecting relevant information from the database
# Selection criteria are project numbers (to avoid huge database) and orbitrap instruments (id between 8 and 10)

rt_project <- dbGetQuery(con,
                         "SELECT scanid, number, spectrumid, l_lcrunid, l_projectid, l_instrumentid, identified, score, identitythreshold, confidence, DB,
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

with(rt_project, identified <<- rt_project[(!is.na(modified_sequence.f)),])

# Merging in a new dataset all the information related to the most common proteins (maybe change that part later)
# 10/07/2013 : after making a data.table version of this code, I realize how much this code is silly
one_peptide <- identified[modified_sequence.f==labels(liste_peptide_mod)[1],]
for(i in 2:1000){
  second_data <- identified[modified_sequence.f==labels(liste_peptide_mod)[i],]
  # one_peptide <<- rbind(one_peptide,second_data)
}

# The package plyr contains many useful functions for working with split datasets
library(plyr)


save(identified, file = "C:/Users/Nicolas Housset/Documents/identified.RData", compression_level = 1)
save(rt_project, file = "C:/Users/Nicolas Housset/Documents/rt_project.RData", compression_level = 1)
rm(rt_project)
rm(identified)
gc()

load("C:/Users/Nicolas Housset/Documents/identified.RData")

# So apparently data.table is way faster than data.frame
# But the grammar is different
one_peptide_T <- data.table(one_peptide)
one_peptide_T$spectrumid <- as.character(one_peptide_T$spectrumid)

one_peptide_T$number <- as.character(one_peptide_T$number)

setkeyv(one_peptide_T, c("spectrumid","number"))
setkey(one_peptide_T,spectrumid)

summary(one_peptide_T)

# setkey(one_peptide_T, spectrumid)

# This one works and is pretty fast, but it makes only half the job.
timethis(one_peptide_T[one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid][V1<=5]])
# We can consider that the part before [V1<=5] is a datatable keyed by spectrumid, so all we need to do is adding a j statement
# in the second part. There might be a more efficient way.
one_peptide_T[one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid][V1<=5],j=.SD[1]]
# This works 
one_peptide_T_reduced <- one_peptide_T[one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid][V1<=5]][j=.SD[1], by=spectrumid]


# If I use a by to filter duplicates, it states a warning, because the data table is already keyed
# But how to be sure that the first scan is taken ? Is it enough to just put number as a key ?
# Answer : yes it is. In more complicated cases, I am not sure yet what to do. Experience will tell.
one_peptide_T_reduced <- one_peptide_T[one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid][V1<=5],j=.SD[1]]

one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid]

# works but is it really what I want ?
one_peptide_T[j=max(number)-min(number), by=spectrumid][,spectrumid]



testFunction <- function(x){
  
}


# The purpose is to remove grouped spectra where the distance between individuals is too high (here, max of 5)
one_peptide$diff <- NA
one_peptide_reduced <- ddply(one_peptide,.(spectrumid),function(x){x$diff[1] <- max(x$number)-min(x$number)
                                                                   x[as.numeric(x$diff[1]<=5),]})
if(!is.factor(one_peptide_reduced$diff)) one_peptide_reduced$diff <- factor(one_peptide_reduced$diff)
gc()


# One peptide can be identified many times in the same lcrun : we keep the information of the most intense ion
one_peptide_reduced_2 <- ddply(one_peptide_reduced,.(l_lcrunid,modified_sequence),function(x){local_max <- max(x$total_spectrum_intensity)
                                                                                              x[x$total_spectrum_intensity==local_max,]})