# Realizing that some operations were going to be awful with dataframe, here is the syntax with datatable

library(data.table)

library(RMySQL);
con <- dbConnect(MySQL(), group="MSDB", dbname="projects");

# Selecting relevant information from the database
# Selection criteria are project numbers (to avoid huge database) and orbitrap instruments (id between 8 and 10)

rt_project <- data.table(dbGetQuery(con,
                         "SELECT scanid, number, spectrumid, l_lcrunid, l_projectid, l_instrumentid, identified, score, identitythreshold, confidence, DB,
rtsec, total_spectrum_intensity, mass_to_charge, spectrum.charge, accession, start, end, sequence, modified_sequence FROM
(spectrum LEFT JOIN scan ON spectrum.spectrumid = scan.l_spectrumid 
LEFT JOIN identification ON spectrum.spectrumid = identification.l_spectrumid
LEFT JOIN spectrum_file ON spectrum.spectrumid = spectrum_file.l_spectrumid)
WHERE l_projectid BETWEEN 651 AND 750 AND l_instrumentid BETWEEN 8 AND 10;"));

# Forget about this code, OK ? :)
# if(!is.factor(rt_project$l_projectid)) rt_project$l_projectid.f <- factor(rt_project$l_projectid)
# if(!is.factor(rt_project$sequence)) rt_project$sequence.f <- factor(rt_project$sequence)
# if(!is.factor(rt_project$accession)) rt_project$accession.f <- factor(rt_project$accession)
# if(!is.factor(rt_project$modified_sequence)) rt_project$modified_sequence.f <- factor(rt_project$modified_sequence)

invisible(rt_project[, l_projectid.f2 := factor(l_projectid)])

load("C:/Users/Nicolas Housset/Documents/rt_project.RData")

testFunction <- function(...){
  print(class(rt_project))
}

updateFunction <- function(...){
  invisible(rt_project[, l_projectid.f := factor(l_projectid)]);
  invisible(rt_project[, sequence.f := factor(sequence)]);
  invisible(rt_project[, accession.f := factor(accession)]);
  invisible(rt_project[, modified_sequence.f := factor(modified_sequence)]);
  gc();
  return(NULL);
}

badUpdateFunction <- function(...){
  rt_project$l_projectid.f <- factor(rt_project$l_projectid)
  rt_project$sequence.f <- factor(rt_project$sequence)
  rt_project$accession.f <- factor(rt_project$accession)
  rt_project$modified_sequence.f <- factor(rt_project$modified_sequence)
  gc();
  return(NULL);
}

# The factor calculation needs some memory but is freed afterwards
system.time(updateFunction());
# This is not only about the speed, but also the memory consumption
# system.time(badUpdateFunction());

# By the way, maybe we don't even need factors anymore (there is the case of non-alphabetically ordered factors)

liste_peptide <- summary(rt_project[,sequence.f])

liste_peptide_mod <- summary(rt_project[,modified_sequence.f],maxsum=1000)

identified <- rt_project[(!is.na(modified_sequence.f))]

# It takes a little bit of time to sort the data table first
system.time(setkey(identified,modified_sequence))
# Then it extracts at a blazing speed. I can take virtually as much elements as I desire (log complexity)
system.time(one_peptide <- identified[labels(liste_peptide_mod)[1:50]])

setkey(one_peptide,spectrumid,number)
system.time(
  one_peptide_reduced <- unique(
    setkey(
      one_peptide[
        setkeyv(
          one_peptide[
            j=list(
              max(as.numeric(number))-min(as.numeric(number)),
              number),
            by=spectrumid]
          [i=V1<=5],
          c("spectrumid","number")
        )
        ]
      ,spectrumid)
  )
)

one_peptide_reduced_2 <- ddply(one_peptide_reduced,.(l_lcrunid,modified_sequence),function(x){local_max <- max(x$total_spectrum_intensity)
                                                                                              x[x$total_spectrum_intensity==local_max,]})
# One peptide can be identified many times in the same lcrun : we keep the information of the most intense ion
one_peptide_reduced[,l_lcrunid := as.character(l_lcrunid)]
one_peptide_reduced[,total_spectrum_intensity := as.character(total_spectrum_intensity)]

setkey(one_peptide_reduced,l_lcrunid,modified_sequence,total_spectrum_intensity)
one_peptide_reduced_2 <- one_peptide_reduced[one_peptide_reduced[,j=.I[which.max(total_spectrum_intensity)], by=c("l_lcrunid","modified_sequence")]]

one_peptide_reduced_2 <- one_peptide_reduced[one_peptide_reduced[,j=list(.I[which.max(total_spectrum_intensity)]), by=c("l_lcrunid","modified_sequence")][,j=V1]]

setkey(one_peptide_reduced_2,l_lcrunid,modified_sequence)


