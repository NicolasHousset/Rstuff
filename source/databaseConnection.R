# The binary for the RMySQL package is not available, you have to compile the source, and the process is not very straightforward
# A Stackoverflow question gives part of what to do : http://stackoverflow.com/questions/4785933/adding-rmysql-package-to-r-fails
# 1. Install latest RTools
# 2. Install MySQL
# 3. Edit file C:\R_PATH\etc\Renviron.site and add line MYSQL_HOME=C:/Program Files/MySQL/MySQL Server 5.6 (for me the file didn't exist so I created it, path of MySQL is where your bin and lib folder would be)
# You can ensure that step 3 is correct by typing Sys.getenv('MYSQL_HOME') on a new R prompt
# 4. copy libmysql.lib from mysql/lib to mysql/lib/opt to meet dependencies. (not libmysql.dll, I got confused)
# 5. copy libmysql.dll to C:\R_PATH\bin AND to C:\MySQL_PATH\bin (R will look up the dll in the bin folders, though it is originally in the lib folder)
# 6. run install.packages('RMySQL',type='source') and wait while compilation will end
# 7. happily go crash the database you want to connect to

library(RMySQL);
con <- dbConnect(MySQL(), group="MSDB", dbname="projects");

rt_project625 <- dbGetQuery(con,
                          "SELECT scanid, spectrumid, l_lcrunid, l_projectid, l_instrumentid, identified, rtsec, total_spectrum_intensity, mass_to_charge, spectrum.charge, accession, start, end, sequence, modified_sequence FROM
(spectrum LEFT JOIN scan ON spectrum.spectrumid = scan.l_spectrumid 
LEFT JOIN identification ON spectrum.spectrumid = identification.l_spectrumid
LEFT JOIN spectrum_file ON spectrum.spectrumid = spectrum_file.l_spectrumid)
WHERE l_projectid = 625 LIMIT 0, 300000;");

attach(rt_project625);
rt_project625_sorted <- rt_project625[order(l_lcrunid,rtsec),];
detach(rt_project625);

attach(rt_project625_sorted);
rt_project625_sorted$sequence <- factor(sequence);
liste_peptide <- summary(sequence);
detach(rt_project625_sorted);

liste_peptide[1];

one_lcrun <- rt_project625_sorted [rt_project625_sorted$l_lcrunid == 52654,];

one_lcrun$identified <- factor(one_lcrun$identified); 
summary(one_lcrun$identified);

library(sm);
sm.density.compare(one_lcrun$rtsec, one_lcrun$identified, xlab="Retention time");
title(main="Retention time by number of identifications");
colfill <- c(2:(1+length(levels(one_lcrun$identified))));
legend(locator(1), levels(one_lcrun$identified), fill=colfill);

boxplot(rtsec ~ identified, data = one_lcrun,
        notch=TRUE,
        varwidth=TRUE,
        col="red",
        main="Useless Boxplot",
        xlab="Identification",
        ylab="Retention time");

library(vioplot);
vioplot(one_lcrun$rtsec[one_lcrun$identified==0], one_lcrun$rtsec[one_lcrun$identified==1],
        names=c("Not identified", "Identified"),
        col="gold");
title("Violin Plots of Retention Times");


one_lcrun_id <- one_lcrun[one_lcrun$identified == 1,];
one_lcrun_noid <- one_lcrun[one_lcrun$identified == 0,];

numBox <- (max(one_lcrun$rtsec)-min(one_lcrun$rtsec)) %/% 120 + 1;
Hist(one_lcrun_id$rtsec, scale="frequency", breaks=numBox, col="darkgray");
Hist(one_lcrun_noid$rtsec, scale="frequency", breaks=numBox, col="darkgray");

plot(density(one_lcrun_id$rtsec));
plot(density(one_lcrun_noid$rtsec));

testLoop <- foreach(i=1:18) %do% aBiggerTest[1,i];


