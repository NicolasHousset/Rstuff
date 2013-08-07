

attach(rt_project625_sorted);
rt_project625_sorted$sequence <- factor(sequence);
liste_peptide <- summary(sequence);
detach(rt_project625_sorted);

!is.factor(rt_project625_sorted$sequence)

attach(rt_project625_sorted);
if(!is.factor(identified))
rt_project625_sorted$identified <- factor(identified);
detach(rt_project625_sorted);

# This command will keep the lines of the most frequent identified peptide
attach(rt_project625_sorted)
one_peptide <- rt_project625_sorted[(!is.na(sequence)) & sequence==labels(liste_peptide)[1],] 
detach(rt_project625_sorted)


library(vioplot);
vioplot(one_peptide$rtsec,
        names=c("Most frequent"),
        col="gold");
title("Violin Plots of Retention Times");

numBox <- (max(one_peptide$rtsec)-min(one_peptide$rtsec)) %/% 120 + 1;
Hist(one_peptide$rtsec, scale="frequency", breaks=numBox, col="darkgray");

plot(density(one_peptide$rtsec));