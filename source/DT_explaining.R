# Script for details about how to solve the "Remove grouped spectra which are too far apart and keep only one line per group" problem

# A first solution is to use ddply from the plyr package

one_peptide$diff <- NA

one_peptide_reduced <- ddply(one_peptide,.(spectrumid),function(x){x$diff[1] <- max(x$number)-min(x$number)
                                                                   x[as.numeric(x$diff[1]<=5),]})

# Here, ddply will split the dataframe one_peptide into smaller dataframes, one per unique spectrumid
# Then it will compute (once) the difference between the max and the min of number
# Number is a way of identifying a scan within an lcrun, grouped scan (sharing the same spectrumid) should have close "numbers"

# As I was working with small dataframes, this code was working fine
# But when I went with bigger dataframes, this code would take forever to compute
# The reason is located in the subsetting of dataframes
# Here, one_peptide would be split like that :
# one_peptide_grp1 = one_peptide[one_peptide$spectrumid == "specid_1",]
# one_peptide_grp2 = one_peptide[one_peptide$spectrumid == "specid_2",]
# one_peptide_grp3 = one_peptide[one_peptide$spectrumid == "specid_3",]
# ...
# Those individual statements are pretty long in themselves, but we have to consider that if we have 1000 unique specid, each
# statement would be repeated 1000 times.
# The computing time of one subsetting depends on the number of rows, we'll call it n
# The number of subsets is closely related to the number of rows
# Our complexity is then O(n²)
# If we double the size of our dataframe, the computing time will be 4 times bigger

# Another (big) reason why ddply takes forever to compute is that all the columns of one_peptide are copied when subsetting,
# which consumes a lot of memory. We can solve that by selecting first the necessary columns, the memory consumption will
# be considerably reduced and it will be definitely faster

timethis <- function(...) {
  start.time <- Sys.time();
  eval(..., sys.frame(sys.parent(sys.parent())));
  end.time <- Sys.time();
  print(end.time - start.time)
}

one_peptide_cols <- one_peptide[,c("spectrumid","number","diff")]

# This one is better because it will work on a (column) subset of one_peptide
one_peptide$diff <- NA
# Takes 1.25 seconds to compute
system.time(one_peptide_reduced <- merge(one_peptide,ddply(one_peptide[,c("spectrumid","number","diff")],.(spectrumid),function(x){x$diff[1] <- max(x$number)-min(x$number)
                                                                                          x[as.numeric(x$diff[1]<=5),]}),
                             by=c("spectrumid","number")))


# Takes 9 seconds to compute, should not be that long
timethis(one_peptide_T_reduced <- one_peptide_T[one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid][V1<=5],j=.SD[1]])

# This one is blazing fast (0.01 sec), we clearly see that it is the last part taking so long
timethis(one_peptide_T_reduced <- one_peptide_T[one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid][V1<=5]])

# Maybe this one ?
timethis(one_peptide_T_reduced <- one_peptide_T[one_peptide_T[j=list("max(as.numeric(number))-min(as.numeric(number))",
                                                                     ".SD"), by=spectrumid][V1<=5]])

# Or this one ?
timethis(one_peptide_T_reduced <- one_peptide_T[one_peptide_T[i=min(as.numeric(number)),j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid][V1<=5]])


timethis(one_peptide_T_reduced <- one_peptide_T[one_peptide_T[j=max(as.numeric(number))-min(as.numeric(number)), by=spectrumid, mult="first"][V1<=5]])

# The inside actually works fine
one_peptide_T[j=list(max(as.numeric(number))-min(as.numeric(number)),
                     number), by=spectrumid, mult="first"][V1<=5]
# Oh, oh, oooooooh !
timethis(one_peptide_T[j=list(max(as.numeric(number))-min(as.numeric(number)),
                     number), by=spectrumid][V1<=5][j=.SD[1], by=spectrumid])

# Computing time of 0.94 seconds
timethis(one_peptide_T_reduced <- merge(one_peptide_T[j=list(max(as.numeric(number))-min(as.numeric(number)),
                           number), by=spectrumid][V1<=5][j=.SD[1], by=spectrumid],
      one_peptide_T,
      by=c("spectrumid","number")))

# Instead of adhoc by, set directly the key
timethis(one_peptide_T_reduced <- merge(setkeyv(one_peptide_T[j=list(max(as.numeric(number))-min(as.numeric(number)),
                                                                     number), by=spectrumid][V1<=5][j=.SD[1], by=spectrumid],
                                                c("spectrumid","number")),
                                        one_peptide_T))

# We can still go faster
timethis(one_peptide_T_reduced <- one_peptide_T[setkeyv(one_peptide_T[j=list(max(as.numeric(number))-min(as.numeric(number)),
                                           number), by=spectrumid][V1<=5][j=.SD[1], by=spectrumid],
                      c("spectrumid","number"))])

timethis(one_peptide_T_reduced <- one_peptide_T[setkeyv(one_peptide_T[j=list(max(as.numeric(number))-min(as.numeric(number)),
                                                                             number), by=spectrumid][i=V1<=5,j=.SD[1],by=spectrumid],
                                                        c("spectrumid","number"))])

timethis(one_peptide_T_reduced <- unique(setkey(one_peptide_T[setkeyv(one_peptide_T[j=list(max(as.numeric(number))-min(as.numeric(number)),
                                                                             number), by=spectrumid][i=V1<=5],
                                                        c("spectrumid","number"))]),spectrumid))

# The ultimate code : runs in 0.25 seconds with 40000 rows
system.time(
  one_peptide_T_reduced <- unique(
    setkey(
      one_peptide_T[
        setkeyv(
          one_peptide_T[
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

one_peptide_T_test <- setkey(
  one_peptide_T[
    setkeyv(
      one_peptide_T[
        j=list(
          max(as.numeric(number))-min(as.numeric(number)),
          number),
        by=spectrumid]
      [i=V1<=5],
      c("spectrumid","number")
    )
    ]
  ,spectrumid)


system.time(
  one_peptide_T_reduced <- unique(
    setkey(
      one_peptide_T[
        setkeyv(
          one_peptide_T[
            j=list(
              max(as.numeric(number))-min(as.numeric(number)),
              .I[1], # if number is sorted in ascending order into each spectrumid
              number),
            by=spectrumid]
          [i=V1<=5],
          c("spectrumid","number")
        )
        ]
      ,spectrumid)
  )
)

system.time(
  one_peptide_T_reduced <-
      one_peptide_T[
        setkeyv(
          one_peptide_T[
            j=list(
              max(as.numeric(number))-min(as.numeric(number)),
              .I[1], # if number is sorted in ascending order into each spectrumid
              number),
            by=spectrumid]
          [i=V1<=5],
          c("spectrumid","number")
        )
        ]
)

system.time(one_peptide_test <- # setkeyv(
  one_peptide_T[
    j=list(
      max(as.numeric(number))-min(as.numeric(number)),
      number)
    ,by=c("spectrumid")
    ]
  [i=V1<=5,j=.I[which.min(number)],by=spectrumid],
  c("spectrumid","number")
))

setkey(one_peptide_T_reduced, spectrumid)

one_peptide_T_reduced <- unique()

sum(duplicated(one_peptide_T_reduced))

# Near instant update (0.05 sec), slight increase in memory consumption (needed to expand the data.table)
system.time(rt_project[, dummy_var1 := 7])

# 3 seconds update, the whole data.frame is copied into memory
system.time(rt_project$dummy_var2 <- 8)
