# Just a function to measure the time of something

timethis <- function(...) {
  start.time <- Sys.time();
  eval(..., sys.frame(sys.parent(sys.parent())));
  end.time <- Sys.time();
  print(end.time - start.time)
}

# What is the best way of saving data ? Let's find out...

# No compression
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_NC.RData", compress = FALSE))
# It takes 1.3 seconds to save and weights 250 MB

# ASCII format
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_ASCII.RData", ascii = TRUE))
# It takes 22 seconds to save and weights 233 MB
# This one is present for historical reasons but should not be used anymore
# Like said in the help, it can be slightly lighter than the non-compressed but is awfully slow

# By default, gzip compression level 6
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified.RData"))
# It takes 13 seconds to save and weights 43 MB

# GZIP format level 9
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_GZIP9.RData", compression_level = 9))
# It takes 72 seconds to save and weights 42 MB. Just stick to level 6, right...

# GZIP format level 3
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_GZIP3.RData", compression_level = 3))
# It takes 5 seconds to save and weights 48 MB

# GZIP format level 1
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_GZIP1.RData", compression_level = 1))
# It takes 3.7 seconds to save and weights 48 MB (lighter than lvl 3 -_-)
# Even level 1 compresses extremely well, and very fast
# I will (generally) use this one : huge gain, low pain


# BZIP2 format
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_BZIP.RData", compress = "bzip2"))
# It takes 34 seconds to save and weights 36 MB

# BZIP2 format level 6
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_BZIP6.RData", compress = "bzip2", compression_level = 6))
# It takes 32 seconds to save and weights 36 MB (0.1 MB difference, hum)

# BZIP2 format level 3
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_BZIP3.RData", compress = "bzip2", compression_level = 3))
# It takes 30 seconds to save and weights 36 MB (0.25 MB difference, hum)



# XZ format
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_XZ.RData", compress = "xz"))
# It takes 115 seconds to save and weights 24 MB


# XZ format level 3
timethis(save(identified, file = "C:/Users/Nicolas Housset/Documents/identified_XZ3.RData", compress = "xz", compression_level = 3))
# It takes 30 seconds to save and weights 30 MB


gc()