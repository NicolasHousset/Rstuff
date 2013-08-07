# This script will contain random syntax to learn using the lattice library
# Ultimately the goal is to quickly jump to the ggplot2 library, but lattice first

library(lattice)

histogram(~rtsec | modified_sequence, data = one_peptide_reduced_2,
          main = "Distribution of Retention Times by Peptides",
          xlab="Retention Time (in sec)")

attach(mtcars)

gear <- factor(gear, levels=c(3, 4, 5),
               labels=c("3 gears", "4 gears", "5 gears"))
cyl <- factor(cyl, levels=c(4, 6, 8),
              labels=c("4 cylinders", "6 cylinders", "8 cylinders"))

densityplot(~mpg,
            main="Density Plot",
            xlab="Miles per Gallon")

densityplot(~mpg | cyl,
            main="Density Plot by Number of Cylinders",
            xlab="Miles per Gallon")

bwplot(cyl ~ mpg | gear,
       main = "Box Plots by Cylinders and Gears",
       xlab = "Miles per Gallon", ylab="Cylinders")

xyplot(mpg ~ wt | cyl * gear,
       main="Scatter Plots by Cylinders and Gears",
       xlab="Car Weight", ylab="Miles per Gallon")

cloud(mpg ~ wt * qsec | cyl,
      main="3D Scatter Plots by Cylinders")

dotplot(cyl ~ mpg | gear,
        main="Dot Plots by Number of Gears and Cylinders",
        xlab="Miles Per Gallon")

splom(mtcars[c(1, 3, 4, 5, 6)],
      main="Scatter Plot Matrix for mtcars Data")

detach(mtcars)


mygraph <- densityplot(~height|voice.part, data=singer)
plot(mygraph)
update(mygraph, col="red")
update(mygraph, pch=16)
update(mygraph, jitter=.02)
update(mygraph, lwd=2)
