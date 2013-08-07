# This script will contain random syntax to learn using the ggplot2 library

library(ggplot2)

mtcars$cylinder <- as.factor(mtcars$cyl)
qplot(cylinder, mpg, data=mtcars, geom=c("boxplot", "jitter"),
      fill=cylinder,
      main="Box plots with superimposed data points",
      xlab= "Number of Cylinders",
      ylab="Miles per Gallon")

transmission <- factor(mtcars$am, levels=c(0, 1),
                       labels=c("Automatic", "Manual"))
qplot(wt,mpg, data=mtcars,
      color=transmission, shape = transmission,
      geom=c("point", "smooth"),
      method="lm", formula=y~x,
      xlab="Weight", ylab="Miles Per Gallon",
      main="Regression Example")

# For each combination of "number of cylinders" and "transmission type", make a scatter plot of mpg~wt
# The panels are input with the "facets" parameter
# Also, note that the size of each point depends on the horse power, hence the "size = hp"
mtcars$cyl <- factor(mtcars$cyl, levels=c(4, 6, 8),
                     labels=c("4 cylinders", "6 cylinders", "8 cylinders"))
mtcars$am <- factor(mtcars$am, levels=c(0, 1),
                    labels=c("Automatic", "Manual"))
qplot(wt,mpg, data=mtcars, facets=am~cyl, size=hp)


# Draws the kernel density for each type of singer
data(singer, package="lattice")
qplot(height, data=singer, geom=c("density"),
      facets=.~voice.part, fill=voice.part)


# Now going into the ggplot2 book

data(diamonds, package="ggplot2")

# Makes a reproductible sample of the diamonds dataset
# Note that the random numbers are first generated in a vector through the sample function,
# and then the [ function is called to subset the dataset
# Do not forget the last comma, otherwise you'll get a random sample of the columns, probably not what you want
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100),]

# A first exemple, with x and y parameters (implicit) and data
# Using the data parameter is not compulsory but advised
# R will first try to look up x and y into the data field, which should be a data frame
qplot(carat, price, data = diamonds)

# qplot accepts functions of variables as arguments
qplot(log(carat), log(price), data=diamonds)

# We can also combine existing variables
qplot(carat, x * y * z, data = diamonds)

# Want to map a colour for each value of one variable ? Just fill the colour attribute
qplot(carat, price, data = dsmall, colour = color)

# Similar reasoning for the shape
qplot(carat, price, data = dsmall, shape = cut)

# Different values of transparency. 
# The exemple doesn't work, maybe alpha can only be set with ggplot and not with qplot
qplot(carat, price, data = diamonds, alpha = 1/10)
qplot(carat, price, data = diamonds, alpha = 1/100)
qplot(carat, price, data = diamonds, alpha = 1/1000)


# Now exploring values of the "geom" parameter

# Default value of geom when x and y are filled is "point", but we can add a smoothed line
# In this case, it will plot the points AND the smoothed line
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))

# Geom boxplot and jitter
# Boxplot is good to quickly see differences in the main quantiles
# Jitter is more precize, but is sensible to overplotting

# Looking only at the jitter we could think that the data distribution is different amongst each color
qplot(color, price / carat, data = diamonds, geom = "jitter")
# But boxplots shows that there is not much change
qplot(color, price / carat, data = diamonds, geom = "boxplot")

# Looking at histogram and density plots
# The bining of carat is made inside the qplot function
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")

# We can control the size of the bins
# Note the xlim parameter allowing us to control the minimum and maximum x values (here, carat)
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, xlim=c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim=c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01, xlim=c(0,3))


# Adjust parameter is for the smoothing function, not qplot.
qplot(carat, data = diamonds, geom = "density", adjust = 0.5)

# Using barcharts

# The first barchart plots the total number of diamonds for each color
qplot(color, data = diamonds, geom = "bar")
# The second barchart sums the number of carat, the weight parameter is changed
qplot(color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")


# Using line plots
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

# We can plot the relation ship between unemployment rate and unemployment length over time
# The first plot will be hard to read because the points are joined together with no information about the date
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics, geom = c("point", "path"))
# The second plate colors the line proportionnally to the date, easier to read
qplot(unemploy / pop, uempmed, data = economics, geom = "path",
      colour = year(date)) + scale_area()


# Back to diamonds dataset with faceted histograms

# The first plot might be hard to read because the y scale is the same for all the panels
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram",
      binwidth = 0.1, xlim = c(0, 3))
# Using the ..density.. function for the y parameter makes an appropriate y-scale for each panel
qplot(carat, ..density.., data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))


# Labeling options with xlab, ylab and main parameters
qplot(
  carat, price, data = dsmall,
  xlab = "Price ($)", ylab = "Weight (carats)",
  main = "Price-weight relationship"
  )

# We can use mathematical expressions for labeling, see ?plotmath
# Parameter xlim to plot only part of the data
qplot(carat, price/carat, data = dsmall,
      ylab = expression(frac(price,carat)),
      xlab = "Weight (carats)",
      main="Small diamonds",
      xlim = c(.2,1))

# Using log-scales with the parameter log. Possible values "x", "y" or "xy"
qplot(carat, price, data = dsmall, log ="xy")


# Going in third chapter of the ggplot2 book

qplot(displ, hwy, data = mpg, colour = factor(cyl))
# plot that doesn't make sense, but is grammatically correct
# "The angry rock barked like a comma" is a correct English sentence, but makes no sense
qplot(displ, hwy, data = mpg, colour = factor(cyl), geom="line")
qplot(displ, hwy, data = mpg, colour = factor(cyl), geom="bar", stat="identity")


# Going into more complex plot

qplot(displ, hwy, data=mpg, facets = . ~ year) + geom_smooth()


p <- qplot(displ, hwy, data = mpg, colour = factor(cyl))
# We can call the summary function to a plot object
summary(p)

# We can save the plot, and load it later
save(p, file = "plot.rdata")
load("plot.rdata")

# We can make a png of the plot
ggsave("plot.png", width = 5, height = 5)

# And now entering chapter 4 !
# Now plotting with the ggplot function

p <- ggplot(diamonds, aes(carat, price, colour = cut))
# p can not be displayed yet because there is no layer : nothing to see !

# If we add a layer it becomes displayable. Here a very simple one
p <- p + layer(geom = "point")

# A full definition of the layer
p <- ggplot(diamonds, aes(x = carat))
p <- p + layer(
  geom = "bar",
  geom_params = list(fill = "steelblue"),
  stat = "bin",
  stat_params = list(binwidth = 2)
  )
p

# We can make it shorter, though
p <- ggplot(diamonds, aes(x = carat))

# The default stat for geom_bar is "bin" : no need to provide it. Possible to override it and get exotic results.
# Note however that you can't map the properties to a variable in the dataset
# Here we set the property "fill" to the fixed value "steelblue"
p <- p + geom_bar(fill = "steelblue", binwidth = 2)
p


# ggplot and qplot are doing the same thing, it's just that qplot adds the layer directly
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) + geom_point()
# is equivalent to
qplot(sleep_rem / sleep_total, awake, data = msleep)

# It is also possible to add layers to qplot
qplot(sleep_rem / sleep_total, awake, data = msleep) + geom_smooth()
# Which is equivalent to
qplot(sleep_rem / sleep_total, awake, data = msleep,
      geom=c("point", "smooth"))
# and
ggplot(msleep, aes(sleep_rem / sleep_total, awake)) +
  geom_point() + geom_smooth()


p <- ggplot(msleep, aes(sleep_rem / sleep_total, awake))
# The summary function is usefeul for inspecting the structure of a plot without plotting it
summary(p)
p <- p + geom_point()
summary(p)

# We can store layers as variables and apply them to different plots
bestfit <- geom_smooth(method = "lm", se = F,
                       colour = alpha("steelblue",0.5), size = 2)
# Note : the alpha parameter still doesn't work, what's wrong ?
# Solution found : package scales needs to be imported (should be imported automatically...)
qplot(sleep_rem, sleep_total, data = msleep) + bestfit

p <- ggplot(mtcars, aes(mpg, wt, colour = cyl)) + geom_point()
p
mtcars <- transform(mtcars, mpg = mpg ^ 2)
p %+% mtcars


p <- ggplot(mtcars, aes(x = mpg, y = wt))
p + geom_point()
# We can add a different layer afterwards
p + geom_point(aes(colour = factor(cyl)))
p + geom_point(aes(y=disp))
# Note : label of the y axis is not changed

# The function aes is used to map aesthetics parameters to a variable
# If we want to set a fixed value, we don't need to wrap it in a call to aes()
# If we call aes() to set a fixed value, it will probably not work as expected

# Here the point colour is set to be dark blue instead of black
p <- ggplot(mtcars, aes(mpg, wt))
p + geom_point(colour = "darkblue")

# This code is quite different
p + geom_point(aes(colour = "darkblue"))
# Here the value is mapped to a variable containing only the value "darkblue"
# The value is discrete with only one value >> colour becomes pinkish


# How to group plots ?

# Here we get time series of height on different values of age
# It is important to specify the group so that observations are correctly connected
p <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()
p

# Smoothed line for each boy. Not what we want : one smoothed line for all the boys
p + geom_smooth(aes(group = Subject), method="lm", se = F)

# For that we need to use a different grouping for this layer
p + geom_smooth(aes(group = 1), method="lm", size = 2, se = F)


# An example of overiding the default grouping
boysbox <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot()
# Default grouping is the x value, so it works fine
boysbox
# If we want to add a line for each boy we'll need to overide the grouping
boysbox + geom_line(aes(group = Subject), colour = "#3366FF")


d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
# Based on a histogram, plot the size of the bin and shade the area below it
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
# Wait, we can use different geom to plot this statistical transformation !
d + stat_bin(
  aes(size = ..density..), binwidth = 0.1,
  geom = "point", position="identity")
# This one is weird
d + stat_bin(
  aes(y = 1, fill = ..count..), binwidth = 0.1,
  geom = "tile", position="identity")


# One of the more powerful capabilities of ggplot2 is the ability to plot different data sets on different layers
# So let's use this capability !
# Common example : supplementing the data with predictions from a model

# First, we fit a model, and build the base plot of the data
require(nlme, quiet = TRUE, warn.conflicts = FALSE)
model <- lme(height ~ age, data = Oxboys, random = ~ 1 + age | Subject)
oplot <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line()

# Then we build a data frame of all the combinations of age and subject
age_grid <- seq(-1, 1, length = 10)
subjects <- unique(Oxboys$Subject)
preds <- expand.grid(age = age_grid, Subject = subjects)
# We apply the model to predict all the combinations
preds$height <- predict(model, preds)

# We add a layer where we change the dataset source, to plot predictions
oplot + geom_line(data = preds, colour = "#3366FF", size= 0.4)
# This plot is hard to understand, let's look at residuals

# We first add the predictions in the original dataset, and the residuals
Oxboys$fitted <- predict(model)
Oxboys$resid <- with(Oxboys, fitted - height)

# Then we update the base dataset (with %+%), and the base y axis is switched to the residuals
# Above that, we add a layer with a smoothed line
oplot %+% Oxboys + aes(y= resid) + geom_smooth(aes(group=1))
# We can clearly see with the smoothed line that the residuals are not random : no good, change the model

# We add a quadratic term
model2 <- update(model, height ~ age + I(age ^ 2))
Oxboys$fitted2 <- predict(model2)
Oxboys$resid2 <- with(Oxboys, fitted2 - height)

# Residuals seem more random
oplot %+% Oxboys + aes(y = resid2) + geom_smooth(aes(group=1))

# A conclusion : layering in ggplot2 is designed to work well with the iterative process of fitting and evaluating models


# And now going into chapter 5 of the ggplot2 book !
# Let's explore our possibilities !

# Basic plot types

df <- data.frame(x = c(3, 1, 5), y = c(2, 4, 6), label = c("a","b","c"))
p <- ggplot(df, aes(x, y, label = label)) + xlab(NULL) + ylab(NULL)
p + geom_point() + ggtitle("geom_point")
p + geom_bar(stat="identity") + ggtitle("geom_bar(stat=\"identity\")")
p + geom_line() + ggtitle("geom_line")
p + geom_area() + ggtitle("geom_area")
p + geom_path() + ggtitle("geom_path")
p + geom_text() + ggtitle("geom_text")
p + geom_tile() + ggtitle("geom_tile")
p + geom_polygon() + ggtitle("geom_polygon")


# Plot for displaying distributions

depth_dist <- ggplot(diamonds, aes(depth)) + xlim(58, 68)
# A facetised histogram, which helps seeing the differences between cuts
depth_dist +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  facet_grid(cut ~ .)
# This one gives an interesting point of view
depth_dist +
  geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill")
# I don't like this so much, personal point of view
depth_dist +
  geom_freqpoly(aes(y = ..density.., colour = cut), binwidth = 0.1)

# We can use boxplots conditioned on categorical or continuous variables, if we bin them first
qplot(cut, depth, data=diamonds, geom="boxplot")
qplot(carat, depth, data=diamonds, geom="boxplot",
      group = round_any(carat, 0.1, floor), xlim = c(0, 3))

# The jitter plot allows to handle better overplotting
qplot(class, cty, data=mpg, geom="jitter")
qplot(class, drv, data=mpg, geom="jitter")

# If the amount of overplotting is small, altering the shape might just be enough
# Playing with this part is very fun
df <- data.frame(x = rnorm(200000), y = rnorm(20000))
norm <- ggplot(df, aes(x, y))
norm + geom_point()
norm + geom_point(shape = 1)
norm + geom_point(shape = ".") # Pixel sized

norm + geom_point(colour = alpha("black", 1/2))
norm + geom_point(colour = alpha("black", 1/5))
norm + geom_point(colour = alpha("black", 1/10))



# Using ggplot2 to draw maps
library(maps)
data(us.cities)

# Just plotting big cities on the US map
big_cities <- subset(us.cities, pop > 500000)
qplot(long, lat, data = big_cities) + borders("state", size = 0.5)

# Plotting TEXAS cities
tx_cities <- subset(us.cities, country.etc == "TX")
ggplot(tx_cities, aes(long, lat)) +
  borders("county", "texas", colour = "grey70") +
  geom_point(colour = alpha("black", 0.5))

# Plotting crime rates in different states
# I would say, a very classical one, for americans...
# 
states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

# Data frame syntax is horrible but anyway, for this size it's OK
choro <- merge(states, arrests, by = "region")
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, fill = assault, geom = "polygon")
qplot(long, lat, data = choro, group = group, fill = assault / murder, geom="polygon")


# Using ggplot2 to plot uncertainty

d <- subset(diamonds, carat < 2.5 & rbinom(nrow(diamonds),1, 0.2) == 1)
d$lcarat <- log10(d$carat)
d$lprice <- log10(d$price)

# Remove overall linear trend
detrend <- lm(lprice ~ lcarat, data = d)
d$lprice2 <- resid(detrend)

mod <- lm(lprice2 ~ lcarat * color, data = d)

library(effects)

effectdf <- function(...){
  suppressWarnings(as.data.frame(effect(...)))
}
color <- effectdf("color", mod)
both1 <- effectdf("lcarat:color", mod)

carat <- effectdf("lcarat", mod, default.levels = 50)
both2 <- effectdf("lcarat:color", mod, default.levels = 3)