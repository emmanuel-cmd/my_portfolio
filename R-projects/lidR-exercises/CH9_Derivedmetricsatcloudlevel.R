# DERIVED METRICS AT THE CLOUD LEVEL

LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile)
cloud_metrics(las, func = ~mean(Z)) # calculate mean height

# When several metrics are computed they are returned as a list
metrics <- cloud_metrics(las, func = .stdmetrics_z)
str(head(metrics)) # output is a list

# Applications
# We load .las and compute the metrics for each plot inventory using a shapefile of plot centers
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile, filter = "-keep_random_fraction 0.5")
shpfile <- system.file("extdata", "efi_plot.shp", package="lidR")
inventory <- sf::st_read(shpfile, quiet = TRUE)
metrics <- plot_metrics(las, .stdmetrics_z, inventory, radius = 11.28)

# Take a look at the metrics object which contain the plot ID, VOI and each metrics calculated
# we can use that to build a linear model with some metrics
# Here we have only 5 plots 
model <- lm(VOI~zsd+zmax, data = metrics)
summary(model)
