# In the majority of cases crown_metrics() is run after 
# segmenting tree crowns with segment_trees() (section 7) 
# but the segmentation could also be performed in another way independently of lidR

LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR") 
las <- readLAS(LASfile, filter = "-drop_z_below 0") # read the file

# The output is a sf/sfc_POINT
metrics <- crown_metrics(las, ~list(z_max = max(Z), z_mean = mean(Z))) # calculate tree metrics
head(metrics)

# Visualizing z_max using several colours
plot(metrics["z_max"], pal = hcl.colors, pch = 19) # plot using z_max

# Users can create their custom functions containing 
# all of the metrics of interest. 
custom_crown_metrics <- function(z, i) { # user-defined function
  metrics <- list(
    z_max = max(z),   # max height
    z_sd = sd(z),     # vertical variability of points
    i_mean = mean(i), # mean intensity
    i_max  = max(i)   # max intensity
  )
  return(metrics) # output
}
ccm = ~custom_crown_metrics(z = Z, i = Intensity)

# crown_metrics can also return sf/sfc_POLYGON by changing the geom argument
metrics <- crown_metrics(las, func = ccm, geom = "convex")
plot(metrics["z_max"], pal = hcl.colors)

# Change to concave for 'geom' argument
metrics <- crown_metrics(las, func = ccm, geom = "concave")
plot(metrics["z_max"], pal = hcl.colors)

# Applications
# Selection of trees
# Crown_metrics gives the id of trees and associated metrics
metrics <- crown_metrics(las, ~list(imean = mean(Intensity))) # calculate tree intensity metrics
metrics <- metrics[metrics$imean > 80,] # filter intensity

# We can remove trees with a low intensity
subset <- filter_poi(las, treeID %in% metrics$treeID)
x <- plot(las, bg = "white", size = 4)
plot(subset, add = x + c(-100, 0), size = 5) # some plotting

# Tree based inventory
metrics <- crown_metrics(las, func = ~custom_crown_metrics(Z, Intensity)) # calculate intensity metrics
metrics$G <- 0.7 * metrics$z_max + 0.1 * metrics$i_mean # set value of interest
plot(metrics["G"], pal = hcl.colors, pch = 19) # some plotting

# We can rasterize the map
r <- terra::rast(ext(las),  resolution = 15)
v <- terra::vect(metrics["G"])
map <- terra::rasterize(v, r, field = "G", fun = sum) # extract sum of G at 15m
plot(raster(map), col = hcl.colors(15)) # some plotting
