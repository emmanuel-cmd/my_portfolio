library(lidR)
library(raster)
library(terra)

LASfile <- system.file("extdata", "MixedConifer.laz", package ="lidR")
las <- readLAS(LASfile)
plot(las, size = 3, bg = "white")

# POINT-TO-RASTER
chm <- rasterize_canopy(las, res = 1, algorithm = p2r())
col <- height.colors(25)
plot(raster::raster(chm), col = col)

# Increasing the spatial resolution to 0.5m
chm <- rasterize_canopy(las, res = 0.5, algorithm = p2r())
plot(raster::raster(chm), col = col)

# Replace every point in the point cloud with 
# a disk of known radius e.g. 15cm to reduce the 
# number of voids. 
chm <- rasterize_canopy(las, res = 0.5, algorithm = p2r(subcircle = 0.15))
plot(raster::raster(chm), col = col)

# additional argument to the p2r function for interpolation
chm <- rasterize_canopy(las, res = 0.5, p2r(0.2, na.fill = tin()))
plot(raster::raster(chm), col = col)

# To create a surface model using triangulation we use algorithm = dsmtin()
chm <- rasterize_canopy(las, res = 0.5, algorithm = dsmtin())
terra::plot(raster::raster(chm), col=col)

# The triangulation may also be weak when a lot of points are mising 
# We can generate example using the Topography.laz dataset that contains 
# empty lakes
LASfile <- system.file("extdata", "Topography.laz", package = "lidR")
las2 <- readLAS(LASfile)
las2 <- normalize_height(las2, algorithm = tin())
plot(las2, size = 3, bg = "white")

# In this case the CHM is incorrectly computed in the empty lakes
chm <- rasterize_canopy(las2, res = 0.5, algorithm = dsmtin())
plot(chm, col = col)

# To fix this, one option is to use the max_edge argument, 
# which defines the maximum edge of a triangle allowed in the 
# Delaunay triangulation. It means that every triangle with an 
# edge longer than 8 will be discarded from the triangulation. 
chm <- rasterize_canopy(las2, res = 0.5, algorithm = dsmtin(max_edge = 8))
plot(raster::raster(chm), col = col)

# PIT-FREE ALGORITHM 
# Consists of a series of sequential height thresholds where Delauney 
# triangulations are applied to first returns. For each threshold, the triangulation 
# is cleansed of triangles that are too large similar to the example given in the previous section. 

# The first layer is a regular triangulation
layer0 <- rasterize_canopy(las, res = 0.5, algorithm = dsmtin())

# Triangulation of first return above 10 m
above10 <- filter_poi(las, Z >= 10)
layer10 <- rasterize_canopy(above10, res = 0.5, algorithm = dsmtin(max_edge = 1.5))

# Triangulation of first return above 20 m
above20 <- filter_poi(above10, Z >= 20)
layer20 <- rasterize_canopy(above20, res = 0.5, algorithm = dsmtin(max_edge = 1.5))

# The final surface is a stack of the partial rasters
dsm <- layer0
dsm[] <- pmax(as.numeric(layer0[]), as.numeric(layer10[]), as.numeric(layer20[]), na.rm = T)

layers <- c(layer0, layer10, layer20, dsm)
names(layers) <- c("Base", "Layer10m", "Layer20m", "pitfree")
plot(raster::stack(layers), col = col, axes=F, box=F)

# The above can be run on one line using the pitfree()
chm <- rasterize_canopy(las, res = 0.5, pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5)))
plot(raster::raster(chm), col = col)

# By increasing the max_edge to 2.5, the CHM becomes smoother but also less realistic
chm <- rasterize_canopy(las, res = 0.5, pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 2.5)))
plot(raster::raster(chm), col = col)

# the pit-free algorithm also includes a subcircle option that replaces each first return by a disk made of 8 points.
# Because of high computational demands, the choice was made to include only the highest point of each pixel after subcircling to perform the triangulation
chm <- rasterize_canopy(las, res = 0.5, pitfree(subcircle = 0.15))
plot(raster::raster(chm), col = col)

# POST PROCESSING
# Unfortunately lidR is a point cloud oriented software so it does not provide any tools for postprocessing CHM
fill.na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) }}
w <- matrix(1, 3, 3)

# fill NA 
chm <- rasterize_canopy(las, res = 0.5, algorithm = p2r(subcircle = 0.15), pkg = "terra")
filled <- terra::focal(chm, w, fun = fill.na)
smoothed <- terra::focal(chm, w, fun = mean, na.rm = TRUE)

# Plot
chms <- c(chm, filled, smoothed)
names(chms) <- c("Base", "Filled", "Smoothed")
plot(raster::stack(chms), col = col)
