library(lidR)
library(ggplot2)
library(rlang)

# Load LAZ file
las <- readLAS("3dm_32_280_5652_1_nw.laz")
print(las)

# Selectively load the attributes of interest
# Load XYZ only
las <- readLAS("3dm_32_280_5652_1_nw.laz", select = "xyz")  # load XYZ only
# Load XYZ and intensity only
las <- readLAS("3dm_32_280_5652_1_nw.laz", select = "xyzi") # load XYZ and intensity only

# Parameter filter drop rows of the data 
# Read only first returns
las <- readLAS("3dm_32_280_5652_1_nw.laz", filter = "-keep_first") # Read only first returns

las1 <- readLAS("3dm_32_280_5652_1_nw.laz", filter = "-keep_first")

# The function filter_poi may return the exact same output 
# as the filter option in readLAS
las2 <- readLAS("3dm_32_280_5652_1_nw.laz")
las2 <- filter_poi(las2, ReturnNumber == 1L)

# Read first return between 5 and 50m
#las <-  readLAS("3dm_32_280_5652_1_nw.laz", filter = "-keep_first -drop_z_below 11.125 -drop_z_above 130.725")
# Check all available functions
readLAS(filter = "-help")

# las_check if las objects meets the ASPRS LAS specifications
las_check(las)

# Plot las object by scan angle, 
# make the background white, 
# display XYZ axis and  scale colors
plot(las, color = "ScanAngleRank", bg = "white", axis = TRUE, legend = TRUE)

# If data contains RGB data
plot(las, color ="RGB")

plot(las, color = "Intensity", breaks = "quantile", bg = "white")

# OVERLAYS
x <- plot(las, bg = "white", size = 3)
add_dtm3d(x, dtm)

# We separate the vegetation and non-vegetation points using filter_poi
nonveg <- filter_poi(las, Classification != LASHIGHVEGETATION)
veg <- filter_poi(las, Classification == LASHIGHVEGETATION)

# Plot together
x <- plot(nonveg, color = "Classification", bg = "white", size = 3)
plot(veg, add = x)

# Extract shift values that can be used later to realign other objects
offsets <- plot(las)
print(offsets)

# We read a file
LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzc")

# get the location of the trees
ttops <- locate_trees(las, lmf(ws = 5)) 

# plot the point cloud
offsets <- plot(las, bg = "white", size = 3)
add_treetops3d(offsets, ttops)

# extract the coordinates of the trees and
# apply the shift to display the lines
# in the rendering coordinate system
x <- sf::st_coordinates(ttops)[,1] - offsets[1] 
y <- sf::st_coordinates(ttops)[,2] - offsets[2] 
z <- ttops$Z

# Build a GL_LINES matrix for fast rendering
x <- rep(x, each = 2)
y <- rep(y, each = 2)
tmp <- numeric(2*length(z)) 
tmp[2*1:length(z)] <- z
z <- tmp
M <- cbind(x,y,z)

# Display lines
rgl::segments3d(M, col = "black", lwd = 2)

# Voxel rendering 
vox <- voxelize_points(las, 6)
plot(vox, voxel = TRUE, bg = "white")

# Cross sections 2D rendering
p1 = c(min(las@data$X), mean(las@data$Y))
p2 = c(max(las@data$X), mean(las@data$Y))
las_tr <- clip_transect(las, p1, p2, width = 4, xz = TRUE)

# Plotting
ggplot(las_tr@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))

# Clipping and plotting can be combined in a function
plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 4, colour_by = NULL)
{
  colour_by <- rlang::enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}

plot_crossection(las, colour_by = factor(Classification))
