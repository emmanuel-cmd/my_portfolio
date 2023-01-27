# HEIGHT NORMALIZATION
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile)
plot(las, size = 3, bg = "white")

# To get a better idea of what the terrain looks like we remove all non-ground points
gnd <- filter_ground(las)
plot(gnd, size = 3, bg = "white", color = "Classification")

# DTM normalization
dtm <- rasterize_terrain(las, 1, knnidw())
plot(raster::raster(dtm), col = gray(1:50/50))

# Now that we have our surface and are satisfied 
# with it we can use it to normalize our point cloud through subtraction.
nlas <- las - dtm
plot(nlas, size = 4, bg = "white")

# All the ground surface being the reference 0 all 
# the ground points expected to be at Z=0 by definition
# But are they? Let's look at the distribution of ground points
hist(filter_ground(nlas)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")

# POINT CLOUD NORMALIZATION
nlas <- normalize_height(las, knnidw())

# All the ground points should be exactly 0. Let's check it
hist(filter_ground(nlas)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")

# Hybrid method - interpolation of the pixel of an already computed DTM
nlas <- normalize_height(las, tin(), dtm = dtm)

# Histogram distribution plot of values
hist(filter_ground(nlas)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")

# Reversing normalization
las2 <- unnormalize_height(nlas)
plot(las2, size = 4, bg = "white")
