
# The average height of point for the point cloud, for each pixel, 
# each tree crown, each voxel or for each heaxgonal cell can be
# calculated using mean(Z)
# All the functions work the same way but the output format depends 
# on the regularizatin level. 
cloud_metrics(las, func = ~mean(Z))
pixel_metrics(las, func = ~mean(Z))
tree_metrics(las, func = ~mean(Z))
hexagon_metrics(las, func = ~mean(Z))
voxel_metrics(las, func = ~mean(Z))

# In the following example we compute avg intensity at different levels 
# of regularization
LASfile <- system.file("extdata", "MixedConifer.laz", package ="lidR")
las <- readLAS(LASfile)

m <- ~list(avgI = mean(Intensity))

a <- pixel_metrics(las, m, res = 5)
b <- crown_metrics(las, m, geom = "point")
c <- crown_metrics(las, m, geom = "convex")
d <- hexagon_metrics(las, m, area = 25)

par(mfrow=c(2,2))
plot(raster(a), col = heat.colors(15), legend = FALSE)
plot(b["avgI"], pal = heat.colors, pch = 19, cex = 1, axes = TRUE, key.pos = NULL, reset = FALSE)
plot(c["avgI"], pal = heat.colors, axes = TRUE, key.pos = NULL, reset = FALSE)
plot(d["avgI"], pal = heat.colors, axes = TRUE, key.pos = NULL, reset = FALSE)

# USER DEFINED FUNCTIONS
# In order to calculate the mean of elevation, sd and mean 
# of intensity, the following function can be used: 
f <- function(z, i) {
  list(
    mean = mean(z), 
    sd = sd(i),
    imean = mean(i))
}

# The user defined function f can then be used:
cloud_metrics(las, func = ~f(Z, Intensity))
grid_metrics(las, func = ~f(Z, Intensity))
tree_metrics(las, func = ~f(Z, Intensity))
haxagon_metrics(las, func = ~f(Z, Intensity))
voxel_metrics(las, func = ~f(Z, Intensity))

# PRE-DEFINED METRICS
# the stdmetrics*() group of functions contain metrics 
# that summarize the vertical distribution of points, 
# their intensities, and return structure.
cloud_metrics(las, func = .stdmetrics)
grid_metrics(las, func = .stdmetrics)
tree_metrics(las, func = .stdmetrics)
voxel_metrics(las, func = .stdmetrics)

# Calculate Sample L-moments
cloud_metrics(las, func = ~as.list(lmom::samlmu(Z)))
pixel_metrics(las, func = ~as.list(lmom::samlmu(Z)), 10) 

# FRACTAL DIMENSTIONS
#create user-defined function
fd = function(X,Y,Z) {
  M = cbind(X,Y,Z)
  est.boxcount(M)$estdim
}

# Now that we have fd functions which calculates fractal 
# dimensions using XYZ values, we can apply it on our lidar data 
# and create output rasters
cloud_metrics(las, func = ~fd(X,Y,Z), 10)
pixel_metrics(las, func = ~fd(X,Y,Z), 10)
