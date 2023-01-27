# INDIVIDUAL TREE SEGMENTATION AND DETECTION

# Tree tops are detected using the locate_trees() function, 
# followed by crown delineation using segment_trees()
LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzr", filter = "-drop_z_below 0")
chm <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2))
plot(las, bg = "white", size = 4)

# Local maximum filter with window size = 5
ttops <- locate_trees(las, lmf(ws = 5))

# While the algorithm does not need any CHM to work 
# we chose to display the results on top of a CHM for better visualization.
plot(raster::raster(chm), col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

#Tree detection results can also be visualized in 3D
x <- plot(las, bg = "white", size = 4)
add_treetops3d(x, ttops)

# Small windows sizes produce more trees than large window sizes 
ttops_3m <- locate_trees(las, lmf(ws = 3))
ttops_11m <- locate_trees(las, lmf(ws = 11))

par(mfrow=c(1,2))
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops_3m), add = TRUE, pch = 3)
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops_11m), add = TRUE, pch = 3)

# Local Maximum Filter with variable windows size

# Designing a window size as a function of point (or pixel) height
f <- function(x) {x * 0.1 + 3}
heights <- seq(0,30,5)
ws <- f(heights)
plot(heights, ws, type = "l", ylim = c(0,6))

# When applied within lmf():
ttops <- locate_trees(las, lmf(f))

plot(raster(chm), col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

# There is no intrinsic limitation to the user-defined function
# One does however need to pay attention to special cases.
# A more robust function with some built-in thresholds:
f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

heights <- seq(-5,30,0.5)
ws <- f(heights)
plot(heights, ws, type = "l",  ylim = c(0,5))

# LOCAL MAXIMUM FILTER ON A CHM

# In the examples below tree detection on CHMs generated with different 
# algorithms, resolutions and different post-processing smooting steps with 
# a median filter applied. 

# First the different CHM are generated:
# Point-to-raster 2 resolutions
chm_p2r_05 <- rasterize_canopy(las, 0.5, p2r(subcircle = 0.2), pkg = "terra")
chm_p2r_1 <- rasterize_canopy(las, 1, p2r(subcircle = 0.2), pkg = "terra")

# Pitfree with and without subcircle tweak
chm_pitfree_05_1 <- rasterize_canopy(las, 0.5, pitfree(), pkg = "terra")
chm_pitfree_05_2 <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2), pkg = "terra")

# Post-processing median filter
kernel <- matrix(1,3,3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
chm_p2r_1_smoothed <- terra::focal(chm_p2r_1, w = kernel, fun = median, na.rm = TRUE)

# Then the same tree detection with a constant windows size of 5m is applied to each CHM
ttops_chm_p2r_05 <- locate_trees(chm_p2r_05, lmf(5))
ttops_chm_p2r_1 <- locate_trees(chm_p2r_1, lmf(5))
ttops_chm_pitfree_05_1 <- locate_trees(chm_pitfree_05_1, lmf(5))
ttops_chm_pitfree_05_2 <- locate_trees(chm_pitfree_05_2, lmf(5))
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))
ttops_chm_p2r_1_smoothed <- locate_trees(chm_p2r_1_smoothed, lmf(5))

# The results generated are visualized:
par(mfrow=c(3,2))
col <- height.colors(50)
plot(chm_p2r_05, main = "CHM P2R 0.5", col = col); plot(sf::st_geometry(ttops_chm_p2r_05), add = T, pch =3)
plot(chm_p2r_1, main = "CHM P2R 1", col = col); plot(sf::st_geometry(ttops_chm_p2r_1), add = T, pch = 3)
plot(chm_p2r_05_smoothed, main = "CHM P2R 0.5 smoothed", col = col); plot(sf::st_geometry(ttops_chm_p2r_05_smoothed), add = T, pch =3)
plot(chm_p2r_1_smoothed, main = "CHM P2R 1 smoothed", col = col); plot(sf::st_geometry(ttops_chm_p2r_1_smoothed), add = T, pch =3)
plot(chm_pitfree_05_1, main = "CHM PITFREE 1", col = col); plot(sf::st_geometry(ttops_chm_pitfree_05_1), add = T, pch =3)
plot(chm_pitfree_05_2, main = "CHM PITFREE 2", col = col); plot(sf::st_geometry(ttops_chm_pitfree_05_2), add = T, pch =3)

# INDIVIDUAL TREE SEGMENTATION
# Below we use the dalponte2016() algorithm which is best suited to this example data
algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
las <- segment_trees(las, algo) # segment point cloud
plot(las, bg = "white", size = 4, color = "treeID") # visualize trees

# An ID is assigne to each point because it enables interesting analyses at the point-cloud level
tree110 <- filter_poi(las, treeID == 110)
plot(tree110, size = 8, bg = "white")

# Delineation of crown shapes
crowns <- crown_metrics(las, func = .stdtreemetrics, geom = "convex")
plot(crowns["convhull_area"], main = "Crown area (convex hull)")

# Segmentation of the CHM
algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
crowns <- algo()

# Plot - the output is a raster layer!
plot(crowns, col = pastel.colors(200))

# cOMPARISON OF TREE SEGMENTATIONS
algo1 <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
algo2 <- li2012()
las <- segment_trees(las, algo1, attribute = "IDdalponte")
las <- segment_trees(las, algo2, attribute = "IDli")

x <- plot(las, bg = "white", size = 4, color = "IDdalponte", colorPalette = pastel.colors(200))
plot(las, add = x + c(100,0), bg = "white", size = 4, color = "IDli", colorPalette = pastel.colors(200))

# By comparing crowns with crown_metrics() we can 
# more easily see that li2012 alg doesn't perform well in this example
# with default parameters -  maybe some parameter tuning can
# lead to better results!
crowns_dalponte <- crown_metrics(las, func = NULL, attribute = "IDdalponte", geom = "concave")
crowns_li <- crown_metrics(las, func = NULL, attribute = "IDli", geom = "concave")

# Visualizations
par(mfrow=c(1,2),mar=rep(0,4))
plot(sf::st_geometry(crowns_dalponte), reset = FALSE)
plot(sf::st_geometry(crowns_li), reset = FALSE)

