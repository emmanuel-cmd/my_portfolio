# GROUND POINT CLASSIFICATION
# Progressive Morphological Filter
LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzrn")
las <- classify_ground(las, algorithm = pmf(ws = 5, th = 3))

# Visulaize the result
plot(las, color = "Classification", size = 3, bg = "white") 

# To better illustrate the classification we can generate and 
# plot a cross section
p1 <- c(273420, 5274455)
p2 <- c(273570, 5274460)
plot_crossection(las, p1 , p2, colour_by = factor(Classification))

# We use multiple parameters for 2parameters instead of a single value
ws <- seq(3, 12, 3)
th <- seq(0.1, 1.5, length.out = length(ws))
las <- classify_ground(las, algorithm = pmf(ws = ws, th = th))

# Points in the canopy are no longer classified as ground
plot_crossection(las, p1 = p1, p2 = p2, colour_by = factor(Classification))

# CLOTH SIMULATION FUNCTION
# The csf function use the default values proposed by Zhang et al 2016
las <- classify_ground(las, algorithm = csf())

# Classifcation result can be accessed using a cross section
plot_crossection(las, p1 = p1, p2 = p2, colour_by = factor(Classification))

# Parameter tuning to improve cloth simulation results
mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1, cloth_resolution = 1, time_step = 1)
las <- classify_ground(las, mycsf)
plot_crossection(las, p1 = p1, p2 = p2, colour_by = factor(Classification))

# We can also subset only the ground points to display the results in 3D
gnd <- filter_ground(las)
plot(gnd, size = 3, bg = "white") 

# MULTISCALE CURVATURE CLASSIFICATION (MCC)
las <- classify_ground(las, mcc(1.5,0.3))
plot_crossection(las, p1 = p1, p2 = p2, colour_by = factor(Classification))
