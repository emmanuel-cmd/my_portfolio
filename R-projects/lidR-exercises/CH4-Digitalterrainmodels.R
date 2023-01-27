library(terra)
library(rayshader)

# DIGITAL TERRAIN MODELS (DTM)

LASfile <- system.file("extdata", "Topography.laz", package="lidR")
las <- readLAS(LASfile, select = "xyzc")
plot(las, size = 3, bg = "white")

# To generate a DTM model with the TIN algorithm:
dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white") 

# To generate a DTM model with the IDW algorithm:
dtm_idw <- rasterize_terrain(las, algorithm = knnidw(k = 10L, p = 2))
plot_dtm3d(dtm_idw, bg = "white") 

# KRIGING
dtm_kriging <- rasterize_terrain(las, algorithm = kriging(k = 40))
plot_dtm3d(dtm_kriging, bg = "white") 

#> terra version 1.4.22
dtm <- rasterize_terrain(las, algorithm = tin(), pkg ="terra")
dtm_prod <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians")
dtm_hillshade <- terra::shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
plot(raster::raster(dtm_hillshade), col =gray(0:30/30), legend = FALSE)

#> rayshader
dtm <- raster::raster(dtm)
#> Warning in showSRID(SRS_string, format = "PROJ", multiline = "NO", prefer_proj
#> = prefer_proj): Discarded datum NAD83 Canadian Spatial Reference System in Proj4
#> definition
elmat <- raster_to_matrix(dtm)
map <- elmat %>%
  sphere_shade(texture = "imhof1", progbar = FALSE) %>%
  add_water(detect_water(elmat), color = "imhof1") %>%
  add_shadow(ray_shade(elmat, progbar = FALSE), 0.5) %>%
  add_shadow(ambient_shade(elmat, progbar = FALSE), 0)

# Plot map 
plot_map(map)

# Plot 3D version
plot_3d(map, elmat, zscale = 1, windowsize = c(800, 800))
