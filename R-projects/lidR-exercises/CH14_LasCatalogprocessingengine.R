# LAScatalog processing engine
# Rationale for LAScatalog functionality
ctg <- readLAScatalog("data/ENGINE/")
ctg
plot(ctg)

# Validation
# check or inspect LAScatalog objects for file consistency
las_check(ctg)

# Extract Regions of Interest
# clip_* functions allow the extraction of a region of interest from a point cloud
plot <- clip_circle(las, x = 338700, y = 5238650, radius = 15)

# This can be extended to LAScatalog
roi <- clip_circle(ctg, x = 338700, y = 5238650, radius = 40)
plot(roi, bg = "white", size = 4)

# Multiple extractions
# Checking the ROI in several files -  the output returned is a list of LAS objects
x <- c(339348.8, 339141.9, 338579.6, 338520.8, 338110.0, 339385)
y <- c(5239254, 5238717, 5238169, 5239318, 5239247, 5239290)
r <- 40
plot(ctg)
points(x, y)

# Searches the repective files for the ROI
rois <- clip_circle(ctg, x, y, r)
rois[1:2]

# MODIFICATION OF DEFAULT BEHAVIOUR
# The default behavior of the engine is set in such a way that it returns what is most likely to be expected by the users.
# However the behavior of the engine can be tuned to optimize processing.

# Multiple extractions on disk
opt_output_files(ctg) <- paste0(tempdir(), "/{XCENTER}_{YCENTER}_{ID}")
rois <- clip_circle(ctg, x, y, r)
rois
plot(rois)

# Get directory where files have been saved
rois$filename

# GROUND CLASSIFICATION
# Chunk processing

# we first specify where to write the outputs using opt_output_files(). The files written on disk will be LAS files, 
# while the output in R will be a LAScatalog. If we don’t write to disk, the result of each chunk will be stored into 
# memory, potentially leading to memory issues.
opt_output_files(ctg) <- paste0(tempdir(), "{*}_classified")
classified_ctg <- classify_ground(ctg, csf())

# File paths should always be specified for every LAScatalog functions to run without errors
opt_output_files(ctg) <- ""
classified_ctg <- classify_ground(ctg, csf())
#> Error: This function requires that the LAScatalog provides an output file template.

# Modifying buffers
opt_chunk_buffer(ctg) <- 20
plot(ctg, chunk = TRUE)

opt_chunk_buffer(ctg) <- 50
plot(ctg, chunk = TRUE)

# Modifying the chunk size
# Process sequentially tiny 250 x 250 chunks with 10 m buffer
opt_chunk_size(ctg) <- 250
opt_chunk_buffer(ctg) <- 10
plot(ctg, chunk = TRUE)

# Process sequentially bigger 800 x 800 chunks with 40 m buffer
opt_chunk_size(ctg) <- 800
opt_chunk_buffer(ctg) <- 40
plot(ctg, chunk = TRUE)

# Parallel processing
# In the following example we load future and register a 
# parallelization plan to enable parallelized classification.
library(future)
plan(multisession)
opt_chunk_size(ctg) <- 400
opt_chunk_buffer(ctg) <- 40
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_classified")
classified_ctg <- classify_ground(ctg, csf())

# DIGITAL TERRAIN MODEL
# The result is a collection of rasters, however one feature 
# of the engine is to merge everything in single, seamless, manipulable object.
dtm <- rasterize_terrain(ctg, 2, tin(), pkg = "terra")

# We can render a shaded DTM like that from section 4.6 to better visualize the output:
dtm_prod <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians")
dtm_hillshade <- terra::shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect)
plot(dtm_hillshade, col = gray(0:50/50), legend = FALSE)

# On disk DTM
opt_output_files(ctg) <- opt_output_files(ctg) <- paste0(tempdir(), "/{*}_dtm")
dtm <- rasterize_terrain(ctg, 1, tin())
dtm

# Height normalization
# We can use the dtm raster created above
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm")
ctg_norm <- normalize_height(ctg, dtm)

# A point-cloud normalization without a raster is also possible
opt_output_files(ctg) <-  paste0(tempdir(), "/{*}_norm")
ctg_norm <- normalize_height(ctg, tin())

# Area Based Approach
# Two more options of the engine do exist that are not controllable in every function
opt_select(ctg) <- "xyzci"
opt_filter(ctg) <- "-keep_first"

# These enable computation on a selected set of points loading only user specified attributes
# This is useful mainly to save processing memory but may also have a small favorable impact on computation time.
opt_select(ctg_norm) <- "xyz"
hmean <- pixel_metrics(ctg_norm, ~mean(Z), 10)

# Using two set of filters
opt_select(ctg_norm) <- "xyz"
opt_filter(ctg_norm) <- "-keep_first"
hmean <- pixel_metrics(ctg_norm, ~mean(Z), 10)

# Plot hmean
plot(raster::raster(hmean), col = height.colors(25))

# INDIVIDUAL TREE DETECTION
# A total of 100000 trees have been found in this example
ttops <- locate_trees(ctg_norm, lmf(4))
ttops
plot(ttops["Z"], cex = 0.001, pch = 19, pal = height.colors, nbreaks = 30)

# Downside of this approach: Impossible to achieve the same task if each chunk is written in a file, 
# and it is almost impossible to achieve in the case of individual tree segmentation 
ttops$treeID <- 1:nrow(ttops)

# To solve this lidR has 2 strategies to generate reproducible unique IDs
# no matter the processing order or the number of trees found
ttops <- locate_trees(ctg_norm, lmf(4), uniqueness = "bitmerge")
plot(ttops["treeID"], cex = 0.01, pch = 19)

# INDIVIDUAL TREE SEGMENTATION
# First we create a 1 m resolution CHM stored on disk and returned as a light virtual raster.
opt_output_files(ctg_norm) <- paste0(tempdir(), "/chm_{*}")
chm <- rasterize_canopy(ctg_norm, 1, p2r(0.15))
plot(chm, col = height.colors(50))

# Second we compute the seeds by locating the trees.
# The result must be loaded in memory because there is no way to combine many vectors stored on disk like rasters
opt_output_files(ctg_norm) <- ""
ttops <- locate_trees(ctg_norm, lmf(4), uniqueness = "bitmerge")

# We apply segment_trees. 
opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_segmented")
algo <- dalponte2016(chm, ttops)
ctg_segmented <- segment_trees(ctg_norm, algo)

# The output ctg_segmented is a LAScatalog where each point is labelled with an ID that is unique for each tree
opt_output_files(ctg_segmented) <- ""
lasplot <- clip_circle(ctg_segmented, 338500, 5238600, 40)
pol = crown_metrics(lasplot, NULL, geom = "convex")

# Plot shows that the segmentation is perfect with respect to the labeling problem. 
# Trees that were segmented 2ce independently on each edge were attributed the same IDs oon both sides and the final output is wall-to-wall
plot(sf::st_geometry(pol), col = pastel.colors(250), axes = T)
plot(ctg, add = T)

# RETILE A CATALOG
# Retile a catalog into smaller files
# catalog_retile() allows for retiling the acquisition of files in tiles of any size.
opt_output_files(ctg) <- paste0(tempdir(), "/{XLEFT}_{YBOTTOM}") # label outputs based on coordinates
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 250 # retile to 250 m
small <- catalog_retile(ctg) # apply retile
plot(small) # some plotting

# Add a buffer around each file
opt_chunk_buffer(ctg) <- 20 # set buffer to 20
opt_chunk_size(ctg) <- 0 # no change to chunk size
opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_buffered") # name outputs with original name and "_buffered"
buffered <- catalog_retile(ctg) # apply buffer
plot(buffered) # some plotting

# Create a new collection with only first returns
opt_chunk_buffer(ctg) <- 0
opt_chunk_size(ctg) <- 0
opt_output_files(ctg) <- paste0(tempdir(), "/{ORIGINALFILENAME}_first")
opt_filter(ctg) <- "-keep_first"
first <- catalog_retile(ctg)

# Create a new collection of small and buffered 
# ground returns in parallel
library(future)
plan(multisession)

opt_output_files(ctg) <- paste0(tempdir(), "/{XLEFT}_{YBOTTOM}_first_buffered")
opt_chunk_buffer(ctg) <- 10
opt_chunk_size(ctg) <- 250
opt_filter(ctg) <- "-keep_class 2"
newctg <- catalog_retile(ctg)
plot(newctg)

# THE CASE OF GROUND INVENTORIES
opt_chunk_size(rois) <- 0 # processing by files
opt_chunk_buffer(rois) <- 0 # no buffer
opt_wall_to_wall(rois) <- FALSE # disable internal checks to ensure a valid output. Free wheel mode
opt_merge(rois) <- FALSE
opt_output_files(rois) <- ""
opt_independent_files(rois) <- TRUE

#
dtm <- rasterize_terrain(rois, 1, tin())
plot(raster::raster(dtm)[[1]], col = gray(1:50/50))

# One can use summary to display the main current processing options of the catalog
summary(ctg)
