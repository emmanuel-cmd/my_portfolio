# Let's assume we want to normalize a data set and colorize 
# it using RGB aerial data

# LAScatalog processing engine
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_normalized") # specify output normalized las disk save location
ctg_norm <- normalize_height(ctg, tin()) # normalize

# loop through catalog files and apply rgb colour
rgb <- terra::rast("data/ENGINE/rgb.tif")
for (file in ctg$filename){
  newname <-  tools::file_path_sans_ext(basename(file))
  newname <- paste0("folder/", newname, "_colored.laz")
  las <- readLAS(file)
  las <- merge_spatial(las, rgb)
  writeLAS(las, newname)
}

# What if we were able to do all the above in a single run
# Introducing the catalog_apply()
ctg <- readLAScatalog("data/ENGINE/")
plot(ctg)

# Get RGB image 
rgbmap <- terra::rast("data/ENGINE/rgb.tif")
terra::plotRGB(rgbmap)

# CREATE USER-DEFINED FUNCTION
# We need to create a function that combines the normalization + 
# colorization steps. Let's call it norm_color()
norm_color <- function(las, rgbmap) # create user-defined function
{
  nlas <- normalize_height(las, tin()) # normalize
  colorized <- merge_spatial(nlas, rgbmap) # colorize
  return(colorized) # output
}

# Let's try it on a sample plot
las <- clip_circle(ctg, 338800, 5238500, 40)
nlasrgb <- norm_color(las, rgbmap) # apply user defined function
plot(nlasrgb, color = "RGB", bg = "white", size = 6) # some plotting

# Create an intermediate function for catalog_apply()
new_ctg <- catalog_map(norm_color, rgbmap = rgbmap)

# Create new function norm_color_chunk
# catalog_apply() iterates over each chunks therefore user-defined functions 
# should be written to respect that as below:
norm_color_chunk <- function(chunk, rgbmap) # user defined function
{
  las <- readLAS(chunk)                  # read the chunk
  if (is.empty(las)) return(NULL)        # check if it actually contain points
  nlasrgb <- norm_color(las, rgbmap) # apply computation of interest
  return(nlasrgb) # output
}

# The output will be a point cloud, so we need to pay attention 
# to mitigate memory issues and save to disk
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm_rgb")
options <- list(automerge = TRUE) # merge all the outputs
output <- catalog_apply(ctg, norm_color_chunk, rgbmap = rgbmap, .options = options)
output

# Remove buffer from function
norm_color_chunk <- function(chunk, rgbmap){
  las <- readLAS(chunk)
  if (is.empty(las)) return(NULL)
  nlasrgb <- norm_color(las, rgbmap)
  nlasrgb <- filter_poi(nlasrgb, buffer == 0) # remove buffer
  return(nlasrgb)
}

# Repeat steps
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm_rgb")
options <- list(automerge = TRUE)
output <- catalog_apply(ctg, norm_color_chunk, rgbmap = rgbmap, .options = options)
plot(output)

# We can also introduce paralellization, chunk size control, and buffer size control.
library(future)
plan(multisession)
opt_filter(ctg) <- "-keep_class 2"
opt_chunk_size(ctg) <- 300
opt_chunk_buffer(ctg) <- 40
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm_rgb")
options <- list(automerge = TRUE)
output <- catalog_apply(ctg, norm_color_chunk, rgbmap = rgbmap, .options = options)

# Checking that it worked
opt_output_files(output) <- ""
las <- clip_circle(output, 338800, 5238500, 60)
plot(las, color = "RGB", size = 6, bg = "white")

# Make a user-friendly function for third party users
# Create a generic function
norm_color <- function(las, rgbmap){
  if (is(las, "LAScatalog")) {
    options <- list(automerge = TRUE)
    output <- catalog_apply(ctg, norm_color, rgbmap = rgbmap, .options = options)
    return(output)
  } else if (is(las, "LAScluster")) {
    x <- readLAS(las)
    if (is.empty(x)) return(NULL)
    nlasrgb <- norm_color(x, rgbmap)
    nlasrgb <- filter_poi(nlasrgb, buffer == 0)
    return(nlasrgb)
  } else if (is(las, "LAS")) {
    nlas <- normalize_height(las, tin())
    colorized <- merge_spatial(nlas, rgbmap)
    return(colorized)
  } else {
    stop("Not supported input")
  }
}

# We now have a single function that can be used 
# seamlessly on a LAS or a LAScatalog object.
las_norm_colored <-  norm_color(las, rgbmap)
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm_rgb")
ctg_norm_colored <-  norm_color(ctg, rgbmap)

# Below we prevent the use of the function if no disk path is given, 
# or if the user sets a 0m buffer
norm_color <- function(las, rgbmap)
{
  if (is(las, "LAScatalog")) {
    opt_select(las) <- "*" # disable select tuning
    options <- list(automerge = TRUE, need_output_file = TRUE, need_buffer = TRUE) # require output path & buffer size
    output <- catalog_map(ctg, norm_color, rgbmap = rgbmap, .options = options)
    return(output)
  } else if (is(las, "LAS")) {
    nlas <- normalize_height(las, tin())
    colorized <- merge_spatial(nlas, rgbmap)
    return(colorized)
  } else {
    stop("Not supported input")
  }
}
