
library(AeRobiology)
library(lidR)

# Applications
# Derived metric at the voxel level
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile) # read file
vox_met <- voxel_metrics(las, ~list(N = length(Z)), 4) # calculate voxel metrics

# We can count the number of points in each 4 x 4 x 4m voxel:
plot(vox_met, color="N", pal = heat.colors(50), size = 4, bg = "white", voxel = TRUE)

# One can define custom_metrics
custom_metrics <- function(x) { # user-defined function
  m <- list(
    i_min = min(x),
    i_mean = mean(x),
    i_max = max(x),
    i_sd = sd(x)
  )
  return(m) # output
}

vox_met <- voxel_metrics(las, ~custom_metrics(Intensity), 4) # calculate voxel metrics
