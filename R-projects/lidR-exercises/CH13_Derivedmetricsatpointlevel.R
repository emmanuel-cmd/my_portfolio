# Derived metrics at point level
library(lidR)
# Point-based metrics
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las <- readLAS(LASfile) # read file
metrics <- point_metrics(las, ~list(imean = mean(Intensity)), k = 7)        # 1
metrics <- point_metrics(las, ~list(imean = mean(Intensity)), r = 3)        # 2
metrics <- point_metrics(las, ~list(imean = mean(Intensity)), k = 7, r = 3) # 3

# Instead of the ID, one may prefer to get the coordinates
metrics <- point_metrics(las, ~list(imean = mean(Intensity)), k = 7, xyz = TRUE)

# Selecting and filtering points at the same time
metrics <- point_metrics(las, ~list(imean = mean(Intensity)), k = 7, filter = ~Intensity < 100 ) # calculate mean intensity and exclude outliers
head(metrics)

# Applications
las <- readLAS(system.file("extdata", "building_WilliamsAZ_Urban_normalized.laz", package="lidR"))
plot(las, size = 3)

# Define a function that computes the eigenvalue decomposition 
# of a series of points and estimates if the set of points is flat 
is.planar <- function(x, y, z, th1 = 25, th2 = 6) {
  xyz <- cbind(x,y,z)
  cov_m <- cov(xyz)
  eigen_m <- eigen(cov_m)$value
  is_planar <- eigen_m[2] > (th1*eigen_m[3]) && (th2*eigen_m[2]) > eigen_m[1]
  return(list(planar = is_planar))
}

# We then apply the function not considering ground points
M <- point_metrics(las, ~is.planar(X,Y,Z), k = 20, filter = ~Classification != LASGROUND)

# We finally merge the output with the point cloud to visualize the result:
las <- add_attribute(las, FALSE, "planar")
las$planar[M$pointID] <- M$planar
plot(las, color = "planar")

# We can eventually set a valid classification to those points
las$Classification[M$pointID] <- LASBUILDING

# since is.planar() is very slow, we can write a c++ function as a replacement
Rcpp::sourceCpp(code = "
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec eigen_values(arma::mat A) {
arma::mat coeff, score;
arma::vec latent;
arma::princomp(coeff, score, latent, A);
return(latent);
}")

is.planar <- function(x, y, z, th1 = 25, th2 = 6) {
  xyz <- cbind(x,y,z)
  eigen_m <- eigen_values(xyz)
  is_planar <- eigen_m[2] > (th1*eigen_m[3]) && (th2*eigen_m[2]) > eigen_m[1]
  return(list(planar = is_planar))
}

# An alternative R function is available in lidR
las <- segment_shapes(las, shp_plane(k = 25), "planar")

# Lake and wire segmentation
is.linear <- function(x, y, z, th = 10) {
  xyz <- cbind(x,y,z)
  eigen_m <- eigen_values(xyz)
  is_linear <-  th*eigen_m[3] < eigen_m[1] && th*eigen_m[2] < eigen_m[1]
  return(list(linear = is_linear))
}

# lidR has dedicated function for wire segmentation
las <- segment_shapes(las, shp_line(th = 8, k = 15), "linear")
plot(las, color="linear")

# lidR has dedicated function for lake segmentation
las <- segment_shapes(las, shp_line(th = 10, k = 15), "linear")
plot(las, color="linear")

# MULTI SPECTRAL COLORING
f1 <- "PR1107_c1_ar_c.laz"
f2 <- "PR1107_c2_ar_c.laz"
f3 <- "PR1107_c3_ar_c.laz"
f1<-system.file("extdata", "PR1107_c1_ar_c.laz", package="lidR")
f2=system.file("extdata", "PR1107_c2_ar_c.laz", package="lidR")
f3=system.file("extdata", "PR1107_c3_ar_c.laz", package="lidR")

#
las <- readMSLAS(f1, f2, f3,  filter = "-keep_z_below 300")
plot(las, color = "ScannerChannel", size = 5)

#
set.color <- function(intensity, channel)
{
  # Split the intensities of each channel
  i1 <- intensity[channel == 1]
  i2 <- intensity[channel == 2]
  i3 <- intensity[channel == 3]
  
  # If one channel is missing return RGB = NA
  if (length(i1) == 0 | length(i2) == 0 | length(i3) == 0)
    return(list(R = NA_integer_, G = NA_integer_, B = NA_integer_))
  
  # Average and normalise the intensities
  i1 <- as.integer(mean(i1))
  i2 <- as.integer(mean(i2))
  i3 <- as.integer(mean(i3))
  if (i1 > 255L) i1 <- 255L
  if (i2 > 255L) i2 <- 255L
  if (i3 > 255L) i3 <- 255L
  
  return(list(R = i1, G = i2, B = i3))
}

# We can apply this function using point_metrics using a spherical neighbour
M <- point_metrics(las, ~set.color(Intensity, ScannerChannel), r = 0.5)
las <- add_lasrgb(las, M$R, M$G, M$B)
colored <- filter_poi(las, !is.na(R)) # remove RGB = NA
plot(colored, color = "RGB", size = 3)
