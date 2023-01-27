# Derived metrics at the pixel level
# Overview
hmean <- pixel_metrics(las, ~mean(Z), 10) # calculate mean at 10 m x 10 m pixels
plot(raster(hmean), col = height.colors(50))

# To calculate multiple metrics at a time a custom function needs to be created first
f <- function(x) { # user-defined fucntion
  list(mean = mean(x), sd = sd(x))
}
metrics <- pixel_metrics(las, ~f(Z), 10) # calculate grid metrics
plot(stack(metrics), col = height.colors(50))

# To compute the predefined list of 56 metrics in the .stdmetrics 
# we can run the pixel_metrics() function as follows:
metrics <- pixel_metrics(las, .stdmetrics, 10) # calculate standard metrics
plot(stack(metrics), col = height.colors(50))

# We demonstrate how the coefficient of variation 
# and inter-quartile range can be calculated:
metrics_custom <- function(z) { # user defined function
  list(
    coef_var <- sd(z) / mean(z) * 100, # coefficient of variation
    iqr <- IQR(z)) # inter-quartile range
}
metrics <- pixel_metrics(las, ~metrics_custom(z=Z), 10) # calculate grid metrics
plot(metrics, col = height.colors(25))

# APPLICATIONS
# Modeling
prediction <- pixel_metrics(las, ~0.7018 * sum(Z > 2)/length(Z) + 0.9268 *max(Z), 20) # predicting model mapping
plot(prediction, col = height.colors(50)) # some plotting

# Density
# Point density is the number of points within a pixel divided by the area of the pixel
density <- pixel_metrics(las, ~length(Z)/16, 4) # calculate density
plot(raster(density), col = gray.colors(50,0,1)) # some plotting

# When using only the first returns, the same formula gives the pulse density instead of the point density
density <- pixel_metrics(las, ~length(Z)/16, 4, filter = ~ReturnNumber == 1L)

# INTENSITY
# It is possible to generate a map of the avg intensity of first return only
imap <- pixel_metrics(las, ~mean(Intensity), 4, filter = ~ReturnNumber == 1L) # mapping average intensity
plot(raster(imap), col = heat.colors(25))

# To count single returns we can count the numner of points where number of returns equal to 1
# To count the number of multiple returns we can count the number of points with a return number equal to 1 AND a return number above 1.
mymetric <- function(return_number, number_of_returns) { #user-defined function
  nsingle <- sum(number_of_returns == 1L)
  nmultiple <- sum(return_number == 1L & number_of_returns > 1L)
  return(list(n_single = nsingle,
              n_multiple = nmultiple,
              ratio = nmultiple/nsingle))
}

rmap <- pixel_metrics(las, ~mymetric(ReturnNumber, NumberOfReturns), 8) # mapping retunrs
plot(raster(rmap), col = viridis::viridis(50))
