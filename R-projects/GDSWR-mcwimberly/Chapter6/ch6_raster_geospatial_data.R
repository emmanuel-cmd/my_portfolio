# RASTER GEOSPATIAL DATA - CONTINOUS

# Load packages
library(sf)           
library(terra)          
library(ggplot2)      

# Importing Raster Data
lst <- rast("MOD11A2_2017-07-12.LST_Day_1km.tif")
class(lst)
lst

# summary() to provide info about the statistical distribution of raster values
# with the size argument specifying the number of randomly sampled pixels to include in the summary
summary(lst, size = 1e6)

ncell(lst) # number of cells
nrow(lst) # no of grid rows
ncol(lst) # no of grid columns
nlyr(lst) # no of raster layers
res(lst) # returns the grid cell size

# the ext() returns a SpatExtent object that contains the geographic coordinates of the raster extent
lst_ext <- ext(lst)
class(lst_ext)

lst_ext[1:4]

# To get CRS of the raster data
crs(lst, parse = TRUE) # Outout is in well-known text format. Parse output to a vector that is easier to read when printed to the console

# describe returns an abbreviated summary including CRS name n EPSG code
crs(lst, describe = TRUE)

names(lst) <- c("temperature")
names(lst)

# SpatRaster objects can be modified with various functions n used as input to math exp
# Raw digital numbers must be modified by a scaling factor of 0.02 to convert them to Kelvin
lst <- ifel(lst == 0, NA, lst) # Replace zero with NAs
lst_c <- lst * 0.02 - 273.15   # convert to degree celsius
summary(lst_c, size = 1e6)

# the global() can be used to generate statistical summaries of the pixels in a raster data
global(lst_c, fun = "mean", na.rm=T)
global(lst_c, fun = "min", na.rm=T)
global(lst_c, fun = "max", na.rm=T)
global(lst_c, fun = "sd", na.rm=T)

# Raster obj can be exported using writeRaster
writeRaster(lst_c, 
            filename = "MOD11A2_2017-07-12.LST_Day_1km_DegC.tif", 
            filetype="GTiff", overwrite=TRUE)

# Maps of Raster Data
# Converting raster to dataframe using a custom function
rasterdf <- function(x, aggregate = 1) {
  resampleFactor <- aggregate        
  inputRaster <- x    
  inCols <- ncol(inputRaster)
  inRows <- nrow(inputRaster)
  # Compute numbers of columns and rows in the resampled raster
  resampledRaster <- rast(ncol=(inCols / resampleFactor), 
                          nrow=(inRows / resampleFactor),
                          crs = crs(inputRaster))
  # Match to the extent of the original raster
  ext(resampledRaster) <- ext(inputRaster)
  # Resample data on the new raster
  y <- resample(inputRaster,resampledRaster,method='near')
  # Extract cell coordinates into a data frame
  coords <- xyFromCell(y, seq_len(ncell(y)))
  # Extract layer names
  dat <- stack(values(y, dataframe = TRUE))
  # Add names - 'value' for data, 'variable' for different
  # layer names in a multilayer raster
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  dat
}

# The ffg code converts the LST raster to a df after aggregating the cell values by a factor of 3
lst_df <- rasterdf(lst_c, aggregate = 3)
summary(lst_df)

# MODIS Land surface temperature map
# the cood_sf(expand=F) to eliminate extra space at the edges of the map
ggplot(data = lst_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Degrees C", 
                      low = "yellow", 
                      high = "red") +
  coord_sf(expand = FALSE) +
  labs(title = "LST-Adjusted Temperature (Terra Day)",
       x = "longitude", 
       y = "latitude") +
  theme(legend.position = "bottom")

# To clip out a smaller portion of the LST data  
clipext <- ext(-86, -80.5, 30, 35.5)  # bounding box
class(clipext)

# Crop the raster to the US state of Georgia
lst_clip <- crop(lst_c, clipext)
lst_clip_df <- rasterdf(lst_clip)

# Often it is helpful to plot boundary features on top of a raster to provide context
# Vector n raster data can be combined in ggplot
ga_sf <- st_read(dsn = "GA_SHP.shp", quiet = TRUE)

# Overlay Georgia county boundaries on the LST raster
ggplot() +  
  geom_raster(data = lst_clip_df, 
              aes(x = x, 
                  y = y, 
                  fill = value)) +
  geom_sf(data=ga_sf, 
          color = "grey50", 
          fill = NA, size = 0.5) +
  scale_fill_gradient(name = "Degrees C", 
                      low = "yellow", 
                      high = "red") +
  coord_sf(expand = FALSE) +
  labs(title = "LST-Adjusted Temperature (Terra Day)",
       x = "longitude", 
       y = "latitude")

# MULTILAYER RASTERS
# Here we use forcings data from the North America Land Data Assimilation System NLDAS
# The data has a grid cell size of 0.125x0.125 and are stored as GEOTIFF files 

temp2012 <- rast("NLDAS_FORA0125_M.A201207.002.grb.temp.tif")
temp2013 <- rast("NLDAS_FORA0125_M.A201307.002.grb.temp.tif")
temp2014 <- rast("NLDAS_FORA0125_M.A201407.002.grb.temp.tif")
temp2015 <- rast("NLDAS_FORA0125_M.A201507.002.grb.temp.tif")
temp2016 <- rast("NLDAS_FORA0125_M.A201607.002.grb.temp.tif")
temp2017 <- rast("NLDAS_FORA0125_M.A201707.002.grb.temp.tif")

# Producing a stack of SpatRaster objects

# Temp data from 2012 to 2017
tempstack <- c(temp2012, temp2013, temp2014, 
               temp2015, temp2016, temp2017)
names(tempstack) <- c("July 2012", "July 2013", "July 2014", 
                      "July 2015", "July 2016", "July 2017")

# Percip data from 2012 to 2017

# Alternative way to create a multi-layer raster
tempstack <- rast(c("NLDAS_FORA0125_M.A201207.002.grb.temp.tif",
                    "NLDAS_FORA0125_M.A201307.002.grb.temp.tif",
                    "NLDAS_FORA0125_M.A201407.002.grb.temp.tif",
                    "NLDAS_FORA0125_M.A201507.002.grb.temp.tif",
                    "NLDAS_FORA0125_M.A201607.002.grb.temp.tif",
                    "NLDAS_FORA0125_M.A201707.002.grb.temp.tif"))
names(tempstack) <- c("July 2012", "July 2013", "July 2014", 
                      "July 2015", "July 2016", "July 2017")

# Using the global function on multilayer rasters, each layer is summarized individually
global(tempstack, stat = "mean", na.rm=T)

# From these summaries, these data are in Kelvin
# Let's convert them to Celsius
tempstack <- tempstack - 273.15
global(tempstack, stat = "mean", na.rm=T)

# We can map the multilayer raster using ggplot
# First the SpatRaster object is converted to a df using custom rasterdf function
tempstack_df <- rasterdf(tempstack)
summary(tempstack_df)

# A dataset of US state boundaries is imported for use as an overlay
states_sf <- read_sf("conterminous_us_states.shp", quiet = TRUE)

# Using the facet_wrap() to display a composite plot with one year mapped in each facet
ggplot() +
  geom_raster(data = tempstack_df, 
              aes(x = x, 
                  y = y, 
                  fill = value)) +
  geom_sf(data = states_sf, 
          fill = NA,
          color = "grey50", 
          size = 0.25) +
  scale_fill_gradient(name = "Degrees C", 
                      low = "yellow", 
                      high = "red") +
  coord_sf(expand = FALSE) +
  facet_wrap(facets = vars(variable), ncol = 2) + 
  labs(title = "Mean July Temperature",
       x = "longitude", 
       y = "latitude")


# Computations on Raster Objects
# When used with multi-layer rasters, these functions will be applied to each pixel to summarize the data across all layers.
# Note the difference compared to the summaries calculated with global(), which were summarized across all pixels for each 
# layer instead of across all layers for each pixel.
meantemp <- mean(tempstack)

# Convert to dataframe obj
meantemp_df <- rasterdf(meantemp)

# Plot 
ggplot() +
  geom_raster(data = meantemp_df, aes(x = x, 
                                      y = y, 
                                      fill = value)) +
  geom_sf(data = states_sf, 
          fill = NA, 
          color = "grey50", 
          size = 0.25) +
  scale_fill_gradient(name = "Degrees C", 
                      low = "yellow", 
                      high = "red") +
  coord_sf(expand = FALSE) +
  labs(title = "Mean July Temperature, 2012-2017",
       x = "longitude", y = "latitude") +
  theme(legend.position = "bottom")

# Substract mean temp to create a layer of temp anomaly
tempanom <- tempstack - meantemp
names(tempanom) <- names(tempstack)

# The anomalies are displayed using a bicolor ramp generated with the scale_fill_gradients2
tempanom_df <- rasterdf(tempanom)


# Positive anomalies - red, negative anomalies - blue

# The untransformed temperature data show the same strong spatial patterns every year. 
# By subtracting the mean temperature map from each annual temperature map, the spatial pattern of temperature 
# is removed and the interannual variation is emphasized in the anomaly maps.
ggplot() +
  geom_raster(data = tempanom_df, aes(x = x, 
                                      y = y, 
                                      fill = value)) +
  geom_sf(data = states_sf, 
          fill = NA,
          color = "grey50", 
          size = 0.25) +
  scale_fill_gradient2(name = "Degrees C", 
                       low = "blue", 
                       mid = "lightyellow", 
                       high = "red") +
  coord_sf(expand = TRUE) +
  facet_wrap(facets = vars(variable), ncol = 2) + 
  theme_void() +
  theme(strip.text.x = element_text(size=12, face="bold"))

# One or more layers can be extracted from a raster obj using double brackets 
# As with lists in base R, raster layers can be extracted either by number or name
temp1 <- tempstack[[1]]
names(temp1)

temp2 <- tempstack[["July 2012"]]
names(temp2)

temp3 <- tempstack[[1:3]]
names(temp3)

# The terra pkg has methods for many of the base R functions
plot(tempstack[[1]])
plot(tempstack)
hist(tempstack)

# When two single-layer rasters are provided as arguments to the plot function, it will
# generate a scatterplot
plot(
  temp2012, 
  temp2013, 
  xlab = "2012", 
  ylab = "2013", 
  gridded = T   # Setting this produces a gridded scatterplot in which the value of each 
  # grid cell represents the density of points in that portion of the scatterplot
)

# PRACTICE
# 1. Convert the clipped LST raster from Celsius to Fahrenheit and generate an updated version of the Georgia LST map
lst_clip_F = lst_clip + 32
lst_clip_Fdf = rasterdf(lst_clip_F)

# Generate an updated version of the Georgia map
ggplot() +  
  geom_raster(data = lst_clip_Fdf, 
              aes(x = x, 
                  y = y, 
                  fill = value)) +
  geom_sf(data=ga_sf, 
          color = "grey50", 
          fill = NA, size = 0.5) +
  scale_fill_gradient(name = "Degrees F", 
                      low = "yellow", 
                      high = "red") +
  coord_sf(expand = FALSE) +
  labs(title = "LST-Adjusted Temperature°F (Terra Day)",
       x = "longitude", 
       y = "latitude")

# 2. Generate a new raster where each pixel contains the standard deviation of the NLDAS July Temperature values 
# from 2012-2017. Map the result to explore where temperatures were most variable over this period. 
sdtemp = terra::app(tempstack, fun = sd)

# Convert to dataframe obj
sdtemp_df <- rasterdf(sdtemp)

# Plot 
ggplot() +
  geom_raster(data = sdtemp_df, aes(x = x, 
                                    y = y, 
                                    fill = value)) +
  geom_sf(data = states_sf, 
          fill = NA, 
          color = "grey50", 
          size = 0.25) +
  scale_fill_gradient(name = "Degrees C", 
                      low = "yellow", 
                      high = "red") +
  coord_sf(expand = FALSE) +
  labs(title = "Standard deviation July Temperature, 2012-2017",
       x = "longitude", y = "latitude") +
  theme(legend.position = "bottom")

# 3. Create a new raster dataset where each cell contains the difference between July 2015 temperature and July 2016 temperature. 
# Examine the map and histogram of this raster and use them to assess changes in temperature between 2015 and 2016.
tempdiff1516 = tempstack[["July 2015"]] - tempstack[["July 2016"]]
hist(tempdiff1516)

# Convert to dataframe
tempdiff1516_df <- rasterdf(tempdiff1516)

# plot
ggplot() +
  geom_raster(data = tempdiff1516_df, 
              aes(x = x, 
                  y = y, 
                  fill = value)) +
  geom_sf(data = states_sf, 
          fill = NA,
          color = "grey50", 
          size = 0.25) +
  scale_fill_gradient2(name = "Degrees C", 
                       low = "blue", 
                       mid = "lightyellow", 
                       high = "red") +
  coord_sf(expand = FALSE) +
  labs(title = "July Temperature difference 2015-16",
       x = "longitude", 
       y = "latitude")
