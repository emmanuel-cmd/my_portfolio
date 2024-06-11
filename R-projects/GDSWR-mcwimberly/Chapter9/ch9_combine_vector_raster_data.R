# COMBINING VECTOR DATA WITH CONTINOUS RASTER DATA
library(tidyverse)
library(sf)
library(terra)
library(prism) # widely used interpolated climate data that is based on meteorological stations combined with elevation data
library(tigris)

# ACCESSING DATA WITH R PACKAGES
county <- tigris::counties(cb = TRUE,      # to indicate that data with generalized county boundaries will be downloaded
                   resolution = '20m') # to indicate the scale (1:20million) of the data
class(county)
glimpse(county)

# Convert columns to numeric and filter
county <- county %>%
  mutate(state = as.numeric(STATEFP),  # convert to num to make it easier to join with the zonal summary table later on
         fips = as.numeric(GEOID)) %>%
  filter(state != 2, state != 15, state < 60) # filter down to only the 48 conterminous US

# It is important to know the CRS of the data
writeLines(st_crs(county)$WktPretty) # data is in a geographic coordinate system

# The prism pkg downloads and imports PRISM climate data
options(prism.path = ".")        #specify the directory to download the data, here the cwd
get_prism_normals(type = 'ppt',         # ppt = precipitation data
                  resolution = '4km',  # either 4km or 800m grids
                  annual = T,          # annual instead of monthly summaries
                  keepZip = TRUE)

# We can also download monthly summaries for a specific range of years and months
get_prism_monthlys(type = 'ppt', 
                   years = 2018,  
                   mon=1:12, 
                   keepZip = TRUE)

# To check all downloaded PRISM datasets in the working directory
prism_files <- prism_archive_ls()
prism_files

# Each PRISM dataset consists of multiple files and is stored in a separate folder, 
# which conveniently has the exact same name as the dataset.
# The files include a BIL image as well as other aux files 

# To generate a vector of paths to the BIL files
prism_paths <- file.path(".",   # indicates that folder location is relative to the working directory
                         prism_files, 
                         paste0(prism_files, ".bil"))
prism_p30 <- rast(prism_paths[1])    # create a SpatRaster object from BIL img
prism_prec_2018 <- rast(prism_paths[2:13])

# ZONAL STATISTICS: are a type of polygon-on-raster overlay in which 
# the values in the raster are summarized within each polygon

# Rasterize county data so that each raster cell can be coded with 5-digit FIPS code 
cnty_ras <- rasterize(vect(county), 
                      prism_p30,   # to provide params for the rasterization
                      field = "fips")
summary(cnty_ras)

# Calculate zonal statistics using the zonal function
cnty_p30 <- zonal(
  prism_p30, 
  cnty_ras, 
  fun = "mean", 
  na.rm = T
)

summary(cnty_p30)

# To rename the long column name
cnty_p30 <- rename(cnty_p30, 
                   precip = 2)

# When doing such calculations, it is necessary to run basic checks to ensure that the results make sense
# Here we can see that there are fewer counties in the summary datatable than there are in the original polygon file
dim(cnty_p30)
dim(county)

setdiff(county$fips, 
        cnty_p30$fips)

# Zone Size and Raster Cell Size

# One the most effective to assess data processing errors is through visualization
# However when the zonal summaries are joined to the county polygons n mapped, 
# there is no visible counties with missing data 
cnty_join1 <- left_join(county, 
                        cnty_p30, 
                        by = "fips")

ggplot(data = cnty_join1) +
  geom_sf(aes(fill = precip), size = 0.1) +
  scale_fill_continuous(name = "Precip (mm)") +
  theme_bw() +
  theme(legend.position = "bottom")

# Use the scale_fill_distiller to specify a different color palette
# and use a logarithmic scale instead of a linear scale for plotting

# The data are heavily skewed therefore the log transform stretching 
# the lower values over a broad range of colors which make the variation easier to see
ggplot(data = cnty_join1) +
  geom_sf(aes(fill = precip), size = 0.1) +
  scale_fill_distiller(name = "Precip (mm)", 
                       palette = "YlGnBu", 
                       direction = 1,
                       trans = "log", 
                       breaks = c(100, 300, 1000, 3000)) +
  theme_bw() +
  theme(legend.position = "bottom")

# Using setdiff to look for missing values shows that all missing counties have a 
# state FIPS code of 51 which is Virginia
setdiff(county$fips, cnty_p30$fips)

# The next step is to zoom in and take a closer look at just Virginia
va_join1 <- filter(cnty_join1, 
                   STATEFP == "51")

ggplot(data = va_join1) +
  geom_sf(aes(fill = precip), size = 0.1) +
  scale_fill_distiller(name = "Precip (mm)", 
                       palette = "YlGnBu", 
                       direction = 1,
                       trans = "log", 
                       breaks = c(1000, 1150, 1300)) +
  theme_bw() +
  theme(legend.position = "bottom")

# Virginia is unique among states in that it has many small, 
# independent cities that are governed separately from the surrounding counties 
# and are therefore assigned their own county FIPS codes.

# However it is still not clear where the missing data are located
# To zoom in further to a portion of Northern Virginia, we specify coords in the coord_sf
ggplot(data = va_join1) +
  geom_sf(aes(fill = precip), size = 0.25) +
  coord_sf(xlim = c(-77.6, -77.0), 
           ylim = c(38.6, 39.1)) +
  scale_fill_distiller(name = "Precip (mm)", 
                       palette = "YlGnBu", 
                       direction = 1,
                       trans = "log", 
                       breaks = c(1000, 1150, 1300)) +
  theme_bw()

# Why the missing precip data in the smaller cities?
# When the counties were rasterized to a 4 km grid to match the precipitation data, 
# the center point of each 4 km grid cell was sampled and assigned the FIPS code 
# of the county that it fell within. However if a county is small/narrow, it is possible
# that no grid center point will fall within its boundary. 
# In this case, the county ends up excluded from the zonal summary

# The simplest way to deal with this problem is to use a finer sampling grid to generate the zonal statistics

# reduce cell size of prism data by a factor of 4 changing the cell size to approx 1km
# Bilinear resampling is used to conduct linear interpolation between the center points of the 4 km grid cells 
# to estimate values at the 1 km resolution.
prism_p30_1km <- disagg(prism_p30, 
                        fact = 4, 
                        method = "bilinear")

# Then the new 1km grid raster is used to rasterize the county dataset
cnty_ras_1km <- rasterize(vect(county), 
                          prism_p30_1km, 
                          field = "fips")

# recalculate zonal statistics
cnty_p30_1km <- zonal(prism_p30_1km, 
                      cnty_ras_1km, 
                      fun = "mean", 
                      na.rm=T)
summary(cnty_p30_1km)

# rename long column
cnty_p30_1km <- rename(cnty_p30_1km, 
                       precip = 2)

# There is also the resample function which can do similar things as the disagg func
# The resample function is used in situations where one raster needs to be adjusted 
# to match another raster with a different grid cell size or grid origin.

# Now the zonal summary table contains the same number of records as the county file 
dim(cnty_p30_1km)
dim(county)

setdiff(county$fips, 
        cnty_p30_1km$fips)

# By checking the zoomed-in map of Virginia, it is apparent that all of the counties 
# including the tiny independent cities now have precip values
cnty_join2 <- left_join(county, 
                        cnty_p30_1km, 
                        by = "fips")
va_join2 <- filter(cnty_join2, STATEFP == "51")

ggplot(data = va_join2) +
  geom_sf(aes(fill = precip), size = 0.25) +
  scale_fill_distiller(name = "Precip (mm)", 
                       palette = "YlGnBu", 
                       direction = 1,
                       trans = "log", 
                       breaks = c(1000, 1150, 1300)) +
  coord_sf(xlim = c(-77.6, -77.0), 
           ylim = c(38.6, 39.1)) +
  theme_bw()

# Extracting raster values with point data

# The following example will assess the accuracy of the monthly PRISM precipitation datasets 
# for 2018 by comparing them with weather station data.

# The PRISM data will be compared to monthly station data from the small Oklahoma mesonet file
# The geographic coordinates of these stations also need to be imported separately from the geoinfo.csv file.

mesosm <- read_csv("C:/Users/adeleke/Documents/code/gdswr_data/Chapter2/mesodata_small.csv")
geo_coords <- read_csv("C:/Users/adeleke/Documents/code/gdswr_data/Chapter3/geoinfo.csv")

# Convert data to sf object
geo_coords <- st_as_sf(geo_coords, 
                       coords = c("lon", "lat"))

# Get point data for specific stations 
mystations <- unique(mesosm$STID)
station_pts <- geo_coords %>%
  filter(stid %in% mystations)

# The extract func can be used to extract the raster values associated with these points
# First, the multilayer raster containing the precipitation data is assigned layer names
month.abb

names(prism_prec_2018) <- month.abb
prism_samp <- extract(prism_prec_2018, 
                      vect(station_pts), 
                      factors = T, 
                      df = T)
dim(prism_samp)
glimpse(prism_samp)

# The prism_samp data is in wide format while the mesosm data is in long format
# Before comparing the monthly precip values from the 2 datasets, they must be formatted n combined into a single dataframe
compare_prec <- prism_samp %>% 
  bind_cols(station_pts) %>%
  pivot_longer(Jan:Dec, 
               names_to = "mnth_name", 
               values_to = "PPrec") %>%
  mutate(month = match(mnth_name, month.abb),
         PPrec_in = PPrec * 0.0393701) %>%
  inner_join(mesosm, by = c("stid" = "STID", "month" = "MONTH")) %>%
  filter(YEAR == 2018) %>%
  select(stid, month, RAIN, PPrec_in)  # RAIN(monthly rainfall at mesonet stations); PPrec_in(monthly rainfall from PRISM data)

glimpse(compare_prec)

# Generate a scatterplot showing the relationship btw Mesonet weather station data
# and gridded PRISM data on the y-axis
# Of the 4 stations, MTHE appears to have the largest deviation from the 1:1 line
ggplot(data = compare_prec) +
  geom_point(aes(x = RAIN, 
                 y = PPrec_in, 
                 color = stid)) +
  scale_color_discrete(name = "Station ID") +
  geom_abline(slope = 1, 
              intercept = 0,
              linewidth = 0.25,
              linetype = "dashed") +
  xlab("Mesonet rainfall (in)") +
  ylab("PRISM rainfall (in)") +
  theme_bw()

# To generate additional summary statistics for the relationship btw these precip estimates
rain_lm <- lm(PPrec_in ~ RAIN, data = compare_prec) # high R-squared and slope slightly less than 1
summary(rain_lm)

# Other accuracy statistics are calculated using the summarize function
rain_sum <- compare_prec %>%
  summarize(RMSE = sqrt(mean((PPrec_in - RAIN)^2)),
            MAE = mean(abs(PPrec_in - RAIN)),
            ME = mean(PPrec_in - RAIN))
rain_sum     # these all have relatively low values indicating a close agreement btw the PRISM n MESONET measurements

# Although the PRISM precip measurements are well calibrated to the MESONET observations
# this relationship may not be representative of PRISM accuracy at other locations where 
# there are no MESONET stations

# PRACTISE

# 1. Starting with the sf object containing the county-level precipitation summaries, 
# convert the precipitation units from millimeters to inches. 
# Then classify precipitation into a factor with five categories: 
# 0-10, 10-35, 35-50, 50-70, and > 70 inches. 
# Generate a new map of precipitation using these categories.

newcols <- c(
  "gray60",
  "paleturquoise2",
  "orange4",
  "yellow2",
  "firebrick2", 
)

newcols <- colorspace::desaturate(newcols, 0.4)
newlabels <- c("0-10", "10-35", "35-50", "50-70", ">70")

cnty_join1 %>%
  mutate(prec_in = precip * 0.0393701, 
         prec_in_cat = cut(prec_in, breaks = c(0, 10, 35, 50, 70, Inf))) %>%
  ggplot() +
  geom_sf(aes(fill = prec_in_cat), size = 0.1) +
  scale_fill_manual(name = "Precipitation", 
                    values = newcols,
                    labels = newlabels,
                    na.translate = F) +
  theme_bw() +
  theme(legend.position = "bottom")

# 2. Generate a map of mean annual temperature summarized by county for the conterminous United States. 
# You can access the PRISM mean temperature data by using get_prism_normals() and specifying type = 'tmean'. 
# Then you will need to recreate the prism_paths vector and select the appropriate element to import the temperature raster. 
# Compute zonal summaries of mean temperature for every county and create a map to display the results. 
# The distribution of temperature is much less skewed than that of precipitation, so a logarithmic transformation is not needed.

options(prism.path = ".")
get_prism_normals(type = 'tmean', 
                  resolution = '4km', 
                  annual = T,
                  keepZip = TRUE)

# recreate the prism_paths vector and select the appropriate element to import the temp raster
prism_files <- prism_archive_ls()
prism_files
prism_paths <- file.path(".", 
                         prism_files, 
                         paste0(prism_files, ".bil"))
prism_temp30 <- rast(prism_paths[14])

# rasterize the county data based on the prism_temp raster
cnty_rasnew <- rasterize(vect(county), 
                      prism_temp30, 
                      field = "fips")
summary(cnty_rasnew)

# Compute zonal statistics for annual mean temperature for every county 
cnty_temp30 <- zonal(prism_temp30, 
                  cnty_rasnew, 
                  fun = "mean", 
                  na.rm = TRUE)
summary(cnty_temp30)

# Check missing counties
dim(county)
dim(cnty_temp30)
setdiff(county$fips, cnty_temp30$fips)

# Reduce resolution by a factor of 4
prism_temp30_1km <- disagg(prism_temp30, 
       fact = 4, 
       method = "bilinear")

# Rerasterize county data based on new temp data
cnty_rasnew_1km <- rasterize(
  vect(county), 
  prism_temp30_1km, 
  field = "fips"
)

# Recompute zonal statistics
cnty_temp30_1km <- zonal(
  prism_temp30_1km,
  cnty_rasnew_1km, 
  fun = "mean", 
  na.rm = T
)
summary(cnty_temp30_1km)

# Rename long column
cnty_temp30_1km <- rename(cnty_temp30_1km, 
       temp = 2)

# Check missing county
setdiff(county$fips, cnty_temp30_1km$fips)

cnty_temp_join = left_join(county, 
          cnty_temp30_1km, 
          by = "fips")

# Map the computed zonal statistics
ggplot(data = cnty_temp_join) +
  geom_sf(aes(fill = temp), size = 0.1) +
  scale_fill_distiller(name = "Temp (°C)", 
                       palette = "YlOrRd", 
                       direction = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

# 3. Repeat the comparison of gridded PRISM data and Mesonet station data using maximum temperature. 
# You can access the PRISM maximum temperature data by using the get_prism_monthly() and specifying type = 'tmax'. 
# The corresponding variable in the Mesonet dataset is TMAX.
get_prism_monthlys(
  type = "tmax", 
  years = 2018, 
  mon = 1:12, 
  keepZip = F
)

prism_files <- prism_archive_ls()
prism_files
prism_paths <- file.path(".", 
                         prism_files, 
                         paste0(prism_files, ".bil"))
prism_montemp18 <- rast(prism_paths[14:25])

# Rename raster layers
names(prism_montemp18) <- month.abb

prismtemp_samp <- extract(prism_montemp18, 
                      vect(station_pts), 
                      factors = T, 
                      df = T)

# Combine the gridded PRISM data with the station data
compare_temp <- prismtemp_samp %>% 
  bind_cols(station_pts) %>%
  pivot_longer(Jan:Dec, 
               names_to = "mnth_name", 
               values_to = "Ttmax") %>%
  mutate(month = match(mnth_name, month.abb)) %>%
  inner_join(mesosm, by = c("stid" = "STID", "month" = "MONTH")) %>%
  filter(YEAR == 2018) %>%
  select(stid, month, TMAX, Ttmax)

# Generate a scatterplot showing the relationship btw Mesonet weather station data
# and gridded PRISM data on the y-axis
# Of the 4 stations, MTHE appears to have the largest deviation from the 1:1 line

# Put data on a 0-1 scale for better comparison
scaletemp = function(value){
  (value - min(value)) / (max(value) - min(value))
}

compare_temp$TMAX = scaletemp(compare_temp$TMAX)
compare_temp$Ttmax = scaletemp(compare_temp$Ttmax)

# plot
ggplot(data = compare_temp) +
  geom_point(aes(x = TMAX, 
                 y = Ttmax, 
                 color = stid)) +
  scale_color_discrete(name = "Station ID") +
  geom_abline(slope = 1,
              intercept = 0,
              linewidth = 0.25,
              linetype = "dashed") +
  xlab("Mesonet max temp (°C)") +
  ylab("PRISM max temp (°C)") +
  theme_bw()


# Summary statistics using the linear regression eqn
temp_lm <- lm(Ttmax ~ TMAX, data = compare_temp)
summary(temp_lm)

temp_sum <- compare_temp %>%
  summarize(RMSE = sqrt(mean((Ttmax - TMAX)^2)),
            MAE = mean(abs(Ttmax - TMAX)),
            ME = mean(Ttmax - TMAX))
temp_sum

