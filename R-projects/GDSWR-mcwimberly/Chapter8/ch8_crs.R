# Load needed packages
library(tidyverse)
library(sf)
library(terra)
library(colorspace)

# Use a dataset of US county boundaries from the US census bureau

# To keep things simple, Alaska, Hawaii, and the territories are removed 
# so that the data only include the 48 conterminous states.
county <- st_read("cb_2018_us_county_20m.shp", quiet = TRUE)
county <- county %>%
  mutate(state = as.numeric(as.character(STATEFP))) %>%
  filter(state != 2, state != 15, state < 60)  

# To check which projection the data are:
st_crs(county)   # are in geographic coordinate system

# Because longitude and latitude are angles and not cartesian coordinates, 
# simply plotting them on a map results in a distorted view of the United States.
ggplot(data = county) +
  geom_sf(fill = NA) +
  theme_bw()

# Projection Name and EPSG code
st_crs(county)$Name
st_crs(county)$epsg

st_crs(county)$proj4string # older abbreviated format for storing CRS params in R = no longer recommended for defining n storing CRS info in R due to changes in underlying RPROJ software library

# WKT is currently recommended for defining CRS in R
writeLines(st_crs(county)$WktPretty)

# to reproject sf objects
county_aea <- st_transform(county, 5070)  # 5070 = Albers Equal Area projection
writeLines(st_crs(county_aea)$WktPretty)

# In the resulting map, note that the x and y axes of the coordinate system no 
# longer correspond to the graticules of latitude and longitude
ggplot(data = county_aea) +
  geom_sf(fill = NA) +
  theme_bw()

# Another way to define the projection is to enter all of the details using the WKT projection format
# This example specifies a “bad” projection that is based on the Albers projection for the conterminous United States. 
# In this example, the standard parallels have been shifted northward to the Arctic Circle, which should result in considerable distortion.

badcrs_wkt <- 
  'PROJCS["BadAlbers",
GEOGCS["NAD83",
  DATUM["North_American_Datum_1983",
    SPHEROID["GRS 1980",6378137,298.257222101]],
  PRIMEM["Greenwich",0],
  UNIT["degree",0.0174532925199433]],
PROJECTION["Albers_Conic_Equal_Area"],
PARAMETER["latitude_of_center",37.5],
PARAMETER["longitude_of_center",-96],
PARAMETER["standard_parallel_1",75],
PARAMETER["standard_parallel_2",80],
PARAMETER["false_easting",0],
PARAMETER["false_northing",0],
UNIT["metre",1],
AXIS["Easting",EAST],
AXIS["Northing",NORTH]]'

mybadcrs <- st_crs(badcrs_wkt)
county_bad <- st_transform(county, mybadcrs)
 
# This is unnecessary distortion as the standard parallels are outside of the area being mapped
ggplot(data = county_bad) +
  geom_sf(fill = NA) +
  theme_bw()

# We project back to the Albers Equal Area projection
county_fixed <- st_transform(county_bad, st_crs(county_aea))
ggplot(data = county_fixed) +
  geom_sf(fill = NA) +
  theme_bw()

# Looks a bit tilted since it is at the edge of the US
# and thus get distorted by the national-level Albers projection
newyork <- filter(county_aea, state == 36)
ggplot(data = newyork) +
  geom_sf(fill = NA) +
  theme_bw()

# It can be made to look a bit better by putting in a UTM projection zone 18 north
# In this case, the EPSG code of 26918 represents UTM zone 18 north with a NAD83 datum
ny_utm <- st_transform(newyork, 26918)
writeLines(st_crs(ny_utm)$WktPretty)

ggplot(data = ny_utm) +
  geom_sf(fill = NA) +
  theme_bw()

# Reprojecting Raster Data
# looking at the Walton county raster dataset
landcov <- rast("NLCD_2016_Land_Cover_Walton.tiff")
nrow(landcov)
ncol(landcov)
res(landcov)
ext(landcov)[1:4]

# crs returns detailed projection details for SpatRaster 
writeLines(crs(landcov))

# Extract proj name
crs(landcov, describe = TRUE)$name

# This approach prints the CRS in a format that is a bit more compact and easy to read
writeLines(st_crs(landcov)$WktPretty)

# Mapping the landcover data
# Before mapping the landcover data, they're classified into 7 broader classes
oldclas <- unique(landcov)
newclas <- c(1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 7, 7)
lookup <- data.frame(oldclas, newclas)
landcov_rc <- classify(landcov, lookup)
newnames <- c("Water",
              "Developed",
              "Barren",
              "Forest",
              "GrassShrub",
              "Cropland",
              "Wetland")
newcols <- c("mediumblue", 
             "red2", 
             "gray60", 
             "darkgreen", 
             "yellow2", 
             "orange4", 
             "paleturquoise2")
newcols2 <- desaturate(newcols, amount = 0.3)

# convert to a dataframe and then map
source("rasterdf.R")
landcov_df <- rasterdf(landcov_rc)

ggplot(data = landcov_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols2,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()

# Data will be projected into a UTM projection (zone 17 north) and NAD 83 datum using an EPSG code (26917)
landcov_utm <- project(landcov_rc, "epsg:26917", 
                       method = "near", # nearest neighbor interpolation - preserve interpolated values that fall in btw the class codes
                       res = 30)     # specifying this ensures that the output raster has the same size as the input raster 
crs(landcov_utm, describe = TRUE)$name

# The CRS has been changed and the no of rows, columns and cells in the data 
# have increased as well
nrow(landcov_utm)
ncol(landcov_utm)
res(landcov_utm)

# As was observed with New York state, the orientation of the dataset has shifted after projecting into UTM
# Because raster layers must be rectangular grids, the overall size of the raster is increased
# to accomodate the new orientation of the dataset and NA values are used to account for cells 
# in the new raster that are outside of the boundaries of the original dataset.
landcovutm_df <- rasterdf(landcov_utm)
ggplot(data = landcovutm_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols2,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()

# Reproject back to Albers Equal area
# the second arg: a raster from which to obtain information about the desired CRS, cell size, grid size, and grid origin.
landcov_goback <- project(landcov_utm, 
                          landcov, 
                          method = "near")

crs(landcov_goback, describe = TRUE)$name
crs(landcov_rc, describe = TRUE)$name
compareGeom(landcov_rc, landcov_goback)

# Both rasters have the same geometry but what about the actual data?
freq(landcov_rc)
freq(landcov_goback)

# It appears from above that the distribution of landcover types do not exactly match
# while examining the cross tabs, it is clear that while most of the cell values are the same 
# in both datasets, reprojecting the landcov_goback dataset twice has altered the land cover codes in some of the cells.

projcomp <- crosstab(c(landcov_rc, landcov_goback))
shortnames <- c("Wat", 
                "Dev", 
                "Bare", 
                "For", 
                "Grass", 
                "Crop", 
                "Wet")
rownames(projcomp) <- shortnames
colnames(projcomp) <- shortnames

# Avoid reprojecting raster data more than once
# Instead, go back to the original data and reproject 
# from the native coordinate system if you can. 
# When combining raster and vector data, avoid reprojecting the rasters 
# if possible and instead reproject the vector data to match the CRS of the rasters.

# Specifying Coordinate Reference Systems
county_nocrs <- st_read("cb_2018_us_county_noPRJ.shp", quiet = TRUE)
county_nocrs <- county_nocrs %>%
  mutate(state = as.numeric(as.character(STATEFP))) %>%
  filter(state != 2, state != 15, state < 60) 

# These data can still be mapped but the software does not know whether the coordinates
# represent latitude/longitude or some type of distance units relative to the origin of a projected CRS
st_crs(county_nocrs)

## Coordinate Reference System: NA
ggplot(data = county_nocrs) +
  geom_sf(fill = NA) +
  theme_bw()

# Attempting to reproject the data will return an error

# Referring back to the tree height example, we cannot 
# convert tree heights with unkown units to meters until we 
# known if the current units are centimeters, inches, or feet, 
# or perhaps they are already in meters
county_newcrs <- st_transform(county_nocrs, 4269)

# Although, the coordinate system is undefined, a look at the map clearly 
# shows that the data have geographic coordinates
# This is apparent from the range of coordinate values as well as the characteristic distortion 
# that we see when angular coordinates are plotted on a two-dimensional map.
# The problem is that the necessary metadata to identify the CRS is not present
# the st_set_crs() function is used with the EPSG code for geographic coordinates in a NAD83 datum (4269)
county_crs <- st_set_crs(county_nocrs, 4269)
writeLines(st_crs(county_crs)$WktPretty)

# Now that the proper coordinate reference system has been assigned, it is possible to project the data into another CRS.
county_newcrs <- st_transform(county_crs, 5070)
writeLines(st_crs(county_newcrs)$WktPretty)

# it is possible to assign CRS to raster data using the crs() function
# myraster_nocrs here contains no crs
crs(raster_nocrs) <- "epsg:4326"
crs(raster_nocrs) <- crs(raster_withcrs)

# PRACTISE

# Generate an sf object for the state of California (state FIPS code = 6). 
# Compare maps of the state in a geographic projection (EPSG code 4269), 
# Albers equal area projection (EPSG code 5070), and UTM zones 10 and 11 (EPSG codes 26910 and 26911). 
# Explore how the different coordinate reference systems affect the shape and orientation of the state.

cali <- filter(county_aea, state == 6)
ggplot(data = cali) +
  geom_sf(fill = NA) +
  theme_bw()

cali_4269 = st_transform(cali, 4269)
ggplot(data = cali_4269) +
  geom_sf(fill = NA) +
  theme_bw()

cali_5070 = st_transform(cali, 5070)
ggplot(data = cali_5070) +
  geom_sf(fill = NA) +
  theme_bw()

cali_UTM26910 = st_transform(cali, 26910)
cali_UTM26911 = st_transform(cali, 26911)

ggplot(cali_UTM26910) + 
  geom_sf(fill = NA) + 
  theme_bw()

ggplot(cali_UTM26911) + 
  geom_sf(fill = NA) + 
  theme_bw()

# Reproject the Walton County land cover dataset into the same geographic coordinate system 
# used in the U.S. county dataset. Specify a resolution of 0.008333 degrees (30 arc seconds) for the reprojected raster.
landcov_transformed = project(
  landcov, 
  crs(county), 
  method = "near",
  res = 0.008333
)
