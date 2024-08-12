# Install required packages 
# install.packages("sf")
# install.packages("terra")
# install.packages("spData")
# install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

# remotes::install_github("geocompx/geocompkg", dependencies = TRUE)

# Load the packages
library(sf)
library(terra)
library(spData)
library(spDataLarge)

# To view sf's vignettes
vignette(package = "sf") # see which vignettes are available
vignette("sf1")          # an introduction to the package

# Use world data from the spData pkg
data("world")
class(world)
names(world)

# the geom column give sf objects their spatial powers
plot(world) # triggers the non-exported plot.sf

# Use summary to get an overview of the variables in the sf object
summary(world["lifeExp"])  # though we select only one column it also outputs the summary of the geom column

# this demonstrates the sticky behaviour of the geometry column of sf objects
# Meaning the geometry is kept unless the user deliberately removes them
# the result provides a quick summary of both the non-spatial and spatial data contained in world
# the mean avg life expectancy is 71yrs ranging from 51 to more than 83 years of age

# sf objects are easy to subset
# return an object containing the first two rows and first three columns of the world object
# the geom column is a list column of class sfc. In turn, sfc objects are composed of one or 
# more objects of class sfg: simple feature geometries 
world_mini = world[1:2, 1:3]
world_mini

# sf's support for tidyverse packages is exemplified by the read_sf() 
# Unlike st_read which returns attributes stored in a base R data.frame, 
# read_sf silently returns data as a tidyverse tibble
world_dfr = st_read(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)

world_tbl = read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_tbl)

# Basic map making
# A key or legend with continuous color is produced if the object to be plotted has a single variable
plot(world[3:6])
plot(world["pop"]) # optionally use col= arg although this will not create a continuous palette

# Filter countries in Asia and combine them into a single feature
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

# We can now plot the Asian continent over the world map
# Note that the base plot must have only one facet for add=T to work
# If the plot has a key, reset=F must be used
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# Because sf extends base R plotting methods, plot()'s arguments work with sf objects
?graphics::plot; ?par

# Overlay circles whose diameters represent country's population on the world map
# An unprojected version of this figure:
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

# A projected version of this figure can be produced with these commands:
# world_proj = st_transform(world, "+proj=eck4")
# world_cents = st_centroid(world_proj, of_largest_polygon = TRUE)
# par(mar = c(0, 0, 0, 0))
# plot(world_proj["continent"], reset = FALSE, main = "", key.pos = NULL)
# g = st_graticule()
# g = st_transform(g, crs = "+proj=eck4")
# plot(g$geometry, add = TRUE, col = "lightgrey")
# cex = sqrt(world$pop) / 10000
# plot(st_geometry(world_cents), add = TRUE, cex = cex, lwd = 2, graticule = TRUE)

# sf plot method also arguments specific to geographic data: expandBB which can be 
# used to plot an sf object in context: it takes a numeric vector of length four that 
# expands the bounding box of the plot relative to zero in the following order: 
# bottom, left, top, right.
# This is used to plot India in the context of its giant Asian neighbors with 
# emphasis on China to the east
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)

# GEOMETRY TYPES

# the sf class
# Objects of class sf represents such data by combining the attributes (dataframe)
# with the simple feature geometry column (sfc). they are created with st_sf as illustrated below
lnd_point = st_point(c(0.1, 51.5))                 # sfg object
lnd_geom = st_sfc(lnd_point, crs = "EPSG:4326")    # sfc object (simple feature geometry column)
lnd_attrib = data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2023-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object

# This results in an sf object:
lnd_sf
class(lnd_sf) # sf objects have 2 classes

# Simple features are simpy dataframes (square tables) but with spatial attributes 
# stored in a list column usually called geometry or geom

# Simple Feature Geometries (sfg) object
# st_point creates single points from numeric vectors
st_point(c(5, 2))                 # XY point
st_point(c(5, 2, 3))              # XYZ point
st_point(c(5, 2, 1), dim = "XYM") # XYM point
st_point(c(5, 2, 3, 1))           # XYZM point

# By contrast, use matrices in the case of multipoint and linestrings
# the rbind function simplifies the creation of matrices

## MULTIPOINT
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)

## LINESTRING
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
linestring_matrix = st_linestring(linestring_matrix)

## POLYGON
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)

## POLYGON with a hole
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)

## MULTILINESTRING
multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)))
st_multilinestring(multilinestring_list)

## MULTIPOLYGON
multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
st_multipolygon(multipolygon_list)

## GEOMETRYCOLLECTION
geometrycollection_list = list(st_multipoint(multipoint_matrix),
                               st_linestring(linestring_matrix))
st_geometrycollection(geometrycollection_list)

# Simple Feature Columns
# sfc POINT
point1 = st_point(c(5, 2))
point2 = st_point(c(1, 3))
points_sfc = st_sfc(point1, point2)  # this is important since sfc represents the geometry column in sf dataframes
points_sfc

# In most cases, sfc objects contains objects of the same geometry type
# when we convert sfg objects of type polygon into a simple feature geometry column, 
# we would also end up with an sfc object of type polygon

# sfc POLYGON
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1) #sfg object
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)

# sfc MULTILINESTRING
multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
multilinestring_list2 = list(rbind(c(2, 9), c(7, 9), c(5, 6), c(4, 7), c(2, 7)), 
                             rbind(c(1, 7), c(3, 8)))
multilinestring2 = st_multilinestring((multilinestring_list2))
multilinestring_sfc = st_sfc(multilinestring1, multilinestring2)
st_geometry_type(multilinestring_sfc)

# It is also possible to create an sfc object from sfg objects with different geometry types:
# sfc GEOMETRY
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)

# sfc objects can additionally store info about CRS
st_crs(points_sfc)

# All geometries in sfc objects must have the same CRS. 
# A CRS can be specified with the crs argument of st_sfc or st_sf 
# which takes a CRS identifier provided as a text string

# Set the CRS with an identifier referring to an 'EPSG' CRS code:
points_sfc_wgs = st_sfc(point1, point2, crs = "EPSG:4326")
st_crs(points_sfc_wgs) # print CRS (only first 4 lines of output shown)

# The sfheaders package
# sfheaders speeds-up the construction, conversion and manipulation of sf objects
# It focuses on building sf objects from vectors, matrices and data frames, 
# rapidly, and without depending on the sf library; 
# and exposing its underlying C++ code through header files

# Create the simplest possible sfg object
v = c(1, 1)
v_sfg_sfh = sfheaders::sfg_point(obj = v)
v_sfg_sfh # printing without sf loaded
#>      [,1] [,2]
#> [1,]    1    1
#> attr(,"class")
#> [1] "XY"    "POINT" "sfg" 

# Print output when sf is loaded is similar to sf output
v_sfg_sf = st_point(v)
print(v_sfg_sf) == print(v_sfg_sfh)
#> POINT (1 1)
#> POINT (1 1)
#> [1] TRUE

# The next examples shows how sfheaders creates sfg objects from matrices and data frames:
# matrices
m = matrix(1:8, ncol = 2)
sfheaders::sfg_linestring(obj = m)

# data frames
df = data.frame(x = 1:4, y = 4:1)
sfheaders::sfg_polygon(obj = df)

# Reusing the objects v, m, and df we can also build simple feature columns (sfc) as follows (outputs not shown):
sfheaders::sfc_point(obj = v)
sfheaders::sfc_linestring(obj = m)
sfheaders::sfc_polygon(obj = df)

# Similarly sf objects 
sfheaders::sf_point(obj = v)
sfheaders::sf_linestring(obj = m)
sfheaders::sf_polygon(obj = df)

# Set the CRS 
df_sf = sfheaders::sf_polygon(obj = df)
st_crs(df_sf) = "EPSG:4326"

# Spherical geometry operations with s2
# sf can run in two modes with respect to S2: on and off.
# By default the S2 geometry engine is turned on, as can be verified with the following command:
sf_use_s2()

# An example of the consequences of turning the geometry engine off is shown below, 
# by creating buffers around the india object created earlier in the chapter 
# (note the warnings emitted when S2 is turned off)
india_buffer_with_s2 = st_buffer(india, 1) 
sf_use_s2(FALSE)

# Buffer when s2 is turned off
india_buffer_without_s2 = st_buffer(india, 1) # 1 degree

# Throughout this book, we'll assume that s2 is turned on unless explicitly stated
# Turn it on again with the ffg command:
sf_use_s2(TRUE)

# RASTER DATA
# R packages for working with raster datasets

# terra lets you work on large raster datasets that are too large to fit into the memory
# In this case, terra provides the possibility to divide the raster into smaller chunks
# and processes these iteratively instead of loading the whole raster file into RAM

# For illustration, we'll use data from the spDataLarge pkg
# Create a SpatRaster
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

# print raster header
my_rast

# Dedicated functions report each component:
dim(my_rast); ncell(my_rast); res(my_rast); crs(my_rast); inMemory(my_rast)

# Raster classes
# The SpatRaster class represents rasters object of terra.

# The easiest way to read in a raster object in R is to 
# read in a raster file from disk or from a server
single_raster_file = system.file("raster/srtm.tif", package = "spDataLarge")
single_rast = rast(raster_filepath)

# Rasters can also be created from scratch using the rast() func
# Given the number of rows n columns as well as the extent, the resolution has to be 0.5
# The unit of the resolution is that of the underlying CRS here is in degrees
new_raster = rast(nrows = 6, ncols = 6, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)

# SpatRaster also handles multiple layers 
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
multi_rast

# Retrieve the number of layer stored in a spatraster object
nlyr(multi_rast)

# Select layers
multi_rast3 = subset(multi_rast, 3) #layer number
multi_rast4 = subset(multi_rast, "landsat_4") # layer name

# Most SpatRaster objects do not store raster values, but rather a pointer to the file itself.

# COORDINATE REFERENCE SYSTEM
# CRS defines how the spatial elements of the data relate to the surface of the Earth 
# or other bodies

# Geographic CRSs identify any location on the earths surface using two values
# - longitude and latitude. Longitude is location in the East-West direction in angular distance from the Prime Meridian
# Latitude is angular distance North-South of the equatorial plane
# 2 types of datum - geocentric(WGS84) and local(NAD83)

# Projected CRSs are based on geographic CRSs and rely on map projections to 
# convert the 3D surface into Easting and Northing (x & y) values in a projected CRS
# There are 3 main groups of projection types - conic, cylindrical & planar

# gives a list of the available projections supported by the PROJ library
sf_proj_info(type = "proj")

# Units
# It is important to formally specify the units in which the geometry data or 
# cells are measured to provide context, and ensure that subsequent calculations are done in context
# A novel feature of geometry data in sf objects is that they have native support for units. 
# This means that distance, area and other geometric calculations in sf return values that 
# come with a units attribute, defined by the units package

# This is demonstrated below which calculates the area of Luxembourg
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg) # unit in square metres

# The unit info is stored as of the attributes of the sf object
attributes(st_area(luxembourg))

# To translate the huge number into a more digestible size, it is tempting to 
# divide the results by a million (the number of square meters in a square kilometer):
st_area(luxembourg) / 1000000

# However the result is incorrectly given again as square metres
# The solution is to set the correct unit with the units package
units::set_units(st_area(luxembourg), km^2)

# People working on raster data should approach changes in the units of analysis 
# with care. The my_rast object uses a WGS84 projection with decimal degrees as units.
res(my_rast)

# If we used the UTM projection, the units would change:
repr = project(my_rast, "EPSG:26912")
res(repr)   # unit of UTM is in meters but not in output

# EXERCISES
# Use summary on the geometry column of the world data object
summary(world$geom)

# Plot a map of Nigeria in context

# Filter countries in Africa and combine them into a single feature
world_africa = world[world$continent == "Africa", ]
nigeria = world[world$name_long == "Nigeria", ]
plot(st_geometry(nigeria), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_africa), add = TRUE)

# Create an empty SpatRaster object called my_raster with 10 columns and 10 rows. 
# Assign random values between 0 and 10 to the new raster and plot it.
my_raster = rast(nrows = 10, ncols = 10, 
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = sample(0:10,10))

plot(my_raster)

# Read-in the raster/nlcd.tif file from the spDataLarge package. 
# What kind of information can you get about the properties of this file?
nlcd = system.file("raster/nlcd.tif", package = "spDataLarge")
nlcd = rast(nlcd)
nlcd
plot(nlcd)

# Check the CRS of the nlcd raster. What info can you learn from it?
st_crs(nlcd)
