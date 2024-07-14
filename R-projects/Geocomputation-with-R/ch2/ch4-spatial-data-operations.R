# Spatial data operations
library(sf)
library(terra)
library(dplyr)
library(spData)

# SPATIAL OPERATIONS ON VECTOR DATA ---------------------

# Spatial subsetting - Spatial subsetting is the process of taking a 
# spatial object and returning a new object containing only features 
# that relate in space to another object.

# To demonstrate spatial subsetting, we will use the nz and nz_height datasets 
# in the spData package, which contain geographic data on the 16 main regions 
# and 101 highest points in New Zealand, respectively

canterbury = nz |> filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ] #subsetting to return all high points in canterbury

# Objects used for spatial subsetting must have the class sf/sfc:
# both nz and nz_height are geographic vector dataframes 
# the result of the operation returns another sf object representing the features 
# in the target nz_height object that intersect with (in this case high points 
# that are located within) the canterbury region

# Alternative spatial operators can be specified with op argument
nz_height[canterbury, , op = st_disjoint] # returns the opposite of intersect i.e.points that don't intersect with Canterbury

# Another way of doing spatial subsetting uses objects returned by topological operators
# These objects can be useful in their own right, for example when exploring the graph 
# network of relationships between contiguous regions, but they can also be used for subsetting
sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp) # a list of length x
sel_sgbp

# Get all high points that intersects with cantenbury
sel_logical = lengths(sel_sgbp) > 0
canterbury_height2 = nz_height[sel_logical, ]

# Another way to return a logical output is to set sparse arg in st_intersects
st_intersects(x = nz_height, y = canterbury, sparse = F)[,1]

# The same results can be obtained by using st_filter 
# which was created to increase compatibility btw sf objects and dplyr data manipulation code
canterbury_height3 = nz_height |>
  st_filter(y = canterbury, .predicate = st_intersects)

# Topological relations - describe the spatial relationships btw objects
# Functions testing for different types of topological relations are called binary predicates

# To create tabular data representing coordinates of polygon vertices
polygon_matrix = cbind(
  x = c(0, 0, 1, 1,   0),
  y = c(0, 1, 1, 0.5, 0)
)
polygon_sfc = st_sfc(st_polygon(list(polygon_matrix)))

# We'll create additional geometries to demonstrate spatial relations 
point_df = data.frame(
  x = c(0.2, 0.7, 0.4),
  y = c(0.1, 0.2, 0.8)
)
point_sf = st_as_sf(point_df, coords = c("x", "y"))

# Create a simple plot of the polygon and points
plot(polygon_sfc)
plot(point_sf, add=T)

# A simple query is: which of the points in point_sf intersect in some way with polygon_sfc
st_intersects(point_sf, polygon_sfc) # only registers a relation if one exists 
# reducing the memory requirements of topological operations on multi-feature objects

# Return a dense matrix
st_intersects(point_sf, polygon_sfc, sparse = FALSE)

# In the above output each row represents a feature in the target (argument x) object 
# and each column represents a feature in the selecting object (y)
# In this case, there is only one feature in the y object polygon_sfc so the result
# which can be used for subsetting has only one column

# st_intersects returns true even in cases where the features just touch:
# intersects is a catch-all topological operation which identifies many types of spatial relationships
# More restrictive questions include which points lie within the polygon, 
# and which features are on or contain a shared boundary with y? 
# These can be answered as follows:
st_within(point_sf, polygon_sfc) # only one point is within the polygon boudary
st_touches(point_sf, polygon_sfc) # only one point touches the polygon boundary

# Use st_disjoint to return only objects that do not spatially relate in anyway to the selecting object
st_disjoint(point_sf, polygon_sfc, sparse = FALSE)[, 1]

# st_within_distance can be used to set how close target objects need to be before they're selected
# shows that every point is within 0.2 units of the polygon
st_is_within_distance(point_sf, polygon_sfc, dist = 0.2, sparse = FALSE)[, 1] 

# Verifying distance btw point 2 and polygon 
st_distance(point_sf, polygon_sfc)

# DISTANCE RELATIONS
# Distance relations are continuous while topological relations presented above are binary

# Find the dist btw the highest point in NZ and the geographic centroid of the canterbury region
nz_highest = nz_height |> slice_max(n = 1, order_by = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

# Find the distance btw the first 3 features in nz_height and 
# the Otago and Canterbury regions of NZ represented by the co object
co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

# This demonstrates the fact distance btw the second n third feature in nz_height 
# and the second feature in co is zero
# This demonstrates the fact that distances between points and polygons refer to the distance to any part of the polygon:
# The second and third points in nz_height are in Otago, which can be verified by plotting them
plot(st_geometry(co))
plot(st_geometry(nz_height)[2:3], add = TRUE)

# DE-9IM Strings
# Underlying the binary predicates demonstrated in the previous section is the Dimensionally Extended 9-Intersection Model (DE-9IM).
# It underlies many spatial operations and enables the creation of custom spatial predicates
# DE-9IM is applicable to 2D objects in Euclidean space meaning that the model n software implementing it such as GEOS
# assume that one is working with a projected CRS
# the 9 intersection model (9IM) which shows the intersections between every combination of each object’s interior, boundary and exterior
# DE-9IM strings are derived from the dimension of each type of relation. 
# In this case the red intersections in Figure 4.4 have dimensions of 0 (points), 1 (lines), and 2 (polygons)
# F in the DIM string means false - the objects are disjoint

# Use st_relate to obtain DE-9IM string
xy2sfc = function(x, y) st_sfc(st_polygon(list(cbind(x, y))))
x = xy2sfc(x = c(0, 0, 1, 1, 0), y = c(0, 1, 1, 0.5, 0))
y = xy2sfc(x = c(0.7, 0.7, 0.9, 0.7), y = c(0.8, 0.5, 0.5, 0.8))
st_relate(x, y)
?st_relate

# ‘Queen’ relations mean that ‘boundary-boundary’ relations (the cell in the second column and the second row) must not be empty
# While for 'Rook' relations, the same element must be 1 denoting a linear intersection.
# These are implemented as follows:
st_queen = function(x, y) st_relate(x, y, pattern = "F***T****")
st_rook = function(x, y) st_relate(x, y, pattern = "F***1****")

# Building on the object x created previously, we can use the newly created functions
# to find out which elements in the grid are a 'queen' and a 'rook' in relation to the middle 
# square of the grid as follows:
grid = st_make_grid(x, n = 3)
grid_sf = st_sf(grid)
grid_sf$queens = lengths(st_queen(grid, grid[5])) > 0
plot(grid, col = grid_sf$queens)

grid_sf$rooks = lengths(st_rook(grid, grid[5])) > 0
plot(grid, col = grid_sf$rooks)

# SPATIAL JOINING
# Joining two non-spatial datasets relies on a shared key variable. 
# Spatial data joining applies the same concept, but instead relies on spatial relations, described in the previous section.

# Illustration: imagine you have ten points randomly distributed across the Earth’s surface and you ask, for the points that are on land, which countries are they in?
set.seed(2018) # set seed for reproducibility
(bb = st_bbox(world)) # the world's bounds

# Create points that are randomly scattered over the earth's surface
random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)
random_points = random_df |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") # set coordinates and CRS

# Before doing spatial joins, we first use spatial subsetting to get the countries 
# that contain random points
world_random = world[random_points, ]
nrow(world_random) #  to verify the number of country names returned in the joined dataset should be 4

# By default st_join performs a left join
# To change, set the arg left=FALSE
# Like spatial subsetting, the default topological operator used by st_join() is st_intersects()
# which can be changed by setting the join argument
random_joined = st_join(random_points, world["name_long"])

# DISTANCE-BASED JOINS
# Sometimes 2 geographic data do not intersect but still have a geographic relationship
# due to their proximity. The datasets cycle_hire n cycle_hire_osm provide a good example

# Plotting them shows that they are often closely related but they do not touch
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

# We can check if any points are the same using st_intersects
any(st_intersects(cycle_hire, cycle_hire_osm, sparse = FALSE))

# Imagine that we need to join the capacity variable in cycle_hire_osm 
# onto the official ‘target’ data contained in cycle_hire. 
# This is when a non-overlapping join is needed. 

# Use st_within_distance with a threshold distance of 20m
sel = st_is_within_distance(cycle_hire, cycle_hire_osm, 
                            dist = units::set_units(20, "m"))
summary(lengths(sel) > 0)

# This shows that there are 438 points in the target object cycle_hire 
# within the threshold distance of cycle_hire_osm. 
# How to retrieve the values associated with the respective cycle_hire_osm points?
# The solution is with st_join but with an additional dist argument
z = st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, 
            dist = units::set_units(20, "m"))
nrow(cycle_hire)
nrow(z)

# The number of rows in the joined result is greater than the target. 
# This is because some cycle hire stations in cycle_hire have multiple matches in cycle_hire_osm
# To aggregate the values for the overlapping points and return the mean, 
# we can use the aggregation methods learned in Chapter 3, 
# resulting in an object with the same number of rows as the target.
z = z |> 
  group_by(id) |> 
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

# The capacity of nearby stations can be verified by comparing 
# a plot of the capacity of the source cycle_hire_osm data with 
# the results in this new object:
plot(cycle_hire_osm["capacity"])
plot(z["capacity"], add = T, col="red")

identical(st_geometry(cycle_hire), st_geometry(z))

# SPATIAL AGGREGATION
# Spatial data aggregation condenses data; aggregated outputs have fewer rows than nonaggregated inputs

# Imagine you want to find out the average height of high points in each region: 
# it is the geometry of the source(y) that defines how values in the target object(x) are grouped.
nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)

# The result of the previous command is an sf object with the same geometry as the spatial 
# aggregating object (nz) which you can verify with the ffg command:
identical(st_geometry(nz), st_geometry(nz_agg))

# The same result can also be generated by piping the output from st_join into the tidy funcs
# - group_by() and summarize
nz_agg2 = st_join(x = nz, y = nz_height) |>
  group_by(Name) |>
  summarize(elevation = mean(elevation, na.rm = TRUE))

identical(st_geometry(nz), st_geometry(nz_agg2))

# Note: one difference between the aggregate() and group_by() |> summarize() approaches 
# is that the former results in NA values for unmatching region names while the latter 
# preserves region names.
# The tidy approach is thus more flexible in terms of aggregating functions and the column
# names of the results. 

# JOINING INCONGRUENT LAYERS
# Spatial congruence is an important concept related to spatial aggregation
# An aggregating object(y) is congruent with the target object(x) if the two objects have shared borders.

# spData contains the datasets - incongruent and aggregating_zones
# Let's plot these datasets
plot(incongruent[1], col="NA", lwd=2)
plot(aggregating_zones[1], add =T, col="NA", border="red", lwd=2)

# How can we transfer the values of the underlying nine spatial polygons into the two polygons of aggregating_zones?
# The simplest useful method for this is area weighted spatial interpolation, 
# which transfers values from the incongruent object to a new column in aggregating_zones 
# in proportion with the area of overlap: the larger the spatial intersection 
# between input and output features, the larger the corresponding value.
iv = incongruent["value"] # keep only the values to be transferred
agg_aw = st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)

# Here an aggregating operation (sum) is used to sum up the values of the intersections 
# falling into the aggregating zones since total income is a so-called spatially extensive variable
# assuming income is evenly distributed across the smaller zones 
# This would be different for spatially intensive variables 
# such as average income/percentages, which do not increase as the area increases.

# SPATIAL OPERATIONS ON RASTER DATA

elev = rast(system.file("raster/elev.tif", package = "spData"))
grain = rast(system.file("raster/grain.tif", package = "spData"))

# Raster objects can also be extracted by location(coordinates) and other spatial objects

# To use coordinates for subsetting, one can ‘translate’ the coordinates into a cell ID
id = cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))
elev[id]
# the same as
terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))

# Raster objects can also be subset with another object as demonstrated in the code below:
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
elev[clip]
# we can also use extract
terra::extract(elev, terra::ext(clip))

# The example above returned the values of specific cells but in many cases 
# spatial outputs from subsetting operations on raster datasets are needed.

# We can get spatial outputs from subset operations by setting drop arg in []
elev[1:2, drop = FALSE]    # spatial subsetting with cell IDs

# Another use case is when a raster with logical(or NA) values is used to mask 
# another raster with the same extent and resolution

# create raster mask
rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)

# Next we want to keep those values in elev which are TRUE in rmask. 
# In other words we want to mask elev with rmask
# spatial subsetting
elev[rmask, drop = FALSE]           # with [ operator
# we can also use mask
mask(elev, rmask)

# The above approach can also be used to replace some values 
# (e.g. expected to be wrong) with NA values
elev[elev < 20] = NA

# MAP ALGEBRA = describe a set of conventions, capabilities & techniques for the analysis of 
# geographic raster data and less prominently vector data. 
# In this context, we define map algebra more narrowly, as operations that modify or summarize raster cell values,
# with reference to surrounding cells, zones, or statistical functions that apply to every cell.

# Map algebra operations are classified by the number of cells used for each pixel processing steps
# and the type of output:

# 1. Local (or per cell) operations
elev + elev
elev^2
log(elev)
elev > 5

# Another good example of local raster operations 
# is the classification of intervals of numeric values into groups 

# Create a matrix of grouped intervals
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
rcl

# Use the classify command
recl = classify(elev, rcl = rcl)

# The more efficient app(), lapp() and tapp() functions can also be used in the presence of large raster datasets
# the app() function can be used to summarize the values of multiple layers into one layer
# tapp() allows us to select a subset of layers (see the index arg) for which we want to perform a certain operation
# lapp() allows us to apply a function to each cell using layers as arguments - an application of lapp() is presented below:

# Let's calculate NDVI for the multispectral satellite file of the Zion National Park
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)

# Landsat level-2 products are stored as integers to save disk space, 
# and thus we need to convert them to floating-point numbers before doing any calculations.
# For that purpose, we need to apply a scaling factor and an offset to the original values
multi_rast = (multi_rast * 0.0000275) - 0.2

# The values now should be btw 0 and 1 but this is not the case here probably due to 
# the presence of clouds and other atmospheric effects thus we need to replace values below 0 to 0
multi_rast[multi_rast < 0 ] = 0

# The next step should be to implement the NDVI formula into an R function
ndvi_fun = function(nir, red){
  (nir - red) / (nir + red)
}

# We need to remember that our function expects two bands (not 4) from the original raster
ndvi_rast = lapp(multi_rast[[c(4, 3)]], fun = ndvi_fun)

# Predictive mapping is another interesting application of local raster operations. 
# The response var corresponds to measured/observed points in space
# Spatial predictions on raster objects can therefore be made by applying estimated coefficients 
# to the predictor raster values, and summing the output raster values

# 2. FOCAL OPERATIONS
# Focal operations take into account a central (focal) cell and its neighbors.
# The neighborhood under consideration (also named kernel, filter or moving window) is typically of size 3by3 cells
# A focal operation applies an aggregation function to all cells within the specified neighborhood
# uses the corresponding output as the new value for the central cell n moves on to the next central cell

r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min) # define the shape of the moving window with a matrix where values correspond to weights

# Focal functions or filters play a dominant role in image processing. 
# Low-pass or smoothing filters use the mean function to remove extremes.
# By contrast, high-pass filters accentuate features 
?focal

# Terrain processing - the calculation of topographic characteristics such as slope, aspect and flow directions relies on focal functions.
# terrain() can be used to calculate these metrics although some terrain algorithms are not implemented in this terra function

# 3. ZONAL OPERATIONS - Just like focal operations, zonal operations apply an aggregation function to multiple raster cells
# However a categorical raster defines the zonal filters or zones
# Consequently raster cells defining the zonal filter do not necessarily have to be neighbors
# the result of a zonal operation is a summary table grouped by zone which is why this operation is also known as zonal statistics in the GIS world.
# This is in contrast to focal operations which return a raster object by default

# Use zonal() to calculate the mean elevation associated with each grain size class
z = terra::zonal(elev, grain, fun = "mean", na.rm=T) # set as.raster arg to get a raster with calculated statistics for each zone
z

# 4. GLOBAL OPERATIONS & DISTANCES
# Global operations are a special case of zonal operations with the entire raster dataset representing a single zone. 

# Map algebra counterparts in vector processing
# Many map algebra operations have a counterpart in vector processing
# + Computing a distance raster (global operation) while only considering a maximum distance 
# (logical focal operation) is the equivalent to a vector buffer operation
# + Reclassifying raster data (either local or zonal function depending on the input) is equivalent to dissolving vector data

# MERGING RASTERS
# Suppose we would like to compute the NDVI and additionally want to compute terrain attributes from elevation data for observations within a study area. 
# Such computations rely on remotely sensed information. The corresponding imagery is often divided into scenes covering a specific spatial extent, 
# and frequently, a study area covers more than one scene. Then we'd need to merge the scenes covered by our study area

# Download SRTM elevation data for Austria & Switzerland and merge the 2 rasters into one
aut = geodata::elevation_30s(country = "AUT", path = tempdir())
ch = geodata::elevation_30s(country = "CHE", path = tempdir())
aut_ch = merge(aut, ch)

# The merging approach is of little use when the overlapping values do not correspond to each other.
# This is frequently the case when you want to combine spectral imagery from scenes that were taken on different dates. 
# Using the merge function on these images will work but you'll see a clear border in the image
# the mosaic() command lets you define a function for the overlapping area. 
# For instance, we could compute the mean value – this might smooth the clear border in the merged result but it will most likely not make it disappear.

# EXERCISES

# E1. It was established in Section 4.2 that Canterbury was the region of New Zealand containing most of the 101 highest points in the country. 
# How many of these high points does the Canterbury region contain? 
nrow(canterbury_height); sum(sel_logical)

# Bonus: plot the result using the plot() function to show all of New Zealand, canterbury region highlighted in yellow, high points in 
# Canterbury represented by red crosses (hint: pch = 7) and high points in other parts of New Zealand represented by blue circles. 
# See the help page ?points for details with an illustration of different pch values.
plot(nz[1], col="NA", lwd=2, reset=F)
plot(canterbury, col="yellow", add=T)
plot(canterbury_height, col="red", pch=7, add=T)
plot(nz_height[-as.numeric(row.names(canterbury_height)),], col="blue", pch=1, add=T, lwd=2)

# E2. Which region has the second highest number of nz_height points, and how many does it have?
st_join(nz_height, nz["Name"]) |>
  group_by(Name) |>
  summarise(count = n()) |>
  arrange(desc(count))

# E3. Generalizing the question to all regions: how many of New Zealand’s 16 regions 
# contain points which belong to the top 101 highest points in the country? Which regions?
# Bonus: create a table listing these regions in order of the number of points and their name.

st_join(nz_height, nz["Name"]) |>
  st_drop_geometry() |>
  group_by(Name) |>
  summarise(count = n()) |>
  arrange(desc(count))

# E4. Test your knowledge of spatial predicates by finding out and 
# plotting how US states relate to each other and other spatial objects.
colorado = us_states[us_states$NAME == "Colorado",]

# Plot colorado over the US map
plot(us_states[1], col="NA", lwd=2, reset=F)
plot(colorado, col="red", add=T)

# Create a new object representing all the states that geographically intersect with Colorado
# and plot the result (hint: the most concise way to do this is with the subsetting method [).
us_states[colorado,,op=st_intersects] |>
  plot()

# Create another object representing all the objects that touch (have a shared boundary with) 
# Colorado and plot the result (hint: remember you can use the argument op = st_intersects 
# and other spatial relations during spatial subsetting operations in base R).
us_states[colorado, ,op=st_touches] |>
  plot()

# Bonus: create a straight line from the centroid of the District of Columbia near the East coast 
# to the centroid of California near the West coast of the USA 
# (hint: functions st_centroid(), st_union() and st_cast() described in Chapter 5 may help) 
# and identify which states this long East-West line crosses.

dc = us_states[us_states$NAME=="District of Columbia",]
cali = us_states[us_states$NAME=="California",]

# Create a straight line from DC centroid to East Coast centroid
dc_cali_line = st_union(st_centroid(dc), st_centroid(cali)) |>
  st_cast("LINESTRING") 

# Identify which states this long East-West line crosses
us_states[dc_cali_line,,op=st_intersects] |>
  plot(max.plot=1)

# E5. Use dem = rast(system.file("raster/dem.tif", package = "spDataLarge")), 
# and reclassify the elevation in three classes: low (<300), medium and high (>500). 
# Secondly, read the NDVI raster (ndvi = rast(system.file("raster/ndvi.tif", package = "spDataLarge"))) 
# and compute the mean NDVI and the mean elevation for each altitudinal class.

# Get DEM & NDVI data
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
ndvi = rast(system.file("raster/ndvi.tif", package = "spDataLarge"))

# Reclassify DEM data in 3 classes
# Create a matrix of grouped intervals
rclm = matrix(c(0, 300, 1, 300, 500, 2, 500, 1094, 3), ncol = 3, byrow = TRUE)
rclm

# Use the classify command
dem_rec = classify(dem, rcl = rclm)

# Compute the mean NDVI and elevation for each altitudinal class
zonal(ndvi, dem_rec)
zonal(dem, dem_rec)

# E6. Apply a line detection filter to rast(system.file("ex/logo.tif", package = "terra")). 
# Plot the result. Hint: Read ?terra::focal().

rimg = rast(system.file("ex/logo.tif", package = "terra"))
plot(rimg)

# Apply the line detection Laplace n Sobel filters
rimgld = focal(rimg, w=matrix(c(0,1,0,1,-4,1,0,1,0), nrow=3))
plotRGB(rimgld)

# Calculate the NDWI of a Landsat image
ls = rast(system.file("raster/landsat.tif", package = "spDataLarge"))

# Rescale values in Landsat data
ls = (ls * 0.0000275) - 0.2
ls[ls < 0] = 0

# Implement NDWI formula
ndwi_fun = function(green, nir){
  (green - nir) / (green + nir)
}

# We need to remember that our function expects two bands (not 4) from the original raster
ndwi_rast = lapp(ls[[c(2, 4)]], fun = ndwi_fun)
ndvi_rast = lapp(ls[[c(4, 3)]], fun = ndvi_fun)

names(ndvi_rast) = "ndvi"
names(ndwi_rast) = "ndwi"

# Compute corr btw NDVI and NDWI
layerCor(c(ndvi_rast, ndwi_rast), fun = "cor")

# E8. Compute the distance to the nearest coastline
library(maptools)
library(raster)
data(wrld_simpl)

# Get elevation data for spain
dem_esp = geodata::elevation_30s(country="spain", path = getwd())
plot(dem_esp)

# Increase cell size of the dem raster
dem_esp = aggregate(dem_esp)
demesp = dem_esp

# Change all non-NA values to 1 
demesp[!is.na(values(demesp))] <- 1

# Set landpixels to NA and nonlandpixels to 1
demespna = mask(is.na(demesp), demesp, maskvalue=1, updatevalue=NA)
plot(demespna)

# Compute distance
demespdist = distance(demespna)

# Set all nonlandpixels back to NA
demespdist = demespdist*demesp

# Convert dist(m) to km
demespdist = demespdist*0.001

# E9. Try to modify the approach used in the above exercise by weighting the distance 
# raster with the elevation raster; every 100 altitudinal meters should increase the 
# distance to the coast by 10 km. Next, compute and visualize the difference between 
# the raster created using the Euclidean distance (E7) and the raster weighted by elevation.

# Combine distance raster and elevation raster into one raster stack
demespstk = c(demespdist, dem_esp)

inc_dist = function(dist, elev){
  times = floor(elev/100)
  ifelse(times>=1, dist+(10*times), dist)
}

# Apply wighting function to distance raster
demdistwghtd = lapp(demespstk, fun=inc_dist)

par(mfrow=c(2,2))

plot(demespdist, main="Dist to coastline (km)")
plot(dem_esp, main="DEM")
plot(demdistwghtd, main="Weighted dist to \ncoastline by DEM")
