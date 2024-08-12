# GEOMETRY OPERATIONS

# Prerequisites
library(sf)
library(terra)
library(dplyr)
library(spData)
library(spDataLarge)

# Geometric operations on vector data
# This section is about operations that in some way change the geometry of vector (sf) objects. 

# Simplification - a process for generalization of vector objects 
# (lines and polygons) usually for use in smaller scale maps.
# Another reason for simplifying objects is to reduce the amount of memory, 
# disk space and network bandwidth they consume: 
# it may be wise to simplify complex geometries before publishing them as interactive maps.

# To simplify the river Seine and its tributaries. 
seine_simp = st_simplify(seine, dTolerance = 2000)  # 2000 m dTolerance to control the level of generalization in map units
plot(seine_simp) # a copy of the original object but with fewer vertices

# This is apparent, with the result being visually simpler 
# and consuming less memory than the original object, as verified below:
object.size(seine)
object.size(seine_simp)

# Simplification is also applicable for polygons. This is illustrated using us_states
# representing the contiguous US
us_states_simp1 = st_simplify(us_states, dTolerance = 100000)  # 100 km
plot(us_states_simp1)

# A limitation of st_simplify is that it simplifies objects on a per-geometry basis
# This means the 'topology' is lost resulting in overlapping and 'holey' areal units
# ms_simplify() from rmapshaper overcomes these limitations

# Simplify us_states keeping only 1% of the vertices of the input
# proportion of points to retain (0-1; default 0.05)
# The number of objects remains intact because we set keep_shapes=T
us_states_simp2 = rmapshaper::ms_simplify(us_states, keep = 0.01,
                                          keep_shapes = TRUE)
plot(us_states_simp2)

# Another method is implemented in the smoothr pkg
# Like st_simplify it does not preserve the topology 

# using Gaussian kernel regression to smooth the borders of US states by using method=ksmooth
us_states_simp3 = smoothr::smooth(us_states, method = "ksmooth", smoothness = 6)
plot(us_states_simp3)

# CENTROIDS
# Generate centroids for regions in New Zealand and tributaries to the River Seine
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)

# Sometimes the geographic centroid falls outside the boundaries of their parent objects 
# In such cases point on surface operations can be used to guarantee the point will be in the parent object
nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

# BUFFERS - are polygons representing the area within a given distance of a geometric feature
# Buffering tends to be used for geographic data analysis -
# regardless of whether the input is a point, line or polygon, the output is a polygon.
seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

# st_buffer has a few additional args

# AFFINE TRANSFORMATIONS - any transformation that preserves lines n parallelism
# However angles/lengths are not necessarily preserved
# Many affine transformations are applied when reprojecting or 
# improving the geometry that was created based on a distorted or wrongly projected map.
# Affine transformations also include shifting, scaling. 

# The sf pkg implements affine transformation for objects of class sfg/sfc
nz_sfc = st_geometry(nz)

# Shifting moves every point by the same distance in map units. It could be done by adding 
# a numerical vector to a vector object. For example, the code below shifts all 
# y-coordinates by 100,000 meters to the north, but leaves the x-coordinates untouched 
nz_shift = nz_sfc + c(0, 100000)

# Scaling enlarges/shrinks objects by a factor. It can be applied either globally/locally
# Global scaling increases or decreases all coordinates values in relation to the origin 
# coordinates, while keeping all geometries topological relations intact.
# It can be done by subtraction or multiplication of a sfg or sfc object.

# Local scaling treats geometries independently and requires points around 
# which geometries are going to be scaled, e.g., centroids. 

# In the example below each geometry is shrunk by a factor of 2 around the centroids
# To achieve that each object is firstly shifted in a way that its center has coords of 0,0 (nz_sfc - nz_centroid_sfc)
# Next the sizes of the geometries are reduced by half (*0.5)
# Finally each object's centroid is moved back to the input data coords
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc

# Rotation of a 2D coords requires a rotation matrix
# The rotation matrix can be implemented in R as follows:
# The rotation function accepts one argument a - a rotation angle in degrees.
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

# Rotation can be done around selected points such as centroids
nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc

# Finally the newly created geometries can replace the old ones with the st_set_geometry func
nz_scale_sf = st_set_geometry(nz, nz_scale)

# CLIPPING
# Spatial clipping - a form of spatial subsetting that involves changes to the geometry 
# columns of at least some of the affected features. Clipping can only apply to features
# more complex than points - lines, polygons n their multi-equivalents

b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b, border = "grey")
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3) # add text

# Imagine you want to select not one circle/the other but the space coverd by both circles
x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b, border = "grey")
plot(x_and_y, col = "lightgrey", border = "grey", add = TRUE) # intersecting area

# The subsequent code chunk demonstrates how this works 

# SUBSETTING & CLIPPING
# Clipping objects can change their geometry but it can also subset objects, 
# returning only features that intersect (or partly intersect) with a clipping/subsetting object.
# To illustrate this point, we will subset points that cover the bounding box of the circles x and y
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2024)
p = st_sample(x = box, size = 10)   # generate a simple random distribution of points within the bb of the circles
p_xy1 = p[x_and_y]
plot(box, border = "grey", lty = 2)
plot(x, add = TRUE, border = "grey")
plot(y, add = TRUE, border = "grey")
plot(p, add = TRUE, cex = 3.5)
plot(p_xy1, cex = 5, col = "red", add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"), cex = 3)

# The code chunk below demonstrates 3 ways to achieve the same result:
# 1 We can use the intersection of x and y as a subsetting object directly
# 2 We can also find the intersection between the input points represented by p and the subsetting/clipping object x_and_y
# This second approach will return features that partly intersect with x_and_y but with modified geometries for spatially extensive features that cross the border of the subsetting object. 
# 3 The third approach is to create a subsetting object using the binary spatial predicate st_intersects()

# way #1
p_xy1 = p[x_and_y]
# way #2
p_xy2 = st_intersection(p, x_and_y)
# way #3
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] & 
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy3 = p[sel_p_xy]

# GEOMETRY UNIONS
# As we've seen, spatial aggregation can silently dissolve the geometries of touching polygons 
# in the same group. Below we aggregate the 48 US states n the district of columbia into 4 regions
# using base n dplyr functions
regions = aggregate(x = us_states[, "total_pop_15"], 
                    by = list(us_states$REGION),
                    FUN = sum, na.rm = TRUE)
regions2 = us_states |> 
  group_by(REGION) |>
  summarize(pop = sum(total_pop_15, na.rm = TRUE))

plot(regions2)

# Behind the scenes, both aggregate & summarize combine the geometries and dissolve the boundaries 
# btw them using st_union. This is demonstrated below which creates a united western US
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)
plot(us_west_union)

# the function can take any two geometries and unite them:
texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)
plot(texas_union)

# TYPE TRANSFORMATIONS
# Geometry casting is a powerful operation that enables transformation of the geometry type
# st_cast behaves differently on sfg objects, sfc and simple feature objects
# Let's create a multipoint to illustrate how geometry casting works on sfg objects:
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))
plot(multipoint)

# In this case, st_cast can be used to transform the multipoint into a line/polygon 
linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

plot(linestring); plot(polyg)

# The transformation can also be reversed using st_cast
multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")
all.equal(multipoint, multipoint_2)
all.equal(multipoint, multipoint_3)

# Let's try to apply geometry type transformations on a new object 
# multilinestring_sf as an example
multilinestring_list = list(matrix(c(1, 4, 5, 3), ncol = 2), 
                            matrix(c(4, 4, 4, 1), ncol = 2),
                            matrix(c(2, 4, 2, 2), ncol = 2))
multilinestring = st_multilinestring(multilinestring_list)
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))
multilinestring_sf #has only one row that defines all the lines

# This restricts the number of operations that can be done, 
# for example it prevents adding names to each line segment or 
# calculating lengths of single lines.

# Separate this multilinestring into 3 linestrings
linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2

# The newly created object allows for attribute creation and length measurement
linestring_sf2$name = c("Riddle Rd", "Marshall Ave", "Foulke St")
linestring_sf2$length = st_length(linestring_sf2)
linestring_sf2

# Geometric operations on raster data
