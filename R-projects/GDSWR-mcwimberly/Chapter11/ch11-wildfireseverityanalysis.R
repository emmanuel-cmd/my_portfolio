# Application - Wildfire Severity Analysis
library(tidyverse)
library(terra)
library(ggspatial)
library(sf)
library(cowplot)
library(mgcv)
library(visreg)
source("rasterdf.R")

# REMOTE SENSING IMAGE ANALYSIS
# Import two satellite images - one for before and another after the fire
landsat_pre <- rast("co4058910540420120609_20110912_l5_refl.tif")
landsat_post <- rast("co4058910540420120609_20130901_l8_refl.tif")

# the landsatpre data contains Landsat5 data
nrow(landsat_pre)
ncol(landsat_pre)
res(landsat_pre)
nlyr(landsat_pre) ## 5
ext(landsat_pre)[1:4]

# the landsatpost data contains Landsat8 data
nrow(landsat_post)
ncol(landsat_post)
res(landsat_post)
nlyr(landsat_post) ## 8
ext(landsat_post)[1:4]

# R is not as powerful as dedicated RS software however it is helpful to quickly view RS images in R
# Display a false color composite of the img with SWIR displayed as red, NIR displayed as green and green band displayed as blue
plotRGB(landsat_pre, 
        r = 6, 
        g = 4, 
        b = 2)

# Large areas of trees were killed by the fire in 2012, and soils in the burned areas were blackened by char. 
# These areas reflect less near-infrared radiation and more short-wave infrared radiation. 
plotRGB(landsat_post, 
        r = 6, 
        g = 4, 
        b = 2)

# The NBR index measures the contrast btw NIR which is highest in vegetated areas
# and SWIR which is highest in unvegetated areas with blackened or exposed soils
# The NBR is highest in areas with undisturbed vegetation and lowest in areas where 
# fire has killed most or all of the vegetation and blackened the soil
nbr_pre <- 1000 * (landsat_pre[[4]] - landsat_pre[[6]]) / 
  (landsat_pre[[4]] + landsat_pre[[6]])
nbr_post <- 1000 * (landsat_post[[4]] - landsat_post[[6]]) / 
  (landsat_post[[4]] + landsat_post[[6]])
dnbr <- nbr_pre - nbr_post

# The pre-fire and post-fire NBR rasters are combined into multilayer raster object n mapped
nbr_stack <- c(nbr_pre, nbr_post)
names(nbr_stack) <- c("Pre-fire NBR", "Post-fire NBR")
nbr_stack_df <- rasterdf(nbr_stack)

# Because increasing levels of NBR are generally associated with increasing tree cover, 
# a light-yellow-to-dark-green color ramp is used. 
ggplot(nbr_stack_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) + 
  scale_fill_gradient(name = "NBR", 
                      low = "lightyellow", 
                      high = "darkgreen") +
  coord_sf(expand = FALSE) +
  annotation_scale(location = 'bl') +
  facet_wrap(facets = vars(variable), 
             ncol = 1) +
  theme_void()

# The DNBR index highlights locations where vegetation changed after the fire
# DNBR is centered at 0 with positive values indicating decreases in NBR and 
# negative values indicating increases the scale_fill_gradient2 is used to 
# create a bicolor ramp centered at 0
dnbr_df <- rasterdf(dnbr)

ggplot(dnbr_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) + 
  scale_fill_gradient2(name = "DNBR", 
                       low = "blue", 
                       high = "red",
                       midpoint = 0) +
  coord_sf(expand = F) +
  annotation_scale(location = 'bl') +
  theme_void()

# Burn Severity Classification
# Create a lookup table based on range of values and classify the DNBR image using this table
rclasbs <- matrix(c(-Inf, -970, NA,  # Missing data
                  -970, -100, 5,   # Increased greenness
                  -100, 80, 1,     # Unburned
                  80, 265, 2,      # Low severity
                  265, 490, 3,     # Moderate severity
                  490, Inf, 4),    # High severity
                ncol = 3, byrow = T)

severity <- classify(dnbr, rclasbs)

# Inferring fire severity from vegetation change is based on the assumption that the observed changes
# are the result of the wildfire and not other drivers of landcover and landuse change
# Therefore it is advisable to mask the fire severity raster to the known perimeter of the wildfire
# This can be accomplished by reading in the fire boundary, rasterizing it to match the classified 
# fire severity raster, and masking the areas outside of the fire perimeter.
fire_bndy <- st_read("co4058910540420120609_20110912_20130901_bndy.shp", quiet = TRUE)
bndy_rast <- rasterize(vect(fire_bndy), 
                       severity, 
                       field = "Event_ID")
severity <- mask(severity, bndy_rast)

# Rapid postfire growth can occur where there was sparse vegetation prior to the fire
# and burning triggers a flush of grasses and other herbaceous vegetation growth
SCcolors = c("darkgreen", 
             "cyan3", 
             "yellow", 
             "red", 
             "green")
SCnames = c("Unburned", 
            "Low", 
            "Moderate", 
            "High", 
            "> Green")
severity_df <- rasterdf(severity)

ggplot(severity_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Severity Class",
                    values = SCcolors,
                    labels = SCnames,
                    na.translate = FALSE) +
  annotation_scale(location = 'bl') +
  coord_fixed(expand = F) +
  theme_void()

# The Wildland-Urban Interface (WUI)
# We'll analyse these burn severity patterns by overlaying them on several geospatial datasets
# The WUI is defined as the zone where houses and other infrastructures are located in close
# vicinity to wildland vegetation. The WUI can be identified via geospatial analysis that overlays
# vegetation data from the NLCD with housing density data from the US Census
# Import the WUI data
wui <- st_read("co_wui_cp12_clip.shp", quiet=TRUE)

# The WUI data will need to be rasterized n cropped to match the geometric characteristics of the 
# fire severity dataset. Both dataset are in a similar Albers Equal Area projection although there 
# is a minor difference in the definition of the spheroid
writeLines(st_crs(wui)$WktPretty)
writeLines(st_crs(severity)$WktPretty)

# Reproject the WUI data and crop to severity data extent
wui_reproj <- st_transform(wui, crs(severity))
wui_crop <- st_crop(wui_reproj, severity)

# The map shows the WUI polygons which correspond to the US Census blocks in the different WUI classes
# Intermix characterized by houses and other buildings scattered throughout wildlands at low densities
# whereas the interface is the zone where denser urban settlements are adjacent to wildland vegetation
ggplot(data = wui_crop) +
  geom_sf(aes(fill = as.character(WUIFLAG10))) +
  scale_fill_manual(name = "WUI Class",
                    values = c("Gray", "Orange", "Red"),
                    labels = c("Non-WUI", "Intermix", "Interface"), 
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()

# Convert the vector WUI dataset into a raster layer with the same geometric characteristics 
# as the fire severity raster layer. 
wui_rast <- rasterize(vect(wui_crop), 
                      severity, 
                      field = "WUIFLAG10")
wui_rast_df <- rasterdf(wui_rast)

ggplot(wui_rast_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) +
  scale_fill_manual(name = "WUI Class",
                    values = c("Gray", "Orange", "Red"),
                    labels = c("Non-WUI", "Intermix", "Interface"), 
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()

# Distance from the WUI is calculated using the distance() func
# Distance is calculated for all cells with an NA value to the nearest cell that does not have an NA value.
# Therefore, the WUI raster dataset must first be reclassified into a raster where all WUI cells have a value and all non-WUI cells are NA.
wui_na <- ifel(wui_rast == 0, NA, 1)
wui_dist <- distance(wui_na)

# The raster containing distance values is then reclassified into a discrete raster with 4 distance classes
rclas <- matrix(c(-Inf, 0, 1,
                  0, 1000, 2,
                  1000, 3000, 3,
                  3000, Inf, 4),
                ncol = 3, byrow = T)
wui_rcls <- classify(wui_dist, rcl = rclas)
wui_rcls_df <- rasterdf(wui_rcls)

ggplot(wui_rcls_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.factor(value))) +
  scale_fill_manual(name = "WUI Distance",
                    values = c("Gray", 
                               "Red", 
                               "Orange", 
                               "Yellow"),
                    labels = c("WUI", 
                               "0-1000", 
                               "1000-3000", 
                               "> 3000"), 
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()

# crosstab to calculate the distribution of fire severity class for every WUI distance class
# give the output shorter names, convert counts of 30m square cells to hectares and convert
# the WUI distance and severity variables to labeled factors
wui_xtab <- crosstab(c(wui_rcls, 
                       severity))
wui_df <- as_tibble(wui_xtab)

wui_df <- wui_df %>%
  rename(wuidist = 1, sev = 2, ha = 3) %>%
  mutate(ha = ha * 900 / 10000,
         wuidist = factor(wuidist,
                          levels = 1:4,
                          labels =  c("WUI", 
                                      "0-1000", 
                                      "1000-3000", 
                                      "> 3000")), 
         sev = factor(sev,
                      levels = 1:5,
                      labels = SCnames))

# A bar chart comparing these categories shows that there is an association btw distance to the WUI and fire severity
# Inside the WUI and close to the WUI unburned and low severity are the most common severity classes
# However the relative amounts of moderate n high severity classes increase with distance from the WUI
# and at the farthest distance high severity is the most common class
ggplot(data = wui_df) +
  geom_bar(aes(x = wuidist, 
               y = ha, 
               fill = sev), 
           position = "dodge", 
           stat = "identity") +
  scale_fill_manual(name = "Severity Class",
                    values = SCcolors) +
  labs(x = "WUI Distance Class", 
       y = "Hectares") +
  theme_bw()

# Topographic Effects
# Elevation is often related to fire severity because the climate is strongly influenced by elevation
# and different forest vegetation types n fire regimes are found at different elevations as a result
# The elevation data is obtained from the NED through the USGS' National map
elev <- rast("USGS_1_n41w106_20220331.tif")
writeLines(st_crs(elev)$WktPretty)

# Reproject to match fire severity data
elev_crop <- project(elev, 
                     severity, 
                     method = "bilinear")

# The map shows that elevation in the Colorado Front Range increases from east to west
# and there are several drainages and ridgelines within the boundary of the High park fire
elev_crop_df <- rasterdf(elev_crop)
ggplot(elev_crop_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(name = "Elevation (m)",
                       palette = "Oranges") +
  geom_sf(data = fire_bndy, fill = NA) +
  coord_sf(expand = FALSE) +
  theme_void()

# Slope angle can be calculated using the terrain function and then mapped
slopedeg <- terrain(elev_crop, 
                    v="slope", 
                    unit="degrees")

slopedeg_df <- rasterdf(slopedeg)
# Slope angle within the boundary of the High Park fire
ggplot(slopedeg_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(name = "Slope (degrees)",
                       palette = "Oranges") +
  geom_sf(data = fire_bndy, fill = NA) +
  coord_sf(expand = FALSE) +
  theme_void()

# Slope aspect is similarly calculated using the terrain() function with units as radians.
# A north aspect will have a value of 0, an east aspect a value of 0.5π, a south aspect a value of π, and a west aspect a value of 1.5π.
aspect <- terrain(elev_crop, v="aspect", unit="radians")
nsaspect <- cos(aspect) # convert the circular aspect angle into a linear north-south index where 1 indicates north-facing slopes and -1 indicates south-facing slopes
nsaspect_df <- rasterdf(nsaspect)

ggplot(nsaspect_df) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  scale_fill_distiller(name = "Aspect (N-S index)",
                       palette = "Oranges") +
  geom_sf(data = fire_bndy, fill = NA) +
  coord_sf(expand = FALSE) +
  theme_void()

# the sin func is used to create an east-west index where east-facing slopes have a value of 1
# and west-facing slopes have a value of -1
ewaspect <- sin(aspect)
ewaspect_df <- rasterdf(ewaspect)

ggplot(ewaspect_df) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  scale_fill_distiller(name = "Aspect (E-W index)",
                       palette = "Oranges") +
  geom_sf(data = fire_bndy, fill = NA) +
  coord_sf(expand = FALSE) +
  theme_void()

# To analyze the relationships between fire severity and these topographic indices, 
# a sample of points inside the boundary of the High Park fire is needed. 
# The first step is to combine DNBR with the topographic indices into a multilayer raster object.
fire_stack <- c(dnbr,
                elev_crop,
                slopedeg,
                nsaspect,
                ewaspect)

# Use st_sample to generate a set of random points
set.seed(23456)
sample_pts <- st_sample(
  fire_bndy,     # constrained to fall within this polygon
  type = "SSI",  # a simple sequential inhibition process will be used to generate the points
  r = 500,       # min distance btw points
  n = 300        # number of points to sample
)
st_crs(sample_pts)

st_crs(sample_pts) <- st_crs(fire_bndy)

# Generalized Additive Modelling
# Using widely-spaced points help to ensure broad coverage n maximize the independence of each sample

# Sample points n fire boundary overlaid on the DNBR index for the High Park Fire
ggplot(dnbr_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) + 
  scale_fill_gradient2(name = "DNBR", 
                       low = "blue", 
                       high = "red",
                       midpoint = 0) +
  geom_sf(data = fire_bndy, fill = NA) +
  geom_sf(data = sample_pts) +
  coord_sf(expand = F) +
  theme_void()

# Use extract() to extract raster data at these point locations and the columns of the resulting
# dataframe are renamed
fire_pts <- extract(fire_stack, vect(sample_pts))

fire_pts <- rename(fire_pts,
                   dnbr = 2,
                   elevation = 3,
                   slope = 4,
                   nsaspect = 5,
                   ewaspect = 6)
# Implement a GAM 
fire_gam <- gam(dnbr ~ 
                  s(elevation) +  # models the dependent var as a smooth thin-plate spline function
                  s(slope) + 
                  s(nsaspect) + 
                  s(ewaspect),
                data = fire_pts)

class(fire_gam)
summary(fire_gam) # a table of test statistics and pvalues for each independent variables but there are no coefficient values as with a linear model
# edf (estimate of degree of freedom) provides information about the degree of non-linearity 
# in the relationships with higher values indicating more complex non-linear relationships.

# To understand the smoothed relationship modelled by GAM, they need to be plotted
# We plot partial residuals as a function of each envt'l variable
# These partial residuals represent the relationships between DNBR and each topographic index 
# after removing the modeled effects of all the other topographic indices
plot(fire_gam, pages=1)

# 1. the relationship with elevation shows a unimodal relationship that peaks at 2700m
# 2. the relationship with slope is also unimodal that peaks at 22°
# 3. the relationship with north-south index is linear with fire severity highest on south-facing aspect
# 4. the relationship with east-west is comparatively weak and does not show a clear pattern

# Partial regression plots can also be generated from the visreg pkg
elev_gg <- visreg(fire_gam, 
                  "elevation", 
                  gg = TRUE) +
  theme_bw()
slope_gg <- visreg(fire_gam, 
                   "slope", 
                   gg = TRUE) +
  theme_bw()
nsasp_gg <- visreg(fire_gam, 
                   "nsaspect", 
                   gg = TRUE) +
  theme_bw()
ewasp_gg <- visreg(fire_gam,
                   "ewaspect", 
                   gg = TRUE) +
  theme_bw()

# Then the cowplot pkg can be used to arrange these plots into a larger multipanel graphic
plot_grid(elev_gg, slope_gg, nsasp_gg, ewasp_gg,
          labels = c("A)", "B)", "C)", "D)",
                     label_size = 12),
          ncol = 2, 
          hjust = 0, 
          label_x = 0, 
          align = "hv")

# PRACTISE
# One of the reasons why fire severity was lowest near the WUI may have been that the WUI was 
# concentrated at lower elevations where the forest vegetation is often resilient to fire. 
# Compare these two variables by classifying elevation into four categories and generating a plot 
# like (Figure 11.9) that compares elevation with distance from the WUI.

# Reclassify elevation data - first define elev categories
elevrclas <- matrix(c(1500, 2000, 1,
                  2000, 2500, 2,
                  2500, 3000, 3,
                  3000, 3500, 4),
                  ncol = 3, byrow = T)

elev_rcls = classify(elev_crop, rcl = elevrclas)
elev_rcls_df = rasterdf(elev_rcls)

ggplot(elev_rcls_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.factor(value))) +
  scale_fill_manual(name = "Elevation",
                    values = c("Gray", 
                               "Red", 
                               "Orange", 
                               "Yellow"),
                    labels = c("1500-2000", 
                               "2000-2500", 
                               "2500-3000", 
                               "3000-3500"), 
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()

# calculating the distribution of elevation for every WUI class
wuielev_xtab = crosstab(c(wui_rcls, elev_rcls))
wuielev_df = as_tibble(wuielev_xtab)

wuielev_df <- wuielev_df %>%
  rename(wuidist = 1, elev = 2, ha = 3) %>%
  mutate(ha = ha * 900 / 10000,
         wuidist = factor(wuidist,
                          levels = 1:4,
                          labels =  c("WUI", 
                                      "0-1000", 
                                      "1000-3000", 
                                      "> 3000")), 
         elev = factor(elev,
                      levels = 1:4,
                      labels = c("1500-2000", 
                                 "2000-2500", 
                                 "2500-3000", 
                                 "3000-3500")))

# A bar chart comparing these classes 
ggplot(data = wuielev_df) +
  geom_bar(aes(x = wuidist, 
               y = ha, 
               fill = elev), 
           position = "dodge", 
           stat = "identity") +
  scale_fill_manual(name = "Elevation",
                    values = SCcolors) +
  labs(x = "WUI Distance Class", 
       y = "Hectares") +
  theme_bw()

# Another metric of burn severity is the relative differenced normalized burn ratio (RDNBR), which normalizes the DNBR by dividing it by the pre-fire NBR. 
# The RDNBR is calculated as rdnbr = dnbr/sqrt(abs(nbr_pre)). 
# Calculate the RDNBR for the High Park fire and generate a composite figure that includes both DNBR and RDNBR mapped as continuous variables. 
# Compare the maps and assess the similarities and differences between the two indices.
nbr_pre_maskd = mask(nbr_pre, bndy_rast)
dnbr_maskd = mask(dnbr, bndy_rast)

rdnbr = dnbr_maskd/(sqrt(abs(nbr_pre_maskd)))

# Classify RDNBR based on earlier defined categories
rdnbr_sev = classify(rdnbr, rclas)

# 
dnbr_stack = c(dnbr, rdnbr_sev)
names(dnbr_stack) <- c("DNBR", "RDNBR")
dnbr_stack_df <- rasterdf(dnbr_stack)

# Because increasing levels of NBR are generally associated with increasing tree cover, 
# a light-yellow-to-dark-green color ramp is used. 
ggplot(dnbr_stack_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) + 
  scale_fill_gradient2(name = "DNBR & RDNBR", 
                       low = "blue", 
                       high = "red",
                       midpoint = 0) +
  coord_sf(expand = FALSE) +
  annotation_scale(location = 'bl') +
  facet_wrap(facets = vars(variable), 
             ncol = 1) +
  theme_void()


# 3. Select and download burn severity data for another wildfire from the MTBS website (https://www.mtbs.gov/) 
# along with elevation data for the same area from the National Map (https://www.usgs.gov/the-national-map-data-delivery). 
# Repeat the analysis of topographic effects on burn severity described in this chapter and see if you obtain 
# similar results in a different location.

# Import severity data for Highline Wildfire Idaho
highline_severity = rast("id4549011522720170729/id4549011522720170729_20160724_20180730_dnbr.tif")

# Import elevation data for the area
idahoelev = rast("USGS_13_n46w116_20220309.tif")

# Check the CRS of these datasets
writeLines(st_crs(highline_severity)$WktPretty)
writeLines(st_crs(idahoelev)$WktPretty)

# Reproject the CRS of elevation data to match the severity data
idahoelev = project(
  idahoelev, 
  highline_severity, 
  "bilinear"
)

# Import the fire boundary
hlf_bndy <- st_read("id4549011522720170729/id4549011522720170729_20160724_20180730_burn_bndy.shp", quiet = TRUE)

hlf_bndy_rast = rasterize(vect(hlf_bndy), 
                          highline_severity, 
                          field = "Event_ID")

# Mask severity data to fire severity boundary
highlinesev_maskd = mask(highline_severity, hlf_bndy_rast)
# idahoelev_maskd = mask(idahoelev, hlf_bndy_rast)

# Reclassify data
highlinesev = classify(highline_severity, rclasbs)

# Visualize the elevation data
idahoelevdf = rasterdf(idahoelev)

ggplot(idahoelevdf) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(name = "Elevation (m)",
                       palette = "Oranges") +
  geom_sf(data = hlf_bndy, fill = NA) +
  coord_sf(expand = FALSE) +
  theme_void()

# Slope angle can be calculated using the terrain func
idelevdeg = terrain(idahoelev, 
                    v="slope", 
                    unit="degrees")
idelevdegdf = rasterdf(idelevdeg)

ggplot(idelevdegdf) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(name = "Slope (degrees)",
                       palette = "Oranges") +
  geom_sf(data = hlf_bndy, fill = NA) +
  coord_sf(expand = FALSE) +
  theme_void()

# Calculate slope aspect 
idelevasp = terrain(idahoelev, v="slope", unit="radians")

# Convert circular aspect into linear north-south index and east-west index
idnsapect = cos(idelevasp)
idewaspect = sin(idelevasp)

# Combine DNBR and the topographic indices into a multilayer raster object
idfire_stack = c(highline_severity, 
                 idahoelev, 
                 idelevdeg, 
                 idnsapect, 
                 idewaspect)

# Generate a set of random points using st_sample
set.seed(23456)
hlfsmpl_pts <- st_sample(
  hlf_bndy,
  type = "SSI",
  r = 500,
  n = 300 
)
st_crs(hlfsmpl_pts)
st_crs(hlfsmpl_pts) <- st_crs(hlf_bndy)

# Sample points and fire boundary overlaid on the DNBR index
highline_sevdf = rasterdf(highlinesev)

ggplot(highline_sevdf) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) + 
  scale_fill_gradient2(name = "DNBR", 
                       low = "blue", 
                       high = "red",
                       midpoint = 0) +
  geom_sf(data = hlf_bndy, fill = NA) +
  geom_sf(data = hlfsmpl_pts) +
  coord_sf(expand = F) +
  theme_void()

# Extract the raster datasets at these points
idfirepts = extract(idfire_stack, hlfsmpl_pts)

idfirepts = rename(idfirepts, 
                   dnbr = 2, 
                   elev = 3, 
                   slope = 4, 
                   nsaspect = 5, 
                   ewaspect = 6)

idfirepts$dnbr = as.numeric(idfirepts$dnbr)

# # Fill NA values in dnbr column with 0
# idfirepts <- idfirepts[!is.na(idfirepts$dnbr),]

# Implement a generalized additive model
idfiregam = gam(dnbr ~ elev + slope + nsaspect + ewaspect, data = idfirepts)

class(idfiregam)
summary(idfiregam)

plot(idfiregam)
