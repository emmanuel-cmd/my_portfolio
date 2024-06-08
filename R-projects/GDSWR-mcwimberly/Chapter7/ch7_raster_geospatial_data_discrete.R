# RASTER GEOSPATIAL DATA - DISCRETE
library(RColorBrewer) 
library(ggplot2)      
library(colorspace)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(terra)

# Importing and Mapping Land Cover data
nlcd19 <- rast("NLCD_2019_Land_Cover_Walton.tiff")
class(nlcd19)

ncol(nlcd19)
nrow(nlcd19)
res(nlcd19)

# Convert raster to dataframe for mapping
source("rasterdf.R")
nlcd19_df <- rasterdf(nlcd19)

# By default, the geom_raster generates a raster map with a continous color ramp 
# in shades of blue
ggplot(data = nlcd19_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value))

# To generate a better looking map, it will be necessary to assign each land cover code
# in the map a unique name n color for display
LCcodes <- unique(nlcd19)[, 1]
LCcodes

# Each landcover classes - names
LCnames <-c(
  "Water",
  "DevelopedOpen",
  "DevelopedLow",
  "DevelopedMed",
  "DevelopedHigh",
  "Barren",
  "DeciduousForest",
  "EvergreenForest",
  "MixedForest",
  "ShrubScrub",
  "GrassHerbaceous",
  "PastureHay",
  "CultCrops",
  "WoodyWetlands",
  "EmergentHerbWet"
)

# Extract color table information from the SpatRaster object
nlcdcols <- data.frame(coltab(nlcd19))
nlcdcols <- nlcdcols[LCcodes + 1,]    # extract rows associated with the NLCD landcover codes
LCcolors <- rgb(red = nlcdcols$red,   # convert RGB info into hexadecimmal color codes
                green = nlcdcols$green,
                blue = nlcdcols$blue,
                names = as.character(LCcodes),
                maxColorValue = 255)
LCcolors

# Plot the NLCD landcover raster
ggplot(data = nlcd19_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = LCcolors,
                    labels = LCnames,
                    na.translate = FALSE) + # NA value should not show up in the legend
  coord_sf(expand = FALSE) +
  theme_void()

# It is also possible to zoom in and map a portion of a raster data without modifying the 
# underlying data. Here a subset of Walton County is mapped by setting the x & y limits 
# of the plot using the coord_sf function
ggplot(data = nlcd19_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = LCcolors,
                    labels = LCnames,
                    na.translate = FALSE) +
  coord_sf(expand = FALSE, 
           xlim = c(1114000, 1125000), 
           ylim = c(1260000, 1270000)) + 
  theme_void()

# Reclassifying Raster Data
# Land cover patterns can be seen more clearly if the land cover classes are 
# aggregated into a smaller number of broader land cover types.
LCcodes

newclas <- c(1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 7, 7) # mew landcover classes
lookup <- data.frame(LCcodes, newclas)
nlcd19_rc <- classify(nlcd19, lookup)  # assign a new landcover class to every pixel in Walton County
newnames <- c("Water",
              "Developed",
              "Barren",
              "Forest",
              "GrassShrub",
              "Cropland",
              "Wetland")
newcols <- c("mediumblue", 
             "firebrick2", 
             "gray60", 
             "darkgreen", 
             "yellow2", 
             "orange4", 
             "paleturquoise2")


#nlcd19_rcc <- subst(nlcd19_rc, 1:7, as.factor(newnames))

# The same ggplot code can be used to generate a categorical map with the new reclassified
# raster dataset
nlcd19_rc_df <- rasterdf(nlcd19_rc)

ggplot(data = nlcd19_rc_df) +
  geom_raster(aes(x = x, y = y, fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()

# Desaturate the colors used in the map - mixes gray into the colors to make them appear softer and more muted
newcols2 <- desaturate(newcols, amount = 0.4)

# The map of the reclassified landcover data can now be recreated using the desaturated color palette
ggplot(data = nlcd19_rc_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols2,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()


# Another visualization technique is the "small multiples" approach in which each landcover class is displayed in a separate map
# To generate this map, a separate binary raster must be generated for each of the landcover types
developed <- nlcd19_rc == 2   # developed class
summary(developed)

# To convert multiple classes, the segregate function can be used to convert every unique class 
# in a discrete raster to a 1/0 binary raster. 
nlcd19_stk <- segregate(nlcd19_rc)
nlcd19_stk <- nlcd19_stk[[-1]] # drop water layer
names(nlcd19_stk) <- newnames[-1]

# Convert to dataframe for mapping
nlcd19_stk_df <- rasterdf(nlcd19_stk)
summary(nlcd19_stk_df)

# These data can be used to generate a series of binary maps - one for each of the 6 landcover classes
ggplot(data = nlcd19_stk_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) +
  scale_fill_manual(name = "Present", 
                    values = c("gray80", "gray20"),
                    labels = c("No", "Yes"),
                    na.translate = FALSE) +  
  facet_wrap(facets = vars(variable), ncol = 3) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(strip.text.x = element_text(size=12, face="bold"),
        legend.position="bottom")

# Focal Analysis of Raster Data
# In most cases, the result of a focal analysis is a smoothed version of the input raster
# where variation at smaller scales than the summary window is removed and broader scale 
# spatial patterns are emphasized

# To carry out a focal analysis, a weights object must be defined using the focalMat function
forest <- nlcd19_stk[["Forest"]]
fwts100 <- focalMat(forest, d=100, type = "circle")
fwts500 <- focalMat(forest, d=500, type = "circle")
fwts1000 <- focalMat(forest, d=1000, type = "circle")
fwts2000 <- focalMat(forest, d=2000, type = "circle")

# Then the focal() function is called with the weights objects and a summary function as arguments. 
# By summing across the weights, the focal() function computes the proportion of each land cover 
# class within the specified windows.

for_100 <- focal(forest, w=fwts100, fun=sum)
for_500 <- focal(forest, w=fwts500, fun=sum)
for_1000 <- focal(forest, w=fwts1000, fun=sum)
for_2000 <- focal(forest, w=fwts2000, fun=sum)
focal_stk <- c(for_100, for_500, for_1000, for_2000)
names(focal_stk) <- c("100m", "500m", "1000m", "2000m")

# Changing the size of the focal window changes the degree of smoothing in the resulting maps
# similar to te bandwidth of a kernel density analysis

focal_stk_df <- rasterdf(focal_stk)

# NA values are returned when window size extends beyond the boundary of the raster dataset
# These NA values are displayed in the plot with the gray boundary outside of the dataset 
# increasing in width as the window size increases
ggplot(data = focal_stk_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_distiller(name = "Density", 
                       palette = "YlOrRd") +
  facet_wrap(facets = vars(variable), ncol = 2) +
  coord_sf(expand = TRUE) +
  theme_void() +
  theme(strip.text.x = element_text(size=12, face="bold"),
        legend.position="bottom")

# LAND COVER CHANGE ANALYSIS
nlcd_stk <- rast(c("NLCD_2001_Land_Cover_Walton.tiff",
                   "NLCD_2004_Land_Cover_Walton.tiff",
                   "NLCD_2006_Land_Cover_Walton.tiff",
                   "NLCD_2008_Land_Cover_Walton.tiff",
                   "NLCD_2011_Land_Cover_Walton.tiff",
                   "NLCD_2013_Land_Cover_Walton.tiff",
                   "NLCD_2016_Land_Cover_Walton.tiff",
                   "NLCD_2019_Land_Cover_Walton.tiff"))

nlcd_rc <- classify(nlcd_stk, lookup)
names(nlcd_rc) <- c("2001", "2004", "2006", "2008", "2011", 
                    "2013", "2016", "2019")

# Using ggplot to map 4 years of time series data
nlcd_rc_df  <- rasterdf(nlcd_rc[[c("2001", "2008", 
                                   "2013", "2019")]])

ggplot(data = nlcd_rc_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols2,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = TRUE) +
  facet_wrap(facets = vars(variable), ncol = 2) +
  theme_void() +
  theme(strip.text.x = element_text(size = 12, face="bold"),
        legend.position="bottom")

# When working with landcover data, it is usually important to know which landcover
# classes are increasing/decreasing and by how much
# One way to visualize the changes more effectively is to calculate the total area
# of each landcover class in each year and plot the changes over time

# The Walton County boundaries are extracted from the shapefile of Georgia counties 
gacounty <- st_read("GA_SHP.shp", quiet = TRUE)
gacounty <- st_transform(gacounty, crs(nlcd_rc))
walton <- filter(gacounty, NAME10 == "Walton")

# To restrict the analysis to only Walton county
nlcd_rc_crp <- crop(nlcd_rc, vect(walton))
nlcd_rc_msk <- mask(nlcd_rc_crp, vect(walton))

nlcd_msk_df <- rasterdf(nlcd_rc_msk[[c("2001", "2008", 
                                       "2013", "2019")]])

ggplot(data = nlcd_msk_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols2,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = TRUE) +
  facet_wrap(facets = vars(variable), ncol = 2) +
  theme_void() +
  theme(strip.text.x = element_text(size=12, face="bold"),
        legend.position="bottom")

# The freq() function can be used to extract the number of cells in each landcover class
# from each layer in the raster stack. The output is a dataframe with one row for each 
# combination of year and land cover class
freq_df <- freq(nlcd_rc_msk, usenames=TRUE)
glimpse(freq_df)

# Before plotting the dataframe, a series of dplyr functions is used to modify the columns
nlcd_chg <- freq_df %>%
  mutate(km2 = count * 900 / 1000000,
         class = factor(value,
                        levels = 1:7,
                        labels = newnames),
         year = as.numeric(layer))

# The change in area over time for each class can be displayed as a line graph
ggplot(data = nlcd_chg) +
  geom_line(aes(x = year, y = km2, color = class)) +
  geom_point(aes(x = year, y = km2, color = class)) +
  scale_color_manual(name = "Land Cover Class",
                     values = newcols) +
  labs(x = "Year", y = expression("Area(km"^2*")")) + # a star is required after the superscript expression to indicate that it will be followed by more text
  theme_classic()

# Another way to graph the changes is using facet_wrap to create a separate 
# subplot for each landcover class
ggplot(data = nlcd_chg) +
  geom_line(aes(x = year, y = km2)) +
  facet_wrap(facets = vars(class), ncol = 4) +
  labs(x = "Year", y = expression("Area(km"^2*")")) +
  theme_bw()

# To see the trends of the less common land cover classes more clearly, 
# the scale of the y-axis can be changed to vary freely with the range of values for each class
ggplot(data = nlcd_chg) +
  geom_line(aes(x = year, y = km2)) +
  facet_wrap(facets = vars(class), 
             scales = "free_y", 
             ncol = 4) +
  labs(x = "Year", y = expression("Area(km"^2*")")) +
  theme_bw()

# Land Cover Transition Matrices
# Quantifying land cover change btw two years is through a transition matrix
changeras <- c(nlcd_rc_msk[["2001"]], 
               nlcd_rc_msk[["2019"]])
changeout <- crosstab(changeras)
class(changeout)  # the function outputs the results as a table object which is then converted to a tibble dataframe

changedf <- as_tibble(changeout)
class(changedf)

changedf

# To make the ouput easier to work with, the dataframe is modified using dplyr functions
# The class codes are converted to factors and labeled with the abbreviated class names

shortnames <- c("Wat", 
                "Dev", 
                "Bare", 
                "For", 
                "Grass", 
                "Crop", 
                "Wet")

changedfn <- changedf %>%
  mutate(X2001 = factor(X2001,
                        levels = 1:7,
                        labels = shortnames),
         X2019 = factor(X2019,
                        levels = 1:7,
                        labels = shortnames),
         ha = n * 900/10000) %>%
  group_by(X2001) %>%
  mutate(tot2001 = sum(ha),   # when sum is used in mutate after data is grouped, they return one value for each row in the dataframe instead of one value per group
         perc = 100 * ha / tot2001) # then the area of each transition is computed as a percentage of 2001 area of each landcover class

changedfn

# These transitions are ofen displayed as change matrix. This is a n x n matrix where n is the number of 
# land cover classes
changemat <- matrix(changedfn$ha, 
                    nrow = 7, 
                    ncol = 7)
rownames(changemat) <- shortnames
colnames(changemat) <- shortnames
