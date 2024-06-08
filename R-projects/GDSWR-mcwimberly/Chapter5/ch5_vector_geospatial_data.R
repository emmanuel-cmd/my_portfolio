library(sf)           
library(rgdal)        
library(ggplot2)      
library(dplyr)        
library(tidyr)        
library(scales)       
library(RColorBrewer) 
library(units)
library(cowplot)

# Load required datasets
okcounty <- st_read("ok_counties.shp", quiet = TRUE)
tpoint <- st_read("ok_tornado_point.shp", quiet = TRUE)
tpath <- st_read("ok_tornado_path.shp", quiet = TRUE)
class(okcounty)
glimpse(okcounty)

# Convert sf object to sp and sp object to sf
okcounty_sp <- as(okcounty, 'Spatial')
class(okcounty_sp)

okcounty_sf <- st_as_sf(okcounty_sp)
class(okcounty_sf)

# To generate a map of OK counties with a sf object 
ggplot(data = okcounty) +
  geom_sf(fill = NA)

# If the objective is to map tornadoes for only recent years, then the sf object 
# needs to be modified to contain only the desired years. 
names(tpoint)

# Filter datasets to a particular year range and select columns 
# om (a unique ID code) and date (the date of each tornado)
tpoint_16_21 <- tpoint %>%
  filter(yr >= 2016 & yr <= 2021) %>%
  select(om, yr, date)

tpath_16_21 <- tpath %>%
  filter(yr >= 2016 & yr <= 2021) %>%
  select(om, yr, date)

# Mapping initiation points of tornadoes in OK from 2016 to 2021
ggplot() +
  geom_sf(data = okcounty, fill = NA) +
  geom_sf(data = tpoint_16_21) +
  theme_bw()

# Mapping tornado path - sizes are increased to be displayed larger than the default 0.5 
ggplot() +
  geom_sf(data = okcounty, fill = NA) +
  geom_sf(data = tpath_16_21, 
          color = "red", 
          size = 1) +
  theme_bw()

# Plot of tornadoes in OK from 2016 to 2021
ggplot() +
  geom_sf(data = tpoint_16_21, 
          aes(color = as.factor(yr))) +
  geom_sf(data = okcounty, fill = NA) +
  scale_color_discrete(name = "Year") +  #Set the legend name
  coord_sf(datum = NA) +
  theme_void()  # removes the plot axes and labels

# facet_wrap can be used to display the tornadoes for each year on a separate map
# the sizes of the points are reduced slightly so they are easier to see on the smaller map
ggplot() +
  geom_sf(data = okcounty, 
          fill = NA, 
          color = "gray") +
  geom_sf(data = tpoint_16_21, size = 0.75) +
  facet_wrap(vars(yr), ncol = 2) +
  coord_sf(datum = NA) +
  theme_void()

# Overlaying Vector Datasets
# st_join links rows from the two tables based on the spatial locations 
# instead of their attributes. 
# Each row in countypnt contains additional columns from the okcounty dataset 
# that correspond to the county that the tornado is within
countypnt <- st_join(tpoint_16_21, okcounty)
glimpse(countypnt)

# To count the number of tornadoes per county
countypnt <- st_drop_geometry(countypnt)  # convert from sf to normal dataframe
countysum <- countypnt %>%
  group_by(GEOID) %>%
  summarize(tcnt = n())
glimpse(countysum)

# Next okcounty is joined to countysum so that each polygon is associated with the 
# the appropriate tornado summary
# A few counties that had no tornadoes during 2016-21 are missing from countysum 
# and therefore have NA in the joined table
# 
countymap <- okcounty %>%
  left_join(countysum, by = "GEOID") %>%
  replace(is.na(.), 0) %>%
  mutate(area = st_area(okcounty),
         # calculates density of tornadoes per county - density initially in tornadoes 
         # per square meter but is converted to tornadoes per 1000km2
         tdens = 10^6 * 10^3 * tcnt / area) %>%   
  drop_units()
glimpse(countymap)


# Using the st_write() function to save sf objects to a variety of file formats
# The ffg code saves the county-level tornado summaries in ESRI shapefile format
# The append=F option overwrites the shapefile if it already exists
st_write(countymap, 
         dsn = "oktornadosum.shp", 
         append = FALSE)

# We can also write as OGC GeoPackage
# The data are stored in a SQLite database that may contain one or more layers
st_write(countymap, 
         dsn = "oktornado.gpkg", 
         layer = "countysum",
         delete_dsn = TRUE)   # keeping this as FALSE would add a new layer to existing database

# Another commonly used format is GeoJSON which is based on JavaScript 
# a human readable text format that stores data in ASCII files
st_write(countymap, "oktornado.geojson", 
         layer_options = "RFC7946 = YES") # set option to save data in newest GEOJSON specification

# CHLOROPLETH MAPS
ggplot(data = countymap) +
  geom_sf(aes(fill = tdens)) +
  theme_void()

# To map count data such as the number of tornadoes or disease cases in each county
# People tend to naturally associate sizes with quantity when viewing maps, and displaying 
# the counts as graduated symbols is often an effective approach

# Check geometry type
st_geometry_type(okcounty, by_geometry=FALSE)

# st_centroid generates a point feature located at the centroid of each county
okcntrd = st_centroid(countymap)
st_geometry_type(okcntrd, by_geometry = FALSE)

# Map the tornado counts for each county
ggplot() +
  geom_sf(data = okcntrd, aes(size = tcnt)) +
  geom_sf(data = okcounty, fill = NA) +
  theme_void()

# Modifying the appearance of the map
ggplot(data = countymap) +
  geom_sf(aes(fill = tdens)) +
  scale_fill_distiller(name = expression("Tornadoes/1000 km"^2), 
                       palette = "YlOrRd", 
                       breaks = pretty_breaks(),  # select a set of breaks for the legend
                       direction = 1) +
  theme_void() +   # call theme_void before theme as it removes any settings made by a previous theme function
  theme(legend.position = "bottom")

# The RColorBrewer package provides a selection of palettes designed for chloropleth mapping
RColorBrewer::display.brewer.all()

# To view the colors for a given number of categories and a specific palette
display.brewer.pal(5, "YlGnBu")

# Rather than using continous scales for color and size, it is often recommended 
# to aggregate the data into a small number of classes (typically 3-6)

# The cut function is used to split the continous variables based on user-specified breaks
# The incidence variable is split based on quantiles(percentiles) defined in the qbrks object
numclas <- 4
qbrks <- seq(0, 1, length.out = numclas + 1)
qbrks

# Define breaks according to percentiles defined in qbrks
countymap <- countymap %>%
  mutate(tdens_c1 = cut(tdens,
                        breaks = quantile(tdens, breaks = qbrks),
                        include.lowest = T))

# As a result, we can use scale_fill_brewer instead of scale_fill_distiller
ggplot(data = countymap) +
  geom_sf(aes(fill = tdens_c1)) +
  scale_fill_brewer(name = expression("Tornadoes/1000 km"^2),   
                    palette = "YlOrRd") +
  theme_void() +
  theme(legend.position = "bottom") 

# Generate a classified map of tornado counts - they're converted to discrete categories 
# using the cut(). Instead of using quantiles, the breakpoints for the classification are 
# selected manually and stored in the brkpts vector
maxcnt <- max(okcntrd$tcnt)
brkpts <- c(0, 2, 5, 10, maxcnt)
okcntrd <- okcntrd %>%
  mutate(tcnt_c1 = cut(tcnt,
                       breaks = brkpts,
                       include.lowest = T))

# Using scale_size_discrete() 
ggplot(data = okcntrd) +
  geom_sf(aes(size = tcnt_c1)) +
  scale_size_discrete(name="Tornadoes") +
  geom_sf(data = okcounty, fill = NA) +
  theme_void() +
  theme(legend.position = "bottom")

# Exporting Graphics Output
# It is necessary to export maps and other graphics to files and explicitly specify their dimensions 
# and resolution. This is usually the case when generating graphics for publications that must meet 
# specific size and formatting criteria
# Use ggsave()
ggsave("tornado.png", 
       width = 6, # 6 x 4 inches
       height = 4, 
       dpi = 300)  # resolution: 300pixels
ggsave("tornado2.png", 
       width = 15, 
       height = 10, 
       units = "cm", # other units besides inches can be used by specifying the units argument
       dpi = 100)

# File types: TIFF, JPEG, PDF files. Arguments vary with the type of file being created
ggsave("tornado.jpeg", 
       width = 6, 
       height = 4, 
       dpi = 300, 
       quality = 90)
ggsave("tornado.tiff", 
       width = 6, 
       height = 4, 
       dpi = 300, 
       compression = "lzw")
ggsave("tornado.pdf", 
       width = 6, 
       height = 4)

# The output of ggplot can also be saved as an R object that can be output to graphics files
# using ggsave(). The plot argument is used to specify the ggplot object to be saved
choropleth <- ggplot(data = countymap) +
  geom_sf(aes(fill = tdens_c1)) +
  scale_fill_brewer(name="Density",   
                    palette = "YlOrRd") +
  theme_void()

gradsymbol <- ggplot(data = okcntrd) +
  geom_sf(aes(size = tcnt_c1)) +
  scale_size_discrete(name="Count") +
  geom_sf(data = okcounty, fill = NA) +
  theme_void()

ggsave("choropleth.tiff", 
       plot = choropleth,
       width = 6, 
       height = 4, 
       dpi = 300, 
       compression = "lzw")

ggsave("gradsymbol.tiff",
       plot = gradsymbol,
       width = 6, 
       height = 4, 
       dpi = 300, 
       compression = "lzw")

# Saved graphs and maps can also be combined into a composite figure using the cowplot pkg
# the plot_grid function provides a variety of options for arranging figures in a regular grid
plot_grid(choropleth, gradsymbol, 
          labels = c("A) Choropleth Map", 
                     "B) Graduated Symbol Map",
                     label_size = 12),
          ncol = 1, # single column
          hjust = 0, # justify labels
          label_x = 0, # move labels to top of each map
          align = "hv")    # align maps horizontally n vertically so that they're exactly the same size

# PRACTISE
# 1. Generate a map of tornado paths where the paths from each year are displayed as a 
# different color, similar to the map of tornado points in Figure 5.4. 
# Create a composite figure containing the map of tornado paths and 
# the map of tornado points using plot_grid()
tornadopathsyr <- ggplot() +
  geom_sf(data = okcounty, fill = NA) +
  geom_sf(data = tpath_16_21, aes(color = as.factor(yr))) +
  scale_color_discrete(name = "Year") +
  theme_bw()

tornadoptsyr <- ggplot() +
  geom_sf(data = tpoint_16_21, 
          aes(color = as.factor(yr))) +
  geom_sf(data = okcounty, fill = NA) +
  scale_color_discrete(name = "Year") +  #Set the legend name
  coord_sf(datum = NA) +
  theme_void()  # removes the plot axes and labels

plot_grid(
  tornadopathsyr, 
  tornadoptsyr, 
  labels = c("A) Tornado Paths by Year", 
             "B) Tornado Points by Year",
             label_size = 12),
  ncol = 1, # single column
  hjust = 0, # justify labels
  label_x = 0, # move labels to top of each map
  align = "hv"
)

# 2. Summarize the density of tornado points by both county and year and 
# generate a faceted plot that displays maps of county-level tornado density 
# from 2016-2021
countypntlstyr = split(countypnt, f = countypnt$yr)

# Calculate density of tornado points by year
countysumlstyr <- lapply(countypntlstyr, function(countypnt){
    
  countypnt <- st_drop_geometry(countypnt)
    
    countysum <- countypnt %>%
      group_by(GEOID) %>%
      summarize(tcnt = n())
    
    countymap <- okcounty %>%
      left_join(countysum, by = "GEOID") %>%
      replace(is.na(.), 0) %>%
      mutate(area = st_area(okcounty),
             # calculates density of tornadoes per county - density initially in tornadoes 
             # per square meter but is converted to tornadoes per 1000km2
             tdens = 10^6 * 10^3 * tcnt / area) %>%   
      drop_units()
    
    return(countymap)
    
})

# Combine list of calculated densities into one dataframe
countysumdfperyr = bind_rows(countysumlstyr, .id = "Year")

# Faceted plot
ggplot() +
  geom_sf(data = countysumdfperyr, 
          aes(fill = tdens)) +
  facet_wrap(vars(Year), ncol = 2) +
  scale_fill_distiller(name = expression("Tornadoes/1000 km"^2), 
                       palette = "YlOrRd", 
                       breaks = pretty_breaks(),  # select a set of breaks for the legend
                       direction = 1) +
  theme_void() + 
  theme(legend.position = "bottom")

# 3. Generate four choropleth maps of tornado density based on quantile breaks 
# with numbers of classes ranging from 3 to 6. Create a composite figure containing the 
# four maps using plot_grid() and examine how the number of classes changes the 
# interpretation of the map.

chrbrkmap <- lapply(3:6, function(numclas){
  
  qbrks <- seq(0, 1, length.out = numclas + 1)
  qbrks
  
  # Define breaks according to percentiles defined in qbrks
  countymp <- countymap %>%
    mutate(tdens_c1 = cut(tdens,
                          breaks = quantile(tdens, probs = qbrks),
                          include.lowest = T))
  
  chrbrkmap <- ggplot(data = countymp) +
    geom_sf(aes(fill = tdens_c1)) +
    scale_fill_brewer(name = expression("Tornadoes/1000 km"^2),   
                      palette = "YlOrRd") +
    theme_void() +
    theme(legend.position = "bottom") 
  
  return(chrbrkmap)
  
})

# Chloropleth maps for each number of breaks 
plot_grid(plotlist = chrbrkmap, 
          labels = c("A) Choropleth Map of Tornado density(3)", 
                     "B) Choropleth Map of Tornado density(4)",
                     "C) Choropleth Map of Tornado density(5)",
                     "D) Choropleth Map of Tornado density(6)",
                     label_size = 6),
          ncol = 2, # single column
          hjust = 0, # justify labels
          label_x = 0, # move labels to top of each map
          align = "hv")    # align maps horizontally n vertically so that they're exactly the same size
