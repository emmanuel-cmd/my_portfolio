# # Use install.packages to install R packages from the CRAN repository.
# install.packages('reticulate') # Connect Python with R.
# install.packages('rayshader') # 2D and 3D data visualizations in R.
# install.packages('remotes') # Install R packages from remote repositories.
# remotes::install_github('r-earthengine/rgeeExtra') # rgee extended.
# install.packages('rgee') # GEE from within R.
# install.packages('sf') # Simple features in R.
# install.packages('stars') # Spatiotemporal Arrays and Vector Data Cubes.
# install.packages('geojsonio') # Convert data to 'GeoJSON' from various R classes.
# install.packages('raster') # Reading, writing, manipulating, analyzing and modeling of spatial data.
# install.packages('magick') # Advanced Graphics and ImageProcessing in R
# install.packages('leaflet.extras2') # Extra Functionality for leaflet
# install.packages('cptcity') # colour gradients from the 'cpt-city' web archive

library(reticulate)
library(rgeeExtra)
library(rayshader)
library(raster)
library(rgee)

# Initialize just Earth Engine
ee_Initialize()
# Initialize Earth Engine and GD
ee_Initialize(drive = TRUE) 

# Section 2: Creating a 3D population density map with rgee and rshader 
ee_Initialize(drive=T)

# We will assess the WorldPop Global Project Population dataset
collections <- ee$ImageCollection$Dataset
population_data <- collections$CIESIN_GPWv411_GPW_Population_Density
population_data_max <- population_data$max()

# If you need more info about the data, use this command to go to the official documentation
# in the Earth Engine Data Catalog
population_data %>% ee_utils_dataset_display()

# Define a boundary 
sa_extent <- ee$Geometry$Rectangle(
  coords = c(-100, -50, -20, 12),
  geodesic = TRUE,
  proj = "EPSG:4326"
)

# We use ee_as_raster which automatically transforms an ee object to a rasterlayer 
population_data_ly_local <- ee_as_raster(
  image = population_data_max,
  region = sa_extent,
  dsn = "/home/pc-u78 ser01/population.tif", # change for your own path.
  scale = 5000
) 
