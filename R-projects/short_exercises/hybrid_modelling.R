### Making future projections with a species distribution model and a hybrid model

# 1. Calculate H(x) habitat suitability
# 2. Calculate S(x)
# 3. Put things together

# P(x) = S(r(x)) * H(x) # Equation 1

# Where: x is a cell
# P(x) is the probability of occurrence in cell x
# H(x) is habitat suitability of cell x (scaled between 0 and 1)
# r(x) is the distance to the nearest occupied cell
# S(r(x)) is the spread kernel, i.e. distance-dependent probability of colonizers arriving at cell x

# S(r) = a * exp(-r^2 / (2*d^2)) # Equation 2

# a and d are parameters: 
# a is the kernel height
# d is the kernel width (km)

# Define a path where all necessary files are stored
data_path <- "D:/Neuer Ordner"
setwd(data_path)

# list.files()

# Load the required packages

library(raster)
library(rgdal)
library(dismo)

# Import distribution data for ragweed in Germany
# Use common grid for occurrences and environment

germany <- getData('GADM', country = 'DEU', level = 0) # GADM: database of global administrative boundaries

# Import environmental data for Germany
# Environmental data

altitude <- getData("worldclim", var="alt", res=5) # altitude
bioclim_26 <- getData('CMIP5', var='bio', res=5, rcp=26, model='MP', year=50)
bioclim_85 <- getData('CMIP5', var='bio', res=5, rcp=85, model='MP', year=50) # bioclimatic variables (19)
bioclim_45 <- getData('CMIP5', var='bio', res=5, rcp=45, model='MP', year=50)

# Occurrence of Ambrosia

ambrosia_locations <- try(readOGR(paste(data_path, "ambrosia", sep="/"), "ambrosia"))

# Add the projection of the Germany data set to the ambrosia data

#ambrosia_locations@proj4string <- germany@proj4string 

plot(germany)

plot(ambrosia_locations, add = TRUE)

#if (inherits(ambrosia_locations, "try-error")) {
#  ambrosia_locations <- gbif("Ambrosia", "artemisiifolia", ext = germany,
#                   sp = TRUE)
#  #proj4string(ambrosia_locations) <- CRS("+proj=longlat +datum=WGS84")
#  ambrosia_locations@proj4string <- germany@proj4string
#  # Write file
#  writeOGR(ambrosia_locations, paste(data_path, "ambrosia", sep="/"),
#           "ambrosia", driver="ESRI Shapefile")  
#}

# Filter those occurrences within Germany

in_germany <- over(ambrosia_locations, germany) # Spatial overlay

ambrosia_locations <- ambrosia_locations[!is.na(in_germany$NAME_0),] # maybe ISO

plot(germany)

plot(ambrosia_locations, add = TRUE)

# Create base raster for study site (here: Germany): Define resolution in x and y direction and extent

diff_x <- 1/6 # width of a cell
diff_y <- 1/10 # length of a cell
study_extent <- extent(c(xmn = 5.75 + diff_x/2, # ~ 5.8 degree of longitude
                         xmx = 15.08 + diff_x/2, # 15 degree of longitude
                         ymn = 47.21, ymx = 55.11)) # degree of latitude

base_raster <- raster(nrows = (ymax(study_extent) - ymin(study_extent)) / diff_y, # 79 rows (á 10km)
                      ncols = (xmax(study_extent) - xmin(study_extent)) / diff_x, # 56 columns (á 10 km)
                      ext = study_extent,
                      crs = proj4string(ambrosia_locations))

# Transfer values associated with vector type spatial data (here: "lines" for Germany)
# to the spatially overlapping raster cells

germany_raster <- rasterize(germany, base_raster)
plot(germany_raster)
# Trim (shrink) the raster object by removing outer rows and columns
# that have no data

germany_raster <- trim(germany_raster)

plot(germany_raster)

# Crop the base_raster to the extent of the germany_raster

base_raster <- crop(base_raster, germany_raster)

# Crop environmental data to study extent, resample to study resolution, method: nearest neighbour

altitude <- resample(altitude, base_raster, method = "ngb")

bioclim_45 <- resample(bioclim_45, base_raster, method = "ngb")

# Stack the environmental layers
environment <- stack(altitude, bioclim_45)
names(environment)[1] <- "altitude"
# Mask for Germany
# mask creates a new raster object where all cells that are NA
# in a mask object (here: germany) are set to NA, and that has the same
# values as x (here: environment) in the other cells

environment <- mask(environment, germany)   #removes NA cells from the environment datasets that are in the border of germany 

# Rasterize the occurence of Ambrosia

ambrosia <- rasterize(ambrosia_locations, 
                      base_raster, 
                      fun = "count") # if more than one occurrence falls into one raster cell
# these occurrences are counted

ambrosia
names(ambrosia)[2] <- "ambrosia"  # replace "species" by name of the species

# Stack the occurrence and the environment

full_raster <- stack(ambrosia, environment)

# Convert raster to SpatialPixelsDataFrame (necessary for modelling)

full_data <- as(full_raster, "SpatialPixelsDataFrame")

summary(full_data)

# Replace NA values of ambrosia locations with zeros (absences)

full_data$ambrosia[is.na(full_data$ambrosia)] <- 0

# Set all cells with ambrosia presence to 1 (presences)

full_data$ambrosia <- pmin(full_data$ambrosia, 1)

summary(full_data)

##### Modelling #####

# Fit logistic regression model (GLM) to occurrence data
# bio1: annual mean temperature, bio8: mean temperature of wettest quarter

#m1 <- glm(ambrosia ~ poly(bio1, 2) + bio8,                 #-poly -> a quadratic relationship for the variable
#family = binomial(), data = full_data)

m1 <- glm(ambrosia ~ altitude + mp45bi501 + mp45bi502 + mp45bi503 + mp45bi504 + mp45bi505 + mp45bi506 + 
          mp45bi507 + mp45bi508 + mp45bi509 + mp45bi5010 + mp45bi5011 + mp45bi5012 + mp45bi5013 + mp45bi5014 +
          mp45bi5015 + mp45bi5016 + mp45bi5017 + mp45bi5018 + mp45bi5019, family = binomial(), data = full_data)
summary(m1)
step(m1, direction = "backward")

m2 <- glm(ambrosia ~ mp45bi506 + 
            mp45bi507 + mp45bi509 + mp45bi5013 + 
            mp45bi5017 + mp45bi5018 + mp45bi5019, family = binomial(), data = full_data)
summary(m2)

# Create a raster with habitat suitability values
# predicted from the fitted model object (here: m1)
# and environment as predictor variables

H <- predict(environment, m2, type = "response")  #transfers model output to probability values between 0 and 1

plot(H, main = "SDM: Projected distributions for the year 2050 (RCP 4.5)")
#plot(ambrosia_locations, add = TRUE)

# Calculate the spread of the plant species in Germany
# with the Combined_GLM
# Parameters

a <- 0.659 # Table 3, model "Combined_GLM"
d <- 23.43 # Table 3, model "Combined_GLM"

set.seed(2)

# Create a raster layer object with the occurrences
# For class RasterStack (here: full_raster) it follows the
# form raster(x, layer=0)

occurrences <- raster(full_raster, "ambrosia") >= 1

#summary(occurrences)

for (i in 1:15) {
  
  # Setting empty cells to NA,
  # because the function distance calculates the
  # distance to the nearest non-NA cell
  
  occurrences[occurrences != 1] <- NA 
  
  # Calculate the distances
  
  r <- distance(occurrences) / 1000 # 1000 scales from m to km
  
  # Calculate the distance-dependent probability of colonizers arriving at a cell
  
  S <- calc(r, function(r) a * exp(-r^2 / (2*d^2))) # Equation 2
  
  # Calculate the probability of a cell becoming infested during the respective time step
  
  P <- H * S # Equation 1
  
  occurrences[] <- suppressWarnings(rbinom(ncell(P), 1,  # Ignore all warnings, compare result with a random number
                                           getValues(P))) == 1 | getValues(occurrences) == 1  
  
  Sys.sleep(1)
  
  plot(occurrences, main = "Hybrid model: Projected spread for the year 2050 (RCP 4.5)")
  plot(germany, add = T)
  
}

# Creating the time series data

# The "true" habitat suitability. Here: the GLM fitted to the occurrences of the initial time step

h <- predict(environment, m2, type = "response") 

H <- 1 - (1 - h)^(1/15)   # Eq. 3, scaling to annual suitability

set.seed(2)

# Create a raster layer object with the occurrences

occurrences <- raster(full_raster, "ambrosia") >= 1

ambrosia_timeseries <- occurrences

# Run the model for the 15 years

for (i in 1:15) {
  
  occurrences[occurrences != 1] <- NA # Setting empty cells to NA
  
  r <- distance(occurrences) / 1000 # Calculate the distances
  
  S <- calc(r, function(r) a * exp(-r^2 / (2*d^2))) # Calculate the distance-dependent probability of colonizers arriving at a cell
  
  P <- H * S # Calculate the probability of a cell becoming infested during the respective time step
  
  occurrences[] <- suppressWarnings(rbinom(ncell(P), 1, getValues(P))) == 1 | getValues(occurrences) == 1 # Compare result with a random number
  
  ambrosia_timeseries <- stack(ambrosia_timeseries, occurrences) # Stack the yearly results
  
}

ambrosia_timeseries

names(ambrosia_timeseries) <- paste("year", 1990:2005, sep="") # Add the years as name

plot(ambrosia_timeseries)  

############ Calculation of Log-likelihood #############

# We need to look at the change in sites that are not yet occupied,
# because sites that are already occupied will stay occupied

# Calculates the probability of observing the data
# given a particular set of model parameter values

smolik_neg_log_lik <- function(x, use_distance = FALSE, use_habitat = FALSE) {
  
  a <- exp(x[1]) # use exp to prevent negative values for a
  
  d <- ifelse(use_distance, exp(x[2]), Inf) # use exp to prevent negative values for d
  
  is_germany <- is.finite(getValues(H)) # is.finite(getValues(H)) selects the suitable cells
  
  if (use_habitat == FALSE) H[] <- 1
  
  loglik <- numeric(nlayers(ambrosia_timeseries) - 1)
  
  for (i in 2:nlayers(ambrosia_timeseries)) {
    
    past <- raster(ambrosia_timeseries, i-1) # past occupancy pattern
    
    present <- raster(ambrosia_timeseries, i) # present occupancy pattern
    
    past[past != 1] <- NA # set empty cells to NA to use the distance function
    
    is_unoccupied <- !is.finite(getValues(past)) # !is.finite(getValues(past)) selects the empty cells
    
    r <- distance(past) / 1000
    
    S <- calc(r, function(r) a * exp(-r^2 / (2*d^2)))
    
    P <- H * S
    
    p <- getValues(P)
    
    y <- getValues(present)
    
    y[is.na(y)] <- 0
    
    p <- p[is_germany & is_unoccupied] # use the probability of those cells that are suitable and empty
    
    y <- y[is_germany & is_unoccupied] # update present occupancy where the cell is suitable and empty
    
    loglik[i-1] <- sum(ifelse(y == 1, log(p), log(1-p)))
    
  }
  
  -sum(loglik)  
  
}

# Likelihood profiles for a and d

# using the correct model structure 
# with the "true" habitat suitability

a <- 0.659 # Table 3, model "Combined_GLM"
d <- 23.43 # Table 3, model "Combined_GLM"

h <- predict(environment, m1, type = "response") # The "true" habitat suitability. Here: 
# the GLM fitted to the occurrences of the initial time step

H <- 1 - (1 - h)^(1/15)   # Eq. 3, scaling to annual suitability

as <- seq(0.1, 1, by = 0.1)

LLs <- sapply(as, function(x) smolik_neg_log_lik(log(c(x, d)), use_distance = TRUE, 
                                                 use_habitat = TRUE))
plot(as, LLs)

ds <- c(3.43, 13.43, 23.43, 33.43, 43.43, 53.43)

dLLs <- sapply(ds, function(x) smolik_neg_log_lik(log(c(a, x)), use_distance = TRUE, 
                                                  use_habitat = TRUE))

plot(ds, dLLs)

plot(ds[-1], dLLs[-1])
