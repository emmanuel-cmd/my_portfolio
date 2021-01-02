
# Load useful libraries here..
library(rgeos)
library(sp)
library(raster)
library(rgdal)
library(rnaturalearth)
library(ellipse)
library(maxnet)
library(dismo)

# Load the Aedes occurrence data
aedesDF <- read.csv("data/data/Aedes_albopictus_2019-12-09.csv", header = T)
head(aedesDF)

# subset occurences only in Europe and further data cleaning
aedesdf <- subset(aedesDF, Continent == "Europe")
aedesdf <- aedesdf[,c("ID", "Continent", "Country", "Longitude", "Latitude")]
aedesdf$Country <- as.factor(aedesdf$Country)
aedesdf$Longitude <- as.numeric(as.character(aedesdf$Longitude))
aedesdf$Latitude <- as.numeric(as.character(aedesdf$Latitude))

aedesdf <- aedesdf[!is.na(c(aedesdf$Longitude, aedesdf$Latitude)),]
row.names(aedesdf) <- 1:4514
aedesdf1 <- aedesdf[-c(2258:4514),]
#View(aedesdf)
nrow(aedesdf)
# convert coordinates to SpatialPoints object

aedes.sp <- SpatialPointsDataFrame(aedesdf1[,c(4,5)], 
                       data = data.frame(ID = aedesdf1[,1], 
                                         Continent = aedesdf1[,2], 
                                         Country = aedesdf1[,3]), 
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)

# plot for verification
plot(aedes.sp, axes = T)

# load a shapefile of the EU
#EU <- readOGR(dsn="data/data/shape", layer="europe_shape", stringsAsFactors=FALSE)
EU <- ne_countries(continent = 'europe')
EU <- EU[EU$sovereignt != "Russia",]

#plot together
plot(EU, axes = T)
plot(aedes.sp, add = T)
crs(EU) <- crs(aedes.sp)

# Filter occurences within Europe
in_europe <- over(aedes.sp, EU)
aedes.sp <- aedes.sp[!is.na(in_europe$sovereignt),]

# plot(EU, axes=T)
# plot(aedes.sp, add = T)

# optional: rasterize EU polygon
ext <- extent(EU)
crs(EU)
r <- raster(ext, res=.10) 
crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(rasterize(EU, r))

# rasterize Aedes occurrences
aedes <- as(aedes.sp, "SpatialPoints")
aedes_locations <- rasterize(aedes, r, fun = "count")
crs(aedes_locations) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(aedes_locations)

#---------------------------------------------------------------------------
# get bioclimatic variables and windspeed data
bioclim <- getData("worldclim", var="bio", res=5, path = ".", download = F)
plot(bioclim$bio1)

# crop to the extent of the bioclim shape file
bioclim_1 <- crop(bioclim, ext)
plot(bioclim_1$bio1)
plot(EU, add=T)

# mask bioclim to the extent of EU shapefile
bioclimEU <- mask(bioclim_1, EU)
plot(bioclimEU$bio1)
plot(aedes.sp, add=T)

# Variable selection and habitat modelling
cm <- cor(getValues(bioclimEU), use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

# Select an uncorrelated subset of environmental variables, e.g
env <- subset(bioclimEU, c('bio1', 'bio2', 'bio8', 'bio5', 'bio6', 'bio11', paste0('bio', 12:19)))
extent(env) <- extent(aedes_locations)
env <- projectRaster(env, aedes_locations, method = 'ngb')
proj4string(env) <- "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"
proj4string(aedes_locations) <- "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"

# stack the occurrence and the environment
full_raster <- stack(aedes_locations, env)
names(full_raster)[1] <- "aedes"
crs(full_raster) <- "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs"

# Selecting background points
set.seed(23)
background <- randomPoints(bioclimEU$bio1, 5000)
background <- as.data.frame(background)
background$aedes <- 0

# Convert raster to SpatialPixelsDataFrame (necessary for modelling)

full_data <- as(full_raster, "SpatialPixelsDataFrame")

# replace NA values of aedes locations with 0

#full_raster$aedes[is.na(full_raster$aedes)] <- 0

# Set all cells with aedes presence to 1 (presences)

full_data$aedes <- pmin(full_data$aedes, 1)

# Combine presence and background points
full_data_df <- as.data.frame(full_data)
full_data <- full_data_df[,c("x", "y", "aedes")]
full_data <- rbind(full_data, background)
coordinates(full_data) <- ~ x + y
proj4string(full_data) <- proj4string(full_raster)

# Add environmental data
full_data@data <- cbind(full_data@data, 
                        extract(env, full_data))
full_data <- as(full_data, "data.frame")
full_data <- na.omit(full_data)

# Modelling with Maxent

m1 <- maxnet(full_data$aedes, 
             full_data[,c('bio1', 'bio8', 'bio5', 'bio6', 'bio11', paste0('bio', 12:19))])

H <- predict(env, m1, type = "cloglog")

plot(H)
H >0.5
values(H) <- values(H)[values(H)>0.5]

