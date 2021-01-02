
# Load useful libraries here..
library(rgeos)
library(sp)
library(spThin)
library(raster)
library(rgdal)
library(rnaturalearth)
library(ellipse)
library(maxnet)
library(dismo)
library(gdalUtils)
library(usdm)
library(magrittr)
library(rms)
library(DALEX)

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

# spatial thinning to reduce spatial autocorrelation
aedesdf1$species <- "Ae.albopictus"
thinned_data <- thin(aedesdf1, lat.col = "Latitude", long.col = "Longitude",
     spec.col = "species", thin.par = 10, reps = 100,
     out.dir = "aedes_occurrence/", write.files = F, locs.thinned.list.return = T, 
     write.log.file = F)


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
ext <- extent(c(xmn = -24.14638, 
                xmx = 40.12648, 
                ymn = 34.74356, ymx = 82.00301))
EU <- EU[EU$sovereignt != "Russia",]
EU <- crop(EU, ext)


#plot together
plot(EU, axes = T)
plot(aedes.sp, add = T)
crs(EU) <- crs(aedes.sp)

# Filter occurences within Europe
in_europe <- over(aedes.sp, EU)
aedes.sp <- aedes.sp[!is.na(in_europe$sovereignt),]

# plot(EU, axes=T)
# plot(aedes.sp, add = T)
# writeOGR(aedes.sp, dsn="aedes_occurrence", layer="aedes_occurrence", driver = "ESRI Shapefile")

# optional: rasterize EU polygon
ext <- extent(EU)
crs(EU)
r <- raster(ext, res=.5) 
crs(r) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(rasterize(EU, r))

# rasterize Aedes occurrences
aedes <- as(aedes.sp, "SpatialPoints")
aedes_locations <- rasterize(aedes, r, fun = "count")
crs(aedes_locations) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(aedes_locations)

# # load temperature average and precipitation data
# tavg <- getData("worldclim", var="tmean", res=5, path = ".", download = F)
# tavg <- mean(tavg)
# prec <- getData("worldclim", var="prec", res=5, path = ".", download = F)
# prec <- mean(prec)

# load all bioclimatic variables
bio <- raster::getData('worldclim', var = 'bio', res = 2.5, download = T)

# load wind data
wind_list <- list()
for (i in dir("wc2-5/wind_25m/")){
  wind_list[[i]] <- raster(paste0("wc2-5/wind_25m/", i))
}
wind <- stack(wind_list)
names(wind) <- paste0("wind", 1:12)
wind <- mean(wind)
plot(wind)

# get MODIS EVI

# gdalinfo("rhum/MOD13C2.A2020153.006.2020197064848.hdf")
# 
# evi <- get_subdatasets("rhum/MOD13C2.A2020153.006.2020197064848.hdf")
# evi
# 
# gdal_translate(evi[2], dst_dataset = "EVI.tif")

# Load EVI data
EVI <- raster("EVI.tif")
crs(EVI) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(EVI)

# stack all the data
# EVI <- projectRaster(EVI, wind, method = "ngb")
# environ <- stack(bio, wind, EVI)


# crop environmental variables to EU extent
ext <- extent(EU)
bio <- crop(bio, ext)
bio <- raster::mask(bio, EU)

wind <- crop(wind, ext)
wind <- raster::mask(wind, EU)

EVI <- crop(EVI, EU)
EVI <- raster::mask(EVI, EU)

EVI <- projectRaster(EVI, wind, method = "ngb")


##
r <- raster("rhum/humidity/WDPRESENTMAXRHMEAN.tif")
r[r < -2000] <- NA
plot(r)

r <- projectRaster(r, wind, method = "ngb")
r <- crop(r, ext)
r <- raster::mask(r, EU)

writeRaster(r, "maxent/full/env/hum.asc")


# stack environmental data
env <- stack(bio, wind, r)
env <- env[[c(paste0("bio", c(1,3:7, 9:11)), "wind", "hum")]]
names(env) <- c(paste0("bio", c(1,3:7, 9:11)), "wind", "hum")
proj4string(env) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# # Variable selection and habitat modelling
# cm <- cor(getValues(env), use = "complete.obs")
# plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

# Set all cells with aedes presence to 1 (presences)
#plot(env)
#extent(env1) <- extent(aedes_locations)
crs(aedes_locations) <- crs(env)
aedes_locations <- projectRaster(aedes_locations, env, method = "ngb")

# stack the occurrence and the environment
full_raster <- stack(aedes_locations, env)
names(full_raster)[1] <- "aedes"

# Multicollinearity test - VIF

# checks VIF. If >10, it is a sign of multicollinearity 

# extract raster values over these points
env_df <- raster::extract(full_raster, aedes.sp)

# generate VIF value for all the variable
#vif(env)

# removes variables with the highest VIF and correlation
# above the threshold
aedes_occur <- env_df[,1]
v <- vifcor(env_df[,-1], th=0.7)

# exclude unwanted variables 
env_df <- exclude(env_df, v)
env_df <- cbind(env_df, aedes_occur)
env_df <- as.data.frame(env_df)

# # Convert raster to SpatialPixelsDataFrame (necessary for modelling)
# 
# full_data <- as(full_raster, "SpatialPixelsDataFrame")

# Set all cells with aedes presence to 1 (presences)

env_df$aedes_occur <- pmin(env_df$aedes_occur, 1)

# merge aedes coords information
env_df <- cbind(env_df, aedes.sp@coords)

# Selecting background points
set.seed(23)
background <- randomPoints(env, 5000)
background <- as.data.frame(background)
background$aedes_occur <- 0

# exclude unwanted variables for the full raster 
full_raster <- exclude(full_raster, v)

# extract env data from background points
env_bg <- raster::extract(full_raster, background[,c("x", "y")])
env_bg <- as.data.frame(env_bg)

# merge coord information
env_bg <- cbind(env_bg, background)
names(env_bg) <- c(names(env_bg)[1:5], "Longitude", "Latitude", "aedes_occur")

#### Bind 
fulldata <- rbind(env_df, env_bg)
fulldata <- na.omit(fulldata)

names(fulldata)
# # Combine presence and background points
# full_data_df <- as.data.frame(full_data)
# full_data <- full_data_df[,c("x", "y", "aedes")]
# full_data <- rbind(full_data, background)
# coordinates(full_data) <- ~ x + y
# proj4string(full_data) <- proj4string(full_raster)
# 
# # Add environmental data
# full_data@data <- cbind(full_data@data, 
#                         extract(env1, full_data))
# full_data <- as(full_data, "data.frame")
# full_data <- na.omit(full_data)

# Modelling with Maxent

m1 <- maxnet(fulldata$aedes_occur, 
             fulldata[,c('bio3', 'bio7', 'bio9',
                          'bio10', 'hum')])


H <- predict(full_raster, m1, type = "cloglog")
plot(H)


# write to directory 
#writeRaster(H, "suitability_map_aedes.tif")

# # convert suitability map to dataframe
# df_suitable <- as(H, "SpatialPolygonsDataFrame")
# names(df_suitable@data) <- "suitability"
# 
# windshp <- as(wind, "SpatialPolygonsDataFrame")
# writeOGR(windshp, "windshp", "windshp", "ESRI Shapefile")
# 
# # Create new column and apply threshold of occurrence
# df_suitable@data$occurence <- 0
# df_suitable@data$occurence[df_suitable@data$suitability>=0.5] <- 1

# check
# View(df_suitable@data)

# 
# df_suitable@data$wind_speed <- env1$wind
# 
# plot(df_suitable)

# Model performance: Cross validation

set.seed(2)
fold <- kfold(fulldata, k = 5, by = fulldata$aedes_occur)

# The variable cv_pred will contain the cross-validated 
# predictions

fulldata$cv_pred <- NA 
for (i in unique(fold)) {
  traindata <- fulldata[fold != i, ]
  testdata <- fulldata[fold == i, ]
  cv_model <- maxnet(fulldata[, "aedes_occur"], fulldata[,c('bio3', 'bio7', 'bio9',
                                                            'bio10', 'hum')])
  fulldata$cv_pred[fold == i] <- predict(cv_model, testdata, type='exponential')  
}

# AUC 
round(somers2(fulldata$cv_pred, fulldata$aedes_occur), 2) #AUC -test data
round(somers2(predict(m1, fulldata, 
                      type = "exponential"), fulldata$aedes_occur), 2) #AUC -training data

#   There is no difference in performance between train
#   -ing data and test data

## VARIABLE IMPORTANCE MEASURE
devtools::install_git('https://gitlab.irstea.fr/bjoern.reineking/gceb5.git')
library(gceb5)


m1_varImp <- varImpMaxnet(m1, c('bio3', 'bio7', 'bio9',
                                'bio10', 'hum'), data = fulldata[,-c(6:9)])

x <- as.vector(m1_varImp)

barplot(m1_varImp, ylim = c(0,0.50))


## Raster overlay --------------------------------

# extract above average mean windspeed
wind <- env$wind
#wind[wind<mean(values(wind), na.rm = T)] <- NA
plot(wind)
mean(values(wind), na.rm = T)

prob2presence <- function(prob) {
  # input is predicted probability of presence,
  # output is presence/absence
  prob > quantile(prob, 1-mean(prob), na.rm=T)   
}

df_pres_abs <- prob2presence(values(H)) %>% as.numeric()


# extract suitability values greater than 0.5
presences <- H
absences <- H

presences[na.omit(df_pres_abs==1)] <- NA
absences[na.omit(df_pres_abs==1)] <- NA

presences[presences<mean(values(H), na.rm = T)] <- NA 
absences[absences>=mean(values(H), na.rm = T)] <- NA

#H[H<0.5] <- NA
plot(presences)
plot(absences)

# 
# absences0 <- subset(full_data, is.na(full_data[,3]))
# absences1 <- subset(full_data, !is.na(full_data[,3]))
# 
# absences.sp0 <- SpatialPoints(absences0[,1:2], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# plot(absences.sp0)


# extract wind data of areas of occurrences 

wind_pres <- mask(crop(wind, presences), presences) 
mean(values(wind_pres), na.rm = T)
plot(wind_pres)

# extract wind data of areas of non-occurrences
wind_absences <- mask(crop(wind, absences), absences)
mean(values(wind_absences), na.rm = T)
plot(wind_absences)

# # extract areas of above average windspeeds
# wind_pres[wind_pres<=mean(values(wind), na.rm = T)] <- NA
# wind_absences[wind_absences<=mean(values(wind), na.rm = T)] <- NA

dfwc <- values(wind_pres)
dfnwc <- values(wind_absences)

dfwc <- data.frame(dfwc[!is.na(dfwc)])
dfnwc <- data.frame(dfnwc[!is.na(dfnwc)])

names(dfwc) <- names(dfnwc)
nrow(dfwc)
nrow(dfnwc)

df <- rbind(dfwc, dfnwc) 

df$suitable <- c(rep("presences", nrow(dfwc)), rep("absences", nrow(dfnwc)))
names(df) <- c("wind_speed", "suitable")

# 
df$suitable <- as.factor(df$suitable)

# df$wind_speed <- sqrt(df$wind_speed)

boxplot(df$wind_speed ~ df$suitable)
abline(h = mean(values(wind), na.rm = T), col = "red")

#plot(dfwc[dfwc>mean(values(wind), na.rm = T)])

#plot(wind_pres)
#plot(wind_absences)





