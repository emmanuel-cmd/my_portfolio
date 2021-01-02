
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
library(SDMtune)
library(xtable)

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
                     spec.col = "species", thin.par = 2.79, reps = 100,
                     out.dir = "aedes_occurrence/", write.files = T, locs.thinned.list.return = T, 
                     write.log.file = F)

plot(aedesdf1$Longitude, aedesdf1$Latitude, col = "blue")
plot(thinned_data[[100]]$Longitude, thinned_data[[100]]$Latitude, col = "red")

aedes.sp <- SpatialPointsDataFrame(thinned_data[[100]][,c(1,2)],
                                   data = data.frame(ID = aedesdf1[,1][1:1422],
                                                     Continent = aedesdf1[,2][1:1422],
                                                     Country = aedesdf1[,3][1:1422]),
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)


# convert coordinates to SpatialPoints object

# aedes.sp <- SpatialPointsDataFrame(aedesdf1[,c(4,5)], 
#                                    data = data.frame(ID = aedesdf1[,1], 
#                                                      Continent = aedesdf1[,2], 
#                                                      Country = aedesdf1[,3]), 
#                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)

# plot for verification
plot(aedes.sp, axes = T)



# load a shapefile of the EU

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

# Plot for verification
plot(EU, axes=T)
plot(aedes.sp, add = T)

# write out occurrence csv
write.csv(aedes.sp@coords, "./results/thinned_occurrences.csv")


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

#load humidity data

hum <- raster("./maxent/full/env/hum.asc")
crs(hum) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# crop environmental variables to EU extent
ext <- extent(EU)
bio <- crop(bio, ext)
bio <- raster::mask(bio, EU)

wind <- crop(wind, ext)
wind <- raster::mask(wind, EU)

# stack environmental data
env <- stack(bio, wind, hum)
names(env) <- c(paste0("bio", 1:19), "wind", "hum")
proj4string(env) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# stack the occurrence and the environment
crs(aedes_locations) <- crs(env)
aedes_locations <- projectRaster(aedes_locations, env, method = "ngb")

full_raster <- stack(aedes_locations, env)
names(full_raster)[1] <- "aedes"
names(full_raster)
full_raster_sub <- full_raster[[c("aedes",paste0("bio", c(1,4,5,7,11,12,18)), "hum", "wind")]]

en <- env[[c(paste0("bio", c(1,3,4:6,9:11)), "hum")]]

vifcor(en, th = .7)

# Variable selection and habitat modelling
cm <- cor(getValues(en), use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))

# extract raster values over these points
env_df <- raster::extract(full_raster_sub, aedes.sp)

# change data object to dataframe
env_df <- as.data.frame(env_df)

# Set all cells with aedes presence to 1 (presences)
env_df[is.na(env_df$aedes), ]$aedes <- 1 
env_df$aedes <- pmin(env_df$aedes, 1)

# merge aedes coords information
env_df <- cbind(env_df, aedes.sp@coords)


# Selecting background points
set.seed(23)
background <- randomPoints(env, 10000)
background <- as.data.frame(background)
background$aedes <- 0

# extract env data from background points

env_bg <- raster::extract(full_raster_sub, background[,c("x", "y")])
env_bg <- as.data.frame(env_bg)

# merge coord information
env_bg <- cbind(env_bg, background)
env_bg <- env_bg[is.na(env_bg),] 
env_bg <- env_bg[,-1]
names(env_bg) <- c(names(env_bg)[1:7],"hum","wind", "Longitude", "Latitude", "aedes")

#### Bind 

fulldata <- rbind(env_df, env_bg)
fulldata <- na.omit(fulldata)

# Modelling with Maxent

m1 <- maxnet(fulldata$aedes, 
             fulldata[,c('bio1','bio4','bio5','bio11','bio12','hum')])
full <- dropLayer(full_raster, 1)

H <- predict(full, m1, type = "cloglog")
plot(H)

# Model performance: Cross validation

set.seed(2)
fold <- kfold(fulldata, k = 5, by = fulldata$aedes_occur)

# The variable cv_pred will contain the cross-validated 
# predictions

fulldata$cv_pred <- NA 
for (i in unique(fold)) {
  traindata <- fulldata[fold != i, ]
  testdata <- fulldata[fold == i, ]
  cv_model <- maxnet(fulldata[, "aedes"], fulldata[,c('bio1', 'bio4','bio5','bio11','bio12','hum'
                                                  )])
  fulldata$cv_pred[fold == i] <- predict(cv_model, testdata, type='exponential')  
}

# AUC 
round(somers2(fulldata$cv_pred, fulldata$aedes), 2) #AUC -test data
round(somers2(predict(m1, fulldata, 
                      type = "exponential"), fulldata$aedes), 2) #AUC -training data

#   There is no difference in performance between train
#   -ing data and test data

# VARIABLE IMPORTANCE MEASURE
#devtools::install_git('https://gitlab.irstea.fr/bjoern.reineking/gceb5.git')
library(gceb5)


m1_varImp <- varImpMaxnet(m1, c('bio1', 'bio4','bio5','bio11','bio12',
                                'hum'), data = fulldata[,-c(1,10:13)])



barplot(m1_varImp, ylim = c(0,0.6))

# RESPONSE PLOTS 
plot(m1, type = "cloglog")
abline(h = mean(occurrence_points_df@data$value))


## Raster overlay --------------------------------

# extract above average mean windspeed
wind <- env$wind
mean(values(wind), na.rm = T)

#fulldataa <- fulldata[c(1:188),]

# get spatial points dataframe from occurrence points 
occurrence_points_df <- SpatialPointsDataFrame(fulldata[,c("Longitude", "Latitude")], 
                       data = data.frame(aedes = fulldata$aedes, wind = fulldata$wind), 
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)

# extract these points from suitability map
suitable_points <- extract(H, occurrence_points_df)
suitable_points <- suitable_points %>% as.matrix() %>% as.data.frame()

# merge suitability data to occurrence data
occurrence_points_df@data <- cbind(occurrence_points_df@data, suitable_points)

# determine the LPT threshold
subset(suitSub, occurrence == 1)$value %>% min()

# create column suitability or no suitability
state <- ifelse(occurrence_points_df@data$V1 >=  0.234639, "suitable", "not_suitable")
state <- as.matrix(state) %>% as.data.frame()

# bind to dataframe object
occurrence_points_df@data<- cbind(occurrence_points_df@data, state)

# rename column names of dataframe
names(occurrence_points_df@data) <- c("occurrence", "wind_speed", "value", "suitability")

suitSub <- subset(occurrence_points_df@data, suitability == "suitable")
suitSub$occ_char <- ifelse(suitSub$occurrence == 1, "occurrences", "non-occurrences")
suitSub$occ_char <- as.factor(suitSub$occ_char)


# HYPOTHESIS 1
boxplot(suitSub$wind_speed ~ suitSub$occ_char, xlab = "Ae. albopictus occurrence", ylab = "Wind speed (km/h)")

ggplot(suitSub, aes(x=occ_char, y=wind_speed)) +
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(fill = "white") + 
  scale_fill_manual() + 
  theme_bw() + 
  xlab("Ae. albopictus occurrences") + 
  ylab("Wind speed [km/h]")
  


# More visualization
x11()
par(mfrow = c(1,2))
plot(H, main = "Background(absence) points")
points(occurrence_points_df[occurrence_points_df@data$occurrence == 0&occurrence_points_df@data$suitability=="suitable",]@coords, pch = 3, col = "grey")
plot(H, main = "Presence points")
points(occurrence_points_df[occurrence_points_df@data$occurrence == 1&occurrence_points_df@data$suitability=="suitable",]@coords, pch = 3, col = "grey")


# test of significant difference between groups?
head(suitSub)

kruskal.test(suitSub$wind_speed, suitSub$occ_char)
pgirmess::kruskalmc(suitSub$wind_speed, suitSub$occ_char)

t.test(wind_speed ~ occ_char, data = suitSub)

# visualization
ggplot(data = suitSub, aes(wind_speed, value, color = occ_char))+
  geom_point()+
  geom_smooth(method = "glm", se = FALSE) + 
  labs(title="", x="Wind speed [km/hr]", y="Suitability") +
  scale_color_manual(labels = c("Non-occurrences", "Occurrences"), values = c("grey", "#00008B")) +
  theme_bw()+ 
  guides(color=guide_legend("Occurrences"))
  
# -> in presence regions, suitability reduces with increasing wind speed 

View(suitSub)

# writeOGR(EU, "aedesModelling", "EU_inward", driver = "ESRI Shapefile")


############################################
########### ANSWER HYPOTHESIS 2 ############
############################################

EU_dissolved <- gUnaryUnion(EU)
coast_buffer <- gBuffer(EU_dissolved, width = -1)

plot(EU_dissolved)
plot(coast_buffer, add = T)

# mask suitability information along the coast
H_masked <- raster::mask(H, coast_buffer, inverse = T)
plot(H_masked)

# mask temperature along the coast 
temp <- bio$bio1
temp_masked <- raster::mask(temp, coast_buffer, inverse= T)
plot(temp_masked)

# mask precipitation along the coast
prec <- bio$bio12
prec_masked <- raster::mask(prec, coast_buffer, inverse = T)
plot(prec_masked)


# convert
occ_bg <- SpatialPointsDataFrame(fulldata[,c(11,12)], data = data.frame(aedes = fulldata$aedes, wind = fulldata$wind, hum = fulldata$hum), 
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)

# extract occ_bg information along the coast
occ_bg_df <- over(occ_bg, coast_buffer)
occ_bg <- occ_bg[is.na(occ_bg_df), ]

# extract suitability at this point
Hdata_coast <- raster::extract(H_masked, occ_bg@coords)

# extract temp at this point
tempdata_coast <- raster::extract(temp_masked, occ_bg)

# extract prec at this point
precdata_coast <- raster::extract(prec_masked, occ_bg)

# bind extracted data as columns in a dataframe
finalcoastDF <- cbind(occ_bg@data$aedes, Hdata_coast, tempdata_coast, precdata_coast, occ_bg@data$wind, occ_bg@data$hum) %>% 
  as.data.frame()

# rename dataframe columns
names(finalcoastDF) <- c("occurrences", "index_suit", "temp", "prec", "wind", "hum")

# calculate mean of suitability 
mean_suit <- mean(values(H), na.rm = T)

# classification into suitable and not suitable
finalcoastDF$class <- ifelse(finalcoastDF$index_suit >= 0.234639, "suitable", "not_suitable")

# subset dataframe
finalcoastDF_sub <- subset(finalcoastDF, class == "suitable")
finalcoastDF_sub$class_pres <- ifelse(finalcoastDF_sub$occurrences == 0, "non-occurrences", "occurrences")

# simple boxplot ..
finalcoastDF_sub$class_pres <- as.factor(finalcoastDF_sub$class_pres)
boxplot(finalcoastDF_sub$wind ~ finalcoastDF_sub$class_pres)

# logistic regression analyses

finalcoastDF_sub$test <- ifelse(finalcoastDF_sub$occurrences == 1, 0, 1)

model1<- glm(test ~ wind + temp + prec, 
             data = finalcoastDF_sub, family = binomial)
summary(model1)

# Average marginal effect estimation (Leeper, 2017)
margins::margins(model1) %>% summary()

plot(margins::margins(model1))

# Interpretation: As the occurrence values increases
# from 0 to 1, temperature increases by 0.09% and precipitation remains constant
# while wind increases by 0.31% 

plot(H)
plot(H2)

writeRaster(H, "suitable.tif", overwrite=T)
writeRaster(H_masked, "suitable_coast.tif", overwrite=T)

library(phyloclim)

aed <- SpatialPointsDataFrame(fullCdata[, c("Longitude", "Latitude")], data = data.frame(Species = rep("Aedes Albopictus", 9894), 
                                                                                         Longitude = fullCdata$Longitude, 
                                                                                         Latitude = fullCdata$Latitude), 
                              proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)
fullCdata[, c("Longitude", "Latitude")] 
fullCdata[,c('bio1', 'bio3', 'bio4','bio7','bio11', 'bio12', 'bio18', 'wind')]

e_n <- dropLayer(env_c,1)

e_n <- as(e_n, "SpatialGridDataFrame")

plot(aed)

bg.similarity.test(p = aed, env = e_n)

atrributes<-extract(x = e_n, y = SpatialPoints(aed[, 2:3]))
p <- data.frame(aed, atrributes)

# try 3-d plots
library(scatterplot3d)
plotdata <- finalcoastDF_sub[,c("occurrences", "index_suit","temp", "prec", "wind")]
plotdata$color <- ifelse(plotdata$occurrences == 1, "blue", "red")
sp <- scatterplot3d(plotdata$temp, plotdata$prec, plotdata$index_suit, color = plotdata$color, angle = 45)
sp$plane3d(model1, lty.box = "solid")
