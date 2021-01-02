#####################################################################################
################ MASTER'S THESIS FINAL DATA ANALYSIS - Using ENMeval ################
#####################################################################################
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
library(ENMeval)

# Load occurrence data
occs <- read.csv("results/thinned_occurrences.csv")
occs <- occs[,-1]

# load a shapefile of the EU

EU <- ne_countries(continent = 'europe')
ext <- extent(c(xmn = -24.14638, 
                xmx = 40.12648, 
                ymn = 34.74356, ymx = 82.00301))
EU <- EU[EU$sovereignt != "Russia",]
EU <- crop(EU, ext)

# load all bioclimatic variables
bio <- raster::getData('worldclim', var = 'bio', res = 2.5, download = F)

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

bio_env <- bio[[paste0("bio", c(1,4,5,9,10,12))]]

# stack environmental data
wind <- projectRaster(wind, bio_env$bio1, method = "ngb")
env <- stack(bio_env, wind)
names(env) <- c(paste0("bio", c(1,4,5,9,10,12)), "wind")
proj4string(env) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# crop and mask environmental layer to the EU extent
envs <- crop(env, EU)
envs <- raster::mask(envs, EU)

# Make a SpatialPoints object
occs.sp <- SpatialPoints(occs)
proj4string(occs.sp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# occs.spdf <- SpatialPointsDataFrame(occs.sp, data = data.frame(ID = 1:nrow(occs)))

# Selecting background points
set.seed(23)
bg <- randomPoints(envs$bio1, 10000)
bg <- as.data.frame(bg)

envS <- dropLayer(envs, 7)

# Modelling with ENMeval..
eval2 <- ENMevaluate(occ=occs, env=envS, bg.coords=bg, 
                     method='checkerboard2', RMvalues=seq(0.5, 10, 0.5), 
                     fc=c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), algorithm='maxent.jar', parallel = T)
eval2@results[which(eval2@results$delta.AICc==0),]
eval2@results[which(eval2@results$features == c('LQHP', 'LQHPT')),]

H <- predict(eval2@models[[42]], envS, type = "cloglog")
plot(eval2@models[[42]], type = "exponential")
plot(H_1)
H_1 <- crop(H, EU)
H_1 <- raster::mask(H_1, EU)
# plot combinations of regularization multiplier and feature combinations
eval.plot(eval2@results)

########### CALCULATE PROC

#devtools::install_github("marlonecobos/kuenm")
library(kuenm)

proc <- kuenm_proc(occs, H, threshold = 50, rand.percent = 25, iterations = 1000)
proc$pROC_results[,c("Model_partial_AUC")] %>% mean() # Model partial AUC
proc$pROC_results[,c("Random_curve_partial_AUC")] %>% mean() # Random_curve_partial_AUC
proc$pROC_results[,c("AUC_ratio")] %>% mean() # Mean AUC ratio


###########################################################
################# ANSWER HYPOTHESIS 1 #####################
###########################################################

occs_sp <- SpatialPoints(occs[,c(1,2)], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
bg_sp <- SpatialPoints(bg[,c(1,2)], proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# plot(bg_sp)
# plot(occs_sp,add = T, col="red")

# generate buffer of 1km around occurrences
occsBuf <- gBuffer(occs_sp, width = 1)
plot(occsBuf)

# overlay buffer on background points 
ov <- over(bg_sp, occsBuf)
new_bg <- bg_sp[is.na(ov),]

# prepare occurrence-background records
occs.bg <- rbind(occs_sp@coords, new_bg@coords)
occs.bg <- as.data.frame(occs.bg)
occs.bg$aedes <- c(rep(1, nrow(occs_sp@coords)), rep(0, nrow(new_bg@coords)))

# prepare occurrence-background records
# names(bg) <- c("Longitude", "Latitude")
# occs.bg <- rbind(occs, bg)
# occs.bg$aedes <- c(rep(1, nrow(occs)), rep(0, nrow(bg)))

# average wind speed
mean(values(wind), na.rm = T)

# extract windspeed in areas of occurrences
wind.occs <- raster::extract(wind, occs.bg[,c(1,2)])

# merge wind to occs.bg records
occs.bg$wind <- wind.occs

# extract these points from suitability map
suitable_points <- raster::extract(H, occs.bg[,c(1,2)])

# merge suitability data to occurrence-background records
occs.bg$H_index <- suitable_points

# determine the LPT threshold
LPT <- subset(occs.bg, aedes == 1)$H_index %>% min(.,na.rm=T)
# low10 <- tail(sort(subset(occs.bg, aedes == 1)$H_index, decreasing = T), 129)
# a <- subset(occs.bg, aedes == 1)[subset(occs.bg, aedes == 1)$H_index != low10,]

# b <- subset(occs.bg, aedes == 1)
# b <- na.omit(b[order(-b[,5]),])
# b <- na.omit(b)
# b <- b[-c(1160:1289),]
# LPT <- min(b$H_index)

# create column suitability/no suitability, occurrences/non-occurrences
occs.bg$H_class <- ifelse(occs.bg$H_index >=  LPT, "suitable", "not_suitable")
occs.bg$occ_class <- ifelse(occs.bg$aedes == 1, "occurrences", "non-occurrences")
occs.bg$occ_class <- as.factor(occs.bg$occ_class)


# subset for suitable class
suitSub <- subset(occs.bg, H_class == "suitable")

# HYPOTHESIS 1
boxplot(suitSub$wind ~ suitSub$occ_class, xlab = "Ae. albopictus occurrence", ylab = "Wind speed (km/h)")

ggplot(suitSub, aes(x=occ_class, y=wind)) +
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(fill = "white") + 
  scale_fill_manual() + 
  theme_bw() + 
  xlab("Ae. albopictus") + 
  ylab("Wind speed [m/s]")


# More visualization
x11()
par(mfrow = c(1,1))
plot(H, main = "Background(absence) points")
points(occs.bg[occs.bg$aedes == 0&occs.bg$H_class=="suitable",][,c(1,2)], pch = 3, col = "grey")
plot(H, main = "Presence points")
points(occs.bg[occs.bg$aedes == 1&occs.bg$H_class=="suitable",][,c(1,2)], pch = 3, col = "grey")

# t.test of significant difference between groups

t.test(wind ~ occ_class, data = occs.bg)

t.test(H_index ~ occ_class, data = occs.bg) # suitability is higher in areas of occurrences than in non-occurrences

# visualization
ggplot(data = occs.bg[occs.bg$H_index>LPT,], aes(wind, H_index, color = occ_class))+
  geom_point()+
  geom_smooth(method = "glm", se = FALSE) + 
  labs(title="", x="Wind speed [km/hr]", y="Suitability") +
  scale_color_manual(labels = c("Non-occurrences", "Occurrences"), values = c("grey88", "black")) +
  theme_bw()+ 
  guides(color=guide_legend("Occurrences")) + 
  geom_hline(yintercept = LPT, linetype="dashed")+
  geom_text(aes(4,0.0625,label='LPT threshold', vjust = 1))

ggplot(data = occs.bg[occs.bg$occ_class == 'occurrences'& occs.bg$H_index>LPT,], aes(wind, H_index, color = occ_class))+
  geom_point()+
  geom_smooth(method = "glm", se = FALSE, col = "brown") + 
  labs(title="", x="Wind speed [m/s]", y="Predicted probability of Ae.albopictus occurrences") +
  scale_color_manual(labels = c("Occurrences"), values = "#00008B") +
  theme_bw()+ 
  guides(color=guide_legend("Legend")) + 
  theme(legend.title = element_blank(), legend.position = "none") +
  #geom_hline(yintercept = LPT, linetype="dashed")+
  geom_text(aes(3,LPT,label='', vjust = 1))

ggplot(data = occs.bg[occs.bg$occ_class == 'non-occurrences'& occs.bg$H_index>LPT,], aes(wind, H_index, color = occ_class))+
  geom_point()+
  geom_smooth(method = "glm", se = FALSE, col = "brown") + 
  labs(title="", x="Wind speed [m/s]", y="Predicted probability of Ae.albopictus non-occurrences") +
  scale_color_manual(labels = c(""), values = "#00008B") +
  theme_bw()+ 
  guides(color=guide_legend("Legend")) +
  theme(legend.title = element_blank(), legend.position = "none") + 
  #geom_hline(yintercept = LPT, linetype="dashed")+
  geom_text(aes(4,LPT,label='', vjust = 1))
  

############################################
########### ANSWER HYPOTHESIS 2 ############
############################################

EU_dissolved <- gUnaryUnion(EU)
coast_buffer <- gBuffer(EU_dissolved, width = -1)

plot(EU_dissolved)
plot(coast_buffer, add = T)

# mask suitability information along the coast
H_masked <- raster::mask(H_1, coast_buffer, inverse = T)
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
occs_bg.coast <- SpatialPointsDataFrame(occs.bg[,c(1,2)], data = data.frame(aedes = occs.bg$aedes, wind = occs.bg$wind), 
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"), bbox = NULL)

# extract occ_bg information along the coast
proj4string(coast_buffer) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
occ_bg_df <- over(occs_bg.coast, coast_buffer)
occ_bg <- occs_bg.coast[is.na(occ_bg_df), ]

# extract suitability at this point
Hdata_coast <- raster::extract(H_masked, occ_bg@coords)

# extract temp at this point
tempdata_coast <- raster::extract(temp_masked, occ_bg)

# extract prec at this point
precdata_coast <- raster::extract(prec_masked, occ_bg)

# bind extracted data as columns in a dataframe
finalcoastDF <- cbind(occ_bg@data$aedes, Hdata_coast, tempdata_coast, precdata_coast, occ_bg@data$wind) %>% 
  as.data.frame()

# rename dataframe columns
names(finalcoastDF) <- c("aedes", "H_index", "temp", "prec", "wind")

# classification into suitable and not suitable
finalcoastDF$H_class <- ifelse(finalcoastDF$H_index >= LPT, "suitable", "not_suitable")

# subset dataframe
finalcoastDF_sub <- subset(finalcoastDF, H_class == "suitable")
finalcoastDF_sub$occ_class <- ifelse(finalcoastDF_sub$aedes == 0, "non-occurrences", "occurrences")

# simple boxplot ..
finalcoastDF_sub$occ_class <- as.factor(finalcoastDF_sub$occ_class)
boxplot(finalcoastDF_sub$wind ~ finalcoastDF_sub$occ_class)

# logistic regression analyses

temp_mu <- mean(finalcoastDF_sub$temp, na.rm = T)
prec_mu <- mean(finalcoastDF_sub$prec, na.rm = T)
wind_mu <- mean(finalcoastDF_sub$wind, na.rm = T)

tempmax <- quantile(finalcoastDF_sub$temp, probs = 0.50)
precmax <- quantile(finalcoastDF_sub$prec, probs = 0.50)
windmax <- quantile(finalcoastDF_sub$wind, probs = 0.50, na.rm=T)

finalcoastDF_sub <- finalcoastDF_sub[finalcoastDF_sub$wind>windmax & finalcoastDF_sub$prec>precmax&finalcoastDF_sub$temp>tempmax, ]
finalcoastDF_sub <- na.omit(finalcoastDF_sub)
finalcoastDF_sub$test <- ifelse(finalcoastDF_sub$aedes == 1, 0, 1)

model1<- glm(test ~ wind + temp + prec, 
             data = finalcoastDF_sub, family = binomial)
summary(model1)

# Average marginal effect estimation (Leeper, 2017)
mar <- margins::margins(model1)
margins::margins(model1) %>% summary()

plot(mar, labels = c("prec", "temp", "wind"))


# Interpretation: As the occurrence values increases
# from 0 to 1, temperature increases by 0.09% and precipitation remains constant
# while wind increases by 0.31% 

writeRaster(H, "suitable.tif", overwrite=T)
writeRaster(H_masked, "suitable_coast.tif", overwrite=T)

writeOGR(occs.spdf, "aedes_occurrence", "points.shp", driver = "ESRI Shapefile")
