###########################################################################
###################### GLOBAL TRANSMISSION RISK OF MALARIA DISEASE
###################### - Modelling the incidence of Plasmodium vivax
######################   Plasmodium vivax is the most geographically widespread species and second 
######################   largest contributor to clinical (symptomatic) malaria
######################   worldwide, yet historically, its place on the international
######################   health agenda has remained low
######################  Aims & Objectives
######################  -estimate the spatial distribution and proportion of people infected by malaria parasite
######################  -predict future endemic areas of the pathogen (with future population density) -- see whether high endemic areas coincide with dense populations
######################  -identify potential areas with high risk of malaria infection 
###################### (areas in which improved diagnosis and treatment efforts should be targeted) on the path to malaria elimination
###################### Species data source: Malaria Atlas Project
###################### Author(s): Emmanuel Adeleke
########################################################################


##############SET WORKING DIRECTORY & LOAD USEFUL PACKAGES #########################
setwd("M:/B5/project/Pv_Incidence/Raster Data/Pv_incidence_rate_rmean")

install.packages('Hmisc', dependencies = T, force = T, build = T)
library(Hmisc)
library(rms)
install.packages('raster')
library(raster)
library(mgcv)
source("varImpBiomod.R")
library(randomForest)
install.packages('dismo', dependencies = T)
library(dismo)
library(rgdal)
library(ellipse)
library(randomForest)
library(rJava)
library(XML)
library(rnaturalearth)
install.packages('malariaAtlas', dependencies = T)
library(malariaAtlas)
help("malariaAtlas")
install.packages('glcm', dependencies = T, force = T, build = T)
library(glcm)
install.packages('Rcpp', dependencies = TRUE)
library(Rcpp)
install.packages('ggplot2', dependencies = TRUE)
library(ggplot2)
install.packages('data.table', dependencies = TRUE)
library(data.table)
install.packages('RStoolbox')
library(RStoolbox)
library(sf)
install.packages('ggspatial', dependencies = T)
library(ggspatial)
library(DALEX2)
library(ingredients)

# environmental variables useful for malaria distribution
## distance to river/waterbody, distance to forest, elevation, temperature, precipitation, population density, political and economic instability, water quality/transparency
### TASKS 
### Create plot of points (coordinates) and color by pr with ggplot2
### Look into the help of glm and create a logistic regression
### determine environmental variables that affect species distribution 

##############--LOAD SPECIES DATA--#########################
mal_species <- read.csv("../../../public_pv_data.csv", header = T)
View(mal_species)
str(mal_species)
##########--ADD ABSENCES TO DATA--#####################
mal_species$negative <- mal_species$examined - mal_species$pv_pos

#mal_species$examined = total number of people examined  
#mal_species$pv_pos = total number of people tested malaria positive
#mal_species$negative = total number of people tested malara negative

############## LOAD COVARIATES ######################
############## Predictor variables influencing transmission of Plasmodium vivax
############## - distance of settlements to waterbodies-
############## - #distance to forest
############## - elevation-
############## - temperature-
############## - precipitation-
############## - population density-
############## - #water quality/transparency

###
mydata <- raster::getData(name = 'worldclim', var='bio', res=10, path = "./worldclim/")
settle_dist <- raster("../../../distance2water_30arcsec.tif")
elev <- raster("../../../GDEM-10km-colorized.tif")
pop_dense <- raster("../../../SEDAC_POP_2000-01-01_rgb_360x180.TIFF")
duffy_negative <- raster("../../../2011_Duffy_Negativity_Phenotype_Frequency_5k_Decompressed.geotiff")
plot(duffy_negative)

#resample raster layers and stack them together
settle_dist <- resample(settle_dist, mydata$bio1)
elev <- resample(elev, mydata$bio1)
pop_dense <- resample(pop_dense, mydata$bio1)
duffy_negative <- resample(duffy_negative, mydata$bio1)

#write out rasters to file, load and stack them together 
writeRaster(settle_dist, "settle_dist.tif")
writeRaster(elev, "elev.tif")
writeRaster(pop_dense, "pop_dense.tif")
writeRaster(duffy_negative, "duffy_negative.tif")
#
settle_dist <- raster("settle_dist.tif")
elev <- raster("elev.tif")
pop_dense <- raster("pop_dense.tif")
duffy_negative <- raster("duffy_negative.tif")
#
new.env.rasters <- stack(mydata$bio1, mydata$bio12, duffy_negative, settle_dist)
plot(new.env.rasters)

#Assign name to each raster layer
names(new.env.rasters) <- c('mean_annual_temp', "annual_precip", "settle_dist", "elev", "pop_dense")



################# VISUALIZATION OF POINTS #####################
points <- subset(mal_species, select = c('latitude', 'longitude'))
points <- na.omit(points)
points <-  st_as_sf(points, coords = c("longitude", "latitude"), crs = (" +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(points)
#to 'sp' format
points <- as(points, "Spatial")
plot(points)

plot(new.env.rasters, 1)
plot(points, add = T)

##test of collinearity among predictor variables
cm <- cor(getValues(new.env.rasters), use = "complete.obs")
plotcorr(cm, col=ifelse(abs(cm) > 0.7, "red", "grey"))
#test decision: keep all predictor variables as none of the correlation coefficients is >0.7

##combine species + environmental rasters
mal_species <- subset(mal_species, !is.na(mal_species$latitude))
mal_species <- cbind(mal_species, 
                     extract(new.env.rasters, points))
mal_species <- na.omit(mal_species)
##########--LOGISTIC REGRESSION MODEL--############
##Split datasets into training and testing set
model1 <- glm(cbind(pv_pos, negative) ~  + mean_annual_temp + annual_precip + settle_dist + elev + pop_dense,
              data = mal_species, family = binomial())
summary(model1)

## CROSS VALIDATION
set.seed(2)
fold <- kfold(mal_species, k = 5) #by = c(mal_species$positive))
# The variable cv_pred will contain the cross-validated 
# predictions
mal_species$cv_pred <- NA 
for (i in unique(fold)) {
  traindata <- mal_species[fold != i, ]
  testdata <- mal_species[fold == i, ]
  model1 <- glm(cbind(pv_pos, negative) ~  + mean_annual_temp + annual_precip + settle_dist + elev + pop_dense,
                data = traindata, family = binomial())
  mal_species$cv_pred[fold == i] <- predict(model1, testdata, type='response')  
}
####

mal_species$presence <- as.numeric(mal_species$pv_pos > 0) 
#mal_species$presence[is.na(mal_species$presence)] <- 0
round(somers2(mal_species$cv_pred, mal_species$presence), 2)
round(somers2(predict(model1, mal_species, 
                      type = "response"), mal_species$presence), 2)

ind2<- sample(1:nrow(mal_species), round(0.7 * nrow(mal_species)))
mal_species.train <- mal_species[ind2,]
model1.train <- predict(model1, newdata = mal_species.train, type = "response")
mal_species.test <- mal_species[-ind2,]
model1.test <- predict(model1, newdata = mal_species.test, type = "response")

library(rms)
val.prob(p=model1.train, y=mal_species.train$presence)
val.prob(p=model1.test, y=mal_species.test$presence)

################### VARIABLE IMPORTANCE ###############################
set.seed(2)
# install_dependencies()
glm_explainer <- explain(model1, 
                            data = mal_species[, c("mean_annual_temp", "annual_precip", "settle_dist", "elev", "pop_dense")],
                            y = mal_species[,"presence"], 
                            predict_function = function(model1, new_data) {predict(model1, new_data, clamp = FALSE, type = "response")},
                            label = "glm")

# Plot exponential functions relating measure of specie relative abundance to environmental variables
glm_response <- partial_dependency(glm_explainer, N=50)
plot(glm_response)

# variable importance plots 
fimp <- feature_importance(glm_explainer)
plot(fimp)

#create loss function 
glm_loss <- function(observed, predicted, 
                        background_weight = 100) {
  background_sum <- sum(predicted[observed == 0])
  -sum(log(predicted[observed==1] / background_sum))
}
# trying out loss functions 
loss_sum_of_squares(mal_species$presence, mal_species$cv_pred) #applied to observed and predicted values 
glm_loss(mal_species$presence, mal_species$cv_pred)
#variable importance plots based on loss_function
set.seed(2)
fimp_2 <- feature_importance(glm_explainer, loss_function = glm_loss)
plot(fimp_2)

###################### SPATIAL PREDICTIONS #########################
glm.pred <- new.env.rasters %>% predict(model1, progress = 'text')
plot(glm.pred)

n.pred <- model1 %>% predict(newdata=mal_species, type = "response")

# Calculate presence/absence data from projected occurrence probabilities
prob2presence <- function(prob) {
  # input is predicted probability of presence,
  # output is presence/absence
  prob > quantile(prob, 1-mean(prob))   
}
n.pred.presence <- prob2presence(n.pred)
as.numeric(n.pred.presence)
mean(n.pred.presence == mal_species)

######################## FUTURE PREDICTIONS #############################
#######Get datasets for mean_annual_temp, annual_precip, settle_dist, elev, population density
temp_future <- getData()
