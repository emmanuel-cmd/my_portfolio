# Application - Species Distribution Modelling
library(tidyverse)
library(sf)
library(terra)
library(colorspace)
library(ggspatial)
library(dismo)
library(spatstat)
library(ROCR)
source("rasterdf.R")

# Tree Species Data
# The study will encompass the Cascade Mountain Range in Washington State. 
# This area encompasses three ecoregions: the Cascades, East Cascades, and North Cascades.
wacascades <- st_read("wacascades.shp", quiet = TRUE)  # these areas are read in these polygons 
abla <- read_csv("abla.csv") # presence-absence points for subalpine fir
psme <- read_csv("psme.csv") # presence-absence points for Douglas fir

# Convert these dataframes to sf objects and assign CRS
abla_pts <- st_as_sf(abla, 
                     coords = c("X", "Y")) 
st_crs(abla_pts) <- 4326
psme_pts <- st_as_sf(psme, 
                     coords = c("X", "Y")) 
st_crs(psme_pts) <- 4326

# The map of presence-absence points show that 
# subalpine fir is restricted to relatively high elevations near the Cascade crest
ggplot() +
  geom_sf(data = abla_pts,
          aes(color = as.character(abla)), 
          size = 0.25) +
  geom_sf(data = wacascades, 
          fill = NA) +
  scale_color_manual(name = "Subalpine Fir",
                     values = c("gray", "darkgreen"),
                     labels = c("Absent", "Present")) +
  annotation_scale(location = 'br') +
  theme_void()

# Douglas-fir is distributed more widely at lower elevations and along the 
# eastern edge of the Cascades
ggplot() +
  geom_sf(data = psme_pts,
          aes(color = as.character(psme)), 
          size = 0.25) +
  geom_sf(data = wacascades, 
          fill = NA) +
  scale_color_manual(name = "Douglas-fir",
                     values = c("gray", "darkgreen"),
                     labels = c("Absent", "Present")) +
  annotation_scale(location = 'br') +
  theme_void()

# One of the challenges with visualizing the points is that the overall density of plots is variable.
# One way to try to see the occurrence patterns more clearly is to compute kernel-weighted local means. 
# This technique is a type of point-to-raster conversion in which each raster cell in the output 
# is assigned a mean of the local presence/absence values, where points located closer to the cell 
# are given a higher weight than those more distant.

# To make this conversion, first convert to a projected CRS
boundary_utm <- st_transform(wacascades, 32610)
abla_utm <- st_transform(abla_pts, 32610)
psme_utm <- st_transform(psme_pts, 32610)

# The spatstat pkg contains a func for generating kernel-weighted means
# However it cannot with sf objects therefore we have to convert to ppp objects
abla_ppp <- as.ppp(abla_utm)
class(abla_ppp)

Window(abla_ppp) <- as.owin(boundary_utm)  # create an observation window and assign it to the ppp object
abla_ppp

# Now compute kernel-weighted means with Smooth.ppp func
abla_im <- Smooth.ppp(abla_ppp, 
                      sigma=10000,     #specifies the bandwidth of the kernel
                      eps=c(1000, 1000)) #specifies the grid cell size of the output grid
class(abla_im)

# Convert object to a SpatRaster object
abla_grid <- rast(abla_im)
crs(abla_grid) <-"epsg:32610"

# The output provides a smoothed estimate of the proportion of plots with subalpine fir
abla_df <- rasterdf(abla_grid)
ggplot(data = abla_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Subalpine Fir", 
                      low = "lightyellow", 
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_utm,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()

# The same procedure is performed for Douglas-fir to generate a smooted raster 
psme_ppp <- as.ppp(psme_utm)
Window(psme_ppp) <- as.owin(boundary_utm)
psme_im <- Smooth.ppp(psme_ppp, 
                      sigma=10000, 
                      eps=c(1000, 1000))
psme_grid <- rast(psme_im)
crs(psme_grid) <-"epsg:32610"

# Mapping the smoothed occurrence data shows that Douglas-fir is broadly distributed 
# in the south-western region which is not as clear when looking at the individual occurrence points
psme_df <- rasterdf(psme_grid)
ggplot(data = psme_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Douglas-fir", 
                      low = "lightyellow", 
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_utm,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()

# WorldClim Historical Climate Data
wcbio <- rast("wc2.1_30s_bio_washington.tif")
nlyr(wcbio)
names(wcbio)

# Rename layer names
wcbnames <- paste0("bio", c(1, 10:19, 2:9))
names(wcbio) <- wcbnames
wcbnames

# The Washington Cascades are reprojected into the same geographic CRS with WGS84 datum 
# to match the CRS of the plot data and WorldClim data
# Then use polygon to mask the cascades from the larger WorldClim data
boundary_wgs84 <- st_transform(wacascades, st_crs(wcbio))
wcbio_crop <- crop(wcbio, vect(boundary_wgs84))
wcbio_msk <- mask(wcbio_crop, vect(boundary_wgs84))

# The map of max temp during the warmest month of year highlight the effects of elevation
# with the highest temps at the fringes of the mountain range and in the larger river valleys
# The lowest temps occur along the Cascade crest and at the peaks of the large volcanoes
mtwm_df <- rasterdf(wcbio_msk[["bio5"]])
ggplot(data = mtwm_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Temperature (\u00B0C)", 
                      low = "lightyellow", 
                      high = "darkred",
                      na.value = NA) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()

# The map of precipitation during the wettest month shows the interaction of moist maritime air
# with topography with the highest value occuring west of the cascade crest and the lowest values
# in the rain shadow on the eastern slopes
mtwm_df <- rasterdf(wcbio_msk[["bio13"]])
ggplot(data = mtwm_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Precipitation (mm)", 
                      low = "lightblue", 
                      high = "darkblue",
                      na.value = NA) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()

# Modelling the Climatic Niche

# Subalpine Fir
# To prepare the data for modeling, use the extract function to obtain climate variables
# from wcbio_msk for every plot location in the subalpine dataset
abla_bio <- extract(wcbio_msk, vect(abla_pts)) %>%
  bind_cols(abla_pts) %>%
  as.data.frame()
names(abla_bio)

# To assess the predictive accuracy of the model, a subset of the data needs to be held 
# out from the calibration step and reserved for validation
set.seed(22003)
abla_train <- abla_bio %>%
  sample_frac(size = 0.7)

# The anti_join() function is then used to select the validation points as 
# all the observations in abla_bio that are not present in abla_train
abla_val <- abla_bio %>%
  anti_join(abla_train, by = "ID")

# Use BRT models to model the relationships btw presence/absence and 19 bioclim variables
abla_mod <- gbm.step(data = abla_train, 
                     gbm.x = 2:20, 
                     gbm.y = 21,
                     family = "bernoulli", # treat the response as a binomial(presensce/absence) var
                     tree.complexity = 3,
                     learning.rate = 0.01, 
                     bag.fraction = 0.5,
                     plot.main = FALSE, 
                     verbose = FALSE, 
                     silent = TRUE)

# Use summary to extract info about the relative importance of the predictor variables
# the relative imp is based on the number of times a variable is selected for splitting 
# the regression trees weighted by the resulting improvement to the BRT model.
# The importance values are then rescaled so they sum to 100.
abla_imp <- summary(abla_mod, plotit = FALSE)
abla_imp

# Generate partial residual plots to show nonparametric relationships btw subalpine fir occurrence 
# and predictor variables. 
# Over the range of temperatures in the study area, subalpine fir occurs at the lowest mean temps
# declines monotonically with increasing temps
# Subalpine fir occurrence also decreases slightly with mean temperature of the warmest quarter (bio10) 
# and precipitation in the warmest quarter (bio18) and is slightly higher at intermediate values of 
# mean temperature during the driest quarter (bio9)
gbm.plot(abla_mod, 
         n.plots = 4, 
         write.title = FALSE,
         plot.layout = c(2, 2))

# Map the predicted probability of occurrence
abla_cur <- predict(object = wcbio_msk, 
                    model = abla_mod, 
                    type = "response", 
                    na.rm = TRUE)   # suppresses predictions outside the study area where there are no data

# The predicted map show more local variability associated with topography
# In particular the species is predicted to be most abundant on high ridges at the Cascade crest and eastward
# The circular rings are the upper elevation limits on large volcanoes such as Mount Rainier and Mount Adams
abla_cur_df <- rasterdf(abla_cur)
ggplot(data = abla_cur_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Subalpine Fir", 
                      low = "lightyellow", 
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()

# Douglas-fir
# The data preparation and modelling steps are repeated for Douglas-fir
set.seed(22004)
psme_bio <- extract(wcbio_msk, vect(psme_pts)) %>%
  bind_cols(psme_pts) %>%
  as.data.frame()
psme_train <- psme_bio %>%
  sample_frac(size = 0.7)
psme_val <- psme_bio %>%
  anti_join(abla_train, by = "ID")
psme_mod <- gbm.step(data=psme_train, 
                     gbm.x = 2:20, 
                     gbm.y = 21,
                     family = "bernoulli", 
                     tree.complexity = 3,
                     learning.rate = 0.01, 
                     bag.fraction = 0.5,
                     plot.main = FALSE, 
                     verbose = FALSE, 
                     silent = TRUE)

# Different relative imp for Douglas-fir compared to Subalpine Fir
# The two most important variables are bio5 and bio10
psme_imp <- summary(psme_mod, plotit = FALSE)
psme_imp

# Partial residual plots for Douglas-Fir
# The relationships btw douglas fir and these 2 variables are unimodal with species occurrence 
# peaking across a range of temperatures and decreasing at cooler and warmer temperatures 
# Douglas-fir also has unimodal relationships with bio14 and bio17
gbm.plot(psme_mod, 
         n.plots = 4, 
         write.title = FALSE,
         plot.layout = c(2, 2))

# The predicted occurrence map shows that Douglas-fir is widespread throughout the southern part 
# of the Washington Cascades but is confined to lower-elevation valleys in the northern part of the region
psme_cur <- predict(object = wcbio_msk, 
                    model = psme_mod, 
                    type = "response", 
                    na.rm = TRUE)
psme_cur_df <- rasterdf(psme_cur)
ggplot(data = psme_cur_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "Douglas-fir", 
                      low = "lightyellow", 
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()

# Accuracy Assessment
# To understand how accurate the SDM predictions are, the predict function
# is used to generate predictions based on the validation dataset
abla_pred <- predict(abla_mod, 
                     newdata = abla_val,
                     type = "response")

# The vector of predicted values is combined with the observed values from the validation data
# the prediction() function from the ROCR package is used to generate a prediction object 
# that can be used for computing accuracy statistics.
abla_predobs <- prediction(abla_pred, abla_val$abla)

# Although the observations are binary, the output of the model is a continuous probability between zero and one. 
# The receiver operating characteristic curve (ROC) is commonly used to evaluate the predictive capability of such a model.
# The ROC is generated by using the predicted probability to classify presence or absence over a range of cutoff values 
# and exploring the tradeoff between predictions of presence and absence.
# The ROC curve describes the changes in true positive rates and false positive rates over all possible cutoff values
abla_roc = performance(abla_predobs, 
                       measure = "tpr", 
                       x.measure = "fpr")
class(abla_roc)

# The ROCR pkg uses the S4 object system. 
# These objects require new functions to visualize and extract their components, 
# which are stored in “slots”
slotNames(abla_roc)

# The x.values and y.values contain the false positive rates and the true positive rates for the ROC curve. 
# These data are stored as lists with a single element.
abla_fpr <- slot(abla_roc, "x.values")[[1]]
abla_tpr <- slot(abla_roc, "y.values")[[1]]
abla_aucplot <- data.frame(abla_fpr, abla_tpr)

# Use ggplot to graph the ROC curve
# As the false positive rate (equal to one minus the true negative rate) decreases, the true positive rate also decreases
ggplot(data = abla_aucplot) +
  geom_line(aes(x = abla_fpr,
                y = abla_tpr),
            col = "red") +
  labs(x = "False Positive Rate", 
       y = "True Positive Rate") +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0)) +
  coord_fixed() +
  theme_bw()

# Another way to visualize these relationships is to graph the overall accuracy, 
# true positive rate, and true negative rate as a function of the cutoff used for prediction.
# The performance() function is used to create performance objects containing these three statistics. 
# The values are then extracted and combined into a long-format data frame.
abla_all = performance(abla_predobs, 
                       measure = "acc")
abla_pos = performance(abla_predobs, 
                       measure = "tpr")
abla_neg = performance(abla_predobs, 
                       measure = "tnr")
cutoff <- slot(abla_all, "x.values")[[1]]
totacc <- slot(abla_all, "y.values")[[1]]
posacc <- slot(abla_pos, "y.values")[[1]]
negacc <- slot(abla_neg, "y.values")[[1]]

# Combine to a long-format dataframe
abla_accplot <- data.frame(cutoff,
                           totacc,
                           posacc,
                           negacc) %>%
  pivot_longer(cols = one_of("totacc", 
                             "posacc", 
                             "negacc"),
               values_to = "accval",
               names_to = "accstat")

# The results suggest that a cutoff value around 0.25 would be effective for 
# classifying presence or absence based on the predictions
# The overall accuracy is close to the maximum, and the true negative 
# and true positive rates are balanced and relatively high.
ggplot(data = abla_accplot) +
  geom_line(aes(x = cutoff, 
                y = accval,
                col = accstat)) +
  labs(x = "Classification Cutoff", 
       y = "Classification Accuracy",
       color = "Accuracy Statistic") +
  scale_color_discrete(labels = c("True Negative Rate",
                                  "True Positive Rate",
                                  "Overall Accuracy")) +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0)) +
  coord_fixed() +
  theme_bw()

# The Area under the ROC is frequently used as an index of the predictive power of a probabilistic 
# model for binary data like species presence/absence. Values range from 0.5 to 1.0

# The AUC for the subalpine fir model is 0.91, which is relatively high value and 
# indicates that predictions are accurate over a range of cutoff values.
abla_aucval <- performance(abla_predobs, measure = "auc")
slot(abla_aucval, "y.values")[[1]]

# Climate change projections
# The WorldClim dataset also includes projected future climate grids based on the outputs of GCMs from CMIP 6
# This example uses a projection for 2061-2080 from the MPI-ESM based on RCP4.5 forcing pathway
wcproj <- rast("wc2.1_30s_bioc_MPI-ESM1-2-HR_ssp245_2061-2080_wa.tif")
nlyr(wcproj)

wcbnames <- paste0("bio", 1:19)
names(wcproj) <- wcbnames
wcproj_crop <- crop(wcproj, vect(boundary_wgs84))
wcproj_msk <- mask(wcproj_crop, vect(boundary_wgs84))

# The terra predict func is used to generate predictions of subalpine fir distribution 
# under the future climate scenario

abla_proj <- predict(object = wcproj_msk,  # this raster must have the same layer name as the original variables that were used to train the model. 
                     model = abla_mod, 
                     type = "response", 
                     na.rm = TRUE)
abla_chg <- c(abla_cur, abla_proj)
names(abla_chg) <- c("Current", "Future")

# To display the predictions as binary presence/absence outcomes, the probabilities are 
# classified using a cutoff of 0.25
abla_clas <- ifel(abla_chg > 0.25, 1, 0)


# Comparing these two maps shows that the projected range of subalpine fir is considerably 
# smaller under a future, warmer climate, with the species restricted to the highest elevations 
# in the Washington Cascades
abla_clas_df <- rasterdf(abla_clas)
ggplot(data = abla_clas_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) +
  scale_fill_manual(name = "Subalpine Fir", 
                    values = c("lightyellow", 
                               "darkgreen"),
                    labels = c("absent",
                               "present"),
                    na.translate = FALSE) +
  facet_wrap(~ variable) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  coord_sf(expand = F) +
  theme_void()

# In addition to generating maps, the dataframe generated by the rasterdf can be used to create
# other types of graphs based on the values in the raster dataset. Here, histograms of current 
# and future mean annual temperatures show that the distribution of temperatures 
# will be considerably higher under the future climate scenario
annmean <- c(wcbio_msk[["bio1"]], wcproj_msk[["bio1"]])
names(annmean) <- c("Current", "Future")
annmean_df <- rasterdf(annmean) 

ggplot(data = annmean_df) +
  geom_histogram(aes(x = value), bins = 20) +
  labs(x = "Mean Annual Temperature (\u00B0C)", 
       y = "Count of observations") +
  facet_wrap(~ variable, ncol = 1) +
  theme_bw()

# PRACTISE
# Carry out an accuracy assessment of the species distribution model for Douglas-fir 
# and compare the results to those obtained for subalpine fir.

# Generate predictions based on the validation dataset
psme_pred <- predict(psme_mod, 
                     newdata = psme_val,
                     type = "response")
psme_predobs <- prediction(psme_pred, psme_val$psme)

# Generate a performance object that includes the TPR and FPR

psme_roc = performance(psme_predobs, 
                       measure = "tpr", 
                       x.measure = "fpr")

psme_fpr = slot(psme_roc, "x.values")[[1]]
psme_tpr = slot(psme_roc, "y.values")[[1]]
psme_aucplot = data.frame(psme_fpr, psme_tpr)

# Graph the ROC curve
ggplot(data = psme_aucplot) +
  geom_line(aes(x = psme_fpr,
                y = psme_tpr),
            col = "red") +
  labs(x = "False Positive Rate", 
       y = "True Positive Rate") +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0)) +
  coord_fixed() +
  theme_bw()

# Graph the TPR, TNR as a function of the cutoff used for prediction
psme_all = performance(psme_predobs, 
                       measure = "acc")
psme_pos = performance(psme_predobs, 
                       measure = "tpr")
psme_neg = performance(psme_predobs, 
                       measure = "tnr")
pscutoff <- slot(psme_all, "x.values")[[1]]
pstotacc <- slot(psme_all, "y.values")[[1]]
psposacc <- slot(psme_pos, "y.values")[[1]]
psnegacc <- slot(psme_neg, "y.values")[[1]]

# Combine into a long-format dataframe 
psme_accplot <- data.frame(pscutoff,
                           pstotacc,
                           psposacc,
                           psnegacc) %>%
  pivot_longer(cols = one_of("pstotacc", 
                             "psposacc", 
                             "psnegacc"),
               values_to = "accval",
               names_to = "accstat")

# Plot
# The results suggest that using a cutoff of 0.65 would be effective for classifying presence/absence 
# based on the predictions
ggplot(data = psme_accplot) +
  geom_line(aes(x = pscutoff, 
                y = accval,
                col = accstat)) +
  labs(x = "Classification Cutoff", 
       y = "Classification Accuracy",
       color = "Accuracy Statistic") +
  scale_color_discrete(labels = c("True Negative Rate",
                                  "True Positive Rate",
                                  "Overall Accuracy")) +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0)) +
  coord_fixed() +
  theme_bw()

# Compute AUC value
psme_aucval <- performance(psme_predobs, measure = "auc")
slot(psme_aucval, "y.values")[[1]]

# 2. Predict the future distribution of Douglas-fir using the climate projections 
# and compare the results with the projected future distribution of subalpine fir.

psme_proj <- predict(object = wcproj_msk,  # this raster must have the same layer name as the original variables that were used to train the model. 
                     model = psme_mod, 
                     type = "response", 
                     na.rm = TRUE)
psme_fut <- c(abla_proj, psme_proj)
names(psme_fut) <- c("SubalpFuture", "DougFirFuture")

psme_fut_df <- rasterdf(psme_fut)

# Comparing the future prediction of subalpine fir to douglas fir
ggplot(data = psme_fut_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = value)) +
  scale_fill_gradient(name = "", 
                      low = "lightyellow", 
                      high = "darkgreen",
                      na.value = NA) +
  facet_wrap(~ variable) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  coord_sf(expand = F) +
  theme_void()

# Compare current to future predictions of Douglas fir
psme_chg = c(psme_cur, psme_proj)
names(psme_chg) = c("Current", "Future")

# Create a binary prediction
psme_clas <- ifel(psme_chg > 0.65, 1, 0)

# Convert to dataframe and plot
psme_clas_df <- rasterdf(psme_clas)

ggplot(data = psme_clas_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) +
  scale_fill_manual(name = "Douglas Fir", 
                    values = c("lightyellow", 
                               "darkgreen"),
                    labels = c("absent",
                               "present"),
                    na.translate = FALSE) +
  facet_wrap(~ variable) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  coord_sf(expand = F) +
  theme_void()

# Generate a map that shows where the distribution of subalpine fir 
# is projected to expand, contract, and remain the same under future climate projections. 
# Then generate a similar map for Douglas-fir and compare the two.

# Get subalpine fir distributions that are projected to contract by 
# dropping areas that have remained the same and projected to expand
abla_exp = abla_cur >= abla_proj
abla_exp[abla_exp==1] <- NA

# Get areas that have expanded
abla_expanded = mask(abla_cur, abla_exp)
names(abla_expanded) = "expanded"

# Get subalpine fir distributions that are projected to expand by 
# dropping areas that have remained the same and projected to contract
abla_cont = abla_cur <= abla_proj
abla_cont[abla_cont==1] <- NA

# Get areas that have contracted
abla_contracted = mask(abla_cur, abla_cont)
names(abla_contracted) = "contracted"

# Stack the two raster 
ablaexpcont = c(abla_expanded, abla_contracted)
ablaexpcontdf = rasterdf(ablaexpcont)

# Create categorical values for expanded vs contracted areas
ablaexpcontdf[!is.na(ablaexpcontdf$value)&ablaexpcontdf$variable=="expanded","value"] <- 1 
ablaexpcontdf[!is.na(ablaexpcontdf$value)&ablaexpcontdf$variable=="contracted","value"] <- 2

# Plot using ggplot
ggplot(data = ablaexpcontdf) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) +
  scale_fill_manual(name = "Subalpine Fir projections", 
                    values = c("blue", 
                               "pink"),
                    labels = c("expanded areas",
                               "contracted areas"),
                    na.translate = FALSE) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  coord_sf(expand = F) +
  theme_void()

# Do the same for Douglas Fir

# Get douglas fir distributions that are projected to expanded by 
# dropping areas that have remained the same and projected to contract
psme_exp = psme_cur >= psme_proj
psme_exp[psme_exp==1] <- NA

# Get areas that have expanded
psme_expanded = mask(psme_cur, psme_exp)
names(psme_expanded) = "expanded"

# Get douglas fir distributions that are projected to contract by 
# dropping areas that have remained the same and projected to expand
psme_cont = psme_cur <= psme_proj
psme_cont[psme_cont==1] <- NA

# Get areas that have contracted
psme_contracted = mask(psme_cur, psme_cont)
names(psme_contracted) = "contracted"

# Stack the two raster layers
psmecontexp = c(psme_contracted, psme_expanded)
psmecontexpdf = rasterdf(psmecontexp)

# Create categories for expanded vs contracted areas
psmecontexpdf[!is.na(psmecontexpdf$value)&psmecontexpdf$variable=="expanded", "value"] <- 1
psmecontexpdf[!is.na(psmecontexpdf$value)&psmecontexpdf$variable=="contracted", "value"] <- 2

# Add species column
ablaexpcontdf$species = "subalpine-fir"
psmecontexpdf$species = "douglas-fir"

# Combine into one dataframe for plotting
ablapsmedfplt = rbind(ablaexpcontdf, psmecontexpdf)


# Generate plot
ggplot(data = ablapsmedfplt) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) +
  scale_fill_manual(name = "", 
                    values = c("blue", 
                               "pink"),
                    labels = c("expanded areas",
                               "contracted areas"),
                    na.translate = FALSE) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  facet_wrap(~ species) + 
  coord_sf(expand = F) +
  theme_void()
