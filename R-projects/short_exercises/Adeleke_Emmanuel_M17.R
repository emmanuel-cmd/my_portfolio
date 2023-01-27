### REMOTE SENSING OF VEGETATION COVER CHANGES IN THE TROPICAL RAINFOREST OF 
###                     SOUTHWESTERN NIGERIA (1986 - 2016)
###               A case study of Umuahia forest in SW Nigeria 
### Hypothesis1: There was a reduction in the size of healthy vegetation 
###              from 1986 to 2016
### Hypothesis2: Deforestation has greatly influenced vegetation cover changes in the region
###
### Objectives: 
###-> Find the differences in the health of vegetation between 1986 and 2016 using the 
###   NDVI
###-> Show the extent of deforestation using the Unsupervised classification method
###
### Data Used: LandSat 8 and LandSat 4 GEOTiff images were used for this project.
###            The images were downloaded from the USGS Website. 
### 
### Author(s): Emmanuel Adeleke

## Get and set the working directory
getwd()
setwd("C:/Users/Emma/Documents/Vegetation_Data")

## install and load all useful packages
install.packages("RStoolbox")
library(RStoolbox)
install.packages("rgdal")
library(rgdal)
install.packages("raster")
library(raster)
install.packages("ggplot2")
library(ggplot2)

#Import the raster layers for the 1986 data
band1_1986 <- raster("C:/Users/Emma/Documents/Vegetation_Data/1986_B1.tif")
band2_1986 <- raster("C:/Users/Emma/Documents/Vegetation_Data/1986_B2.tif")
band3_1986 <- raster("C:/Users/Emma/Documents/Vegetation_Data/1986_B3.tif")
band4_1986 <- raster("C:/Users/Emma/Documents/Vegetation_Data/1986_B4.tif")
band5_1986 <- raster("C:/Users/Emma/Documents/Vegetation_Data/1986_B5.tif")
band6_1986 <- raster("C:/Users/Emma/Documents/Vegetation_Data/1986_B6.tif")
band7_1986 <- raster("C:/Users/Emma/Documents/Vegetation_Data/1986_B7.tif")

#Stack all the 1986 bands together
allbands_1986 <- stack(band1_1986, band2_1986, band3_1986, band4_1986, band5_1986, band6_1986, band7_1986)

#RGB plot of the stacked bands
plotRGB(allbands_1986, stretch="hist")

#Cropping raster
ext <- drawExtent(show=T, col="red")
allbands_1986 <- crop(allbands_1986, ext)

#export raster to file
writeRaster(allbands_1986, "Umuahia_1986.tif")

#import the Umuahia_1986 file
allbands_1986 <- brick("Umuahia_1986.tif")

# Generate a RGB plot of the raster data
plotRGB(allbands_1986, r=4, g=5, b=3)

#Importing the raster layers for the 2016 data
band1_2016 <- raster("2016_B1.tif")
band2_2016 <- raster("2016_B2.tif")
band3_2016 <- raster("2016_B3.tif")
band4_2016 <- raster("2016_B4.tif")
band5_2016 <- raster("2016_B5.tif")
band6_2016 <- raster("2016_B6.tif")
band7_2016 <- raster("2016_B7.tif")

#Stacking all the 2016 bands together
allbands_2016 <- stack(band1_2016, band2_2016, band3_2016, band4_2016, band5_2016, band6_2016, band7_2016)
# RGB plot of the stacked bands
plotRGB(allbands_2016, stretch="hist")

#Cropping raster 
ext <- drawExtent(show=T, col="red")
allbands_2016 <- crop(allbands_2016, ext)

#exporting 2016 raster to file
writeRaster(allbands_2016, "Umuahia_2016.tif")

#import the Umuahia_2016 file
allbands_2016 <- brick("Umuahia_2016.tif")

#RGB plot of the data
x11(); par(mfrow = c(1,2))
plotRGB(allbands_1986,7,4,2, stretch="lin", main = "Umuahia 1986")
plotRGB(allbands_2016,7,5,3, stretch="lin", main = "Umuahia 2016")

#Computing the NDVI 
fun_ndvi <- function(nir, red) {
  
  (nir - red) / (nir + red)
}
ndvi_1986 <- overlay(allbands_1986$Umuahia_1986.4, allbands_1986$Umuahia_1986.3, fun=fun_ndvi, forcefun=T)


ndvi_2016 <- overlay(allbands_2016$Umuahia_2016.5, allbands_2016$Umuahia_2016.4, fun=fun_ndvi, forcefun=T)

# plot NDVI
x11(); par(mfrow = c(1,2))
plot(ndvi_1986, zlim = c(0,0.5))
plot(ndvi_2016, zlim = c(0,0.5))

#NDVI Values: 
summary(ndvi_1986)
summary(ndvi_2016)

### Results of the NDVI: 
##  Differences of the NDVI values shows that there has been a reduction in vegetation density
##  from 1986 to 2016. 
##  More information can be found in the NDVI plots

### Unsupervised Classification using the Kmeans function
##  Convert all the raster bands to vector
nr_1986 <- getValues(ndvi_1986)
nr_2016 <- getValues(ndvi_2016)
str(nr_1986) #Raster layers were converted to array
str(nr_2016)

set.seed(99) #we want a uniform random classification

# We want to create 5 clusters, allow 10 iterations, start with 1 random set using 'Lloyd' method
kmn_cluster <- kmeans(na.omit(nr_1986), centers = 5, iter.max = 500, nstart = 1, algorithm = "Lloyd")
kmn_cluster1 <- kmeans(na.omit(nr_2016), centers = 5, iter.max = 500, nstart = 1, algorithm = "Lloyd")

str(kmn_cluster)
str(kmn_cluster1)
# Creating a copy of the 1986 & 2016 layers
ndvi1986 <- stack(ndvi_1986)
ndvi2016 <- stack(ndvi_2016)

# Replace raster cell values with kmn_cluster & kmn_cluster1
ndvi1986[] <- kmn_cluster$cluster
ndvi2016[] <- kmn_cluster1$cluster

values(ndvi1986) <- kmn_cluster$cluster
values(ndvi2016) <- kmn_cluster1$cluster

## Define colors vector for 5 clusters 
mycolor <- c("#00ff00", "#c3ff5b", "#fef65b", "#fef65b", "#daa520")

# Definition of classes and their colours
# -> Forest(Green)
# -> Grasslands(Light Green)
# -> Agricultural areas(Yellow)
# -> Rivers(blue)
# -
# Plot the NDVI's and the Unsupervised classifications 
x11(); par(mfrow=c(2,2))
plot(ndvi_1986, col = rev(terrain.colors(10)), main = "Umuahia NDVI 1986", zlim =c(0,0.5))
plot(ndvi1986, main = "Unsupervised classification 1986", col = mycolor)
plot(ndvi_2016, col = rev(terrain.colors(10)), main = "Umuahia NDVI 2016", zlim=c(0,0.5))
plot(ndvi2016, main = "Unsupervised classification 2016", col = mycolor)

### Results of the Unsupervised Classifications:
##  From the plots of the Unsupervised Classification for the two time periods(1986 and 2016), 
##  we can observe a change in the landcover type, typically from forest to agricultural areas. 
##  

# Check the values of the Raster Layers
freq(ndvi1986)
freq(ndvi2016)
a <- as.vector(ndvi1986)
b <- as.vector(ndvi2016)
a



  


