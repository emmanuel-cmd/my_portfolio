##  FLOOD RISK ASSESSMENT ALONG THE RIVER MAIN IN BAVARIA ##
##  Research Question: 
##  1. Has the distance between urban centres and the river main changed since 2015?
##  2. What can we observe about changes in the waterlevels(topology) of the region?

## install and load all useful packages
install.packages('RStoolbox', dependencies = T)
library(RStoolbox)
install.packages('rgdal', dependencies = T)
library(rgdal)
install.packages('raster', dependencies = T)
library(raster)
install.packages('ggplot2', dependencies = T)
library(ggplot2)
install.packages('glcm', dependencies = T)
library(glcm)
library(viridis)
library(maptools)

#set working directory
getwd()
setwd("C:/Users/Emma/Documents/C7b")
#Import the raster layers for the 2016 data
b22015 <- raster("C:/Users/Emma/Documents/C7b/2015/2015b2.jp2")
b32015 <- raster("C:/Users/Emma/Documents/C7b/2015/2015b3.jp2")
b42015 <- raster("C:/Users/Emma/Documents/C7b/2015/2015b4.jp2")
b82015 <- raster("C:/Users/Emma/Documents/C7b/2015/2015b8.jp2")
b2015all <- stack(b22015, b32015, b42015, b82015)

#Import the raster layers for the 2019 data
b22019 <- raster("C:/Users/Emma/Documents/C7b/2019/2019b2.jp2")
b32019 <- raster("C:/Users/Emma/Documents/C7b/2019/2019b3.jp2")
b42019 <- raster("C:/Users/Emma/Documents/C7b/2019/2019b4.jp2")
b82019 <- raster("C:/Users/Emma/Documents/C7b/2019/2019b8.jp2")
b2019all <- stack(b22019, b32019, b42019, b82019)


#plot the images
par(mfrow = c(1,1))
plotRGB(b2015all, 4,3,2, stretch = "lin", main = "2016 image")
plotRGB(b2019all, 4,3,2, stretch = "lin", main = "2019 image")

#crop the images
ext <- drawExtent(show = T, col = "red")
ext
ext <- c(204668.4,237336.2,3148014,3196108)
ext2015 <- crop(b2015all, ext)
ext2019 <- crop(b2019all, ext)

#plot the new images
plotRGB(ext2015, stretch= "lin")
plotRGB(ext2019, stretch= "lin")

#save the images to file
writeRaster(ext2015,filename = "map2015.tif",overwrite=T)
writeRaster(ext2019,filename = "map2019.tif",overwrite=T)

#import raster images
map2015 <- brick("map2015.tif")
map2019 <- brick("map2019.tif")

######-Plot the images-#######
par(mfrow = c(2,2))
plotRGB(map2015, stretch = "lin")
plotRGB(map2019, stretch = "lin")

#calculate NDWI by defining a function
fun_ndwi<-function(green,nir){
  (green-nir)/(green+nir)
}

ndwi_2015 <- overlay(map2015[[2]], map2015[[4]], fun=fun_ndwi, forcefun = T)
ndwi_2019 <- overlay(map2019[[2]], map2019[[4]], fun=fun_ndwi, forcefun = T)

#plot the NDWI values 
par(mfrow=c(1,1))
plot(ndwi_2015, main = "NDWI 2015")
plot(ndwi_2019, main = "NDWI 2019")

#Local correlation between 2015 and 2019 raster images
#to see which areas of the raster are correlated 
#flood areas that have high correlation values have experienced little or no change
corLocal(ndwi_2015, ndwi_2019, ngb=11)
#applying the moving window 
focal_2015 <- focal(ndwi_2015, w=matrix(1/9, ncol = 3, nrow = 3), fun = sd)
plot(focal_2015, main = "standard deviation 2016")

texture_result2015 <- glcm(ndwi_2015)
texture_result2019 <- glcm(ndwi_2019)

names(texture_result2015)
names(texture_result2019)

par(mfrow=c(2,2))
plot(texture_result2015$glcm_variance, main = "GLCM variance for the year 2015")
plot(texture_result2019$glcm_variance, main = "GLCM variance for the year 2019")

par(mfrow=c(1,1))
plot(texture_result2016$glcm_homogeneity, main = "GLCM homogeneity for the year 2015")
plot(texture_result2018$glcm_homogeneity, main = "GLCM homogeneity for the year 2019")

## Temporal variability of NDWI images 


#Supervised classification for the 2015 data
library(sf)
xx <- st_read("C:/Users/Emma/Documents/C7b/training.dbf")
x <- rasterize(xx, r, 1)
plot(x)
td <- rgdal::readOGR("C:/Users/Emma/Documents/C7b/training.dbf")
td <- spTransform(td, projection(map2015))
sc2015 <- superClass(map2015, trainData = td, responseCol = "training", filename = "training.tif")

arg <- list(at=seq(1,5,1),labels= c("bareground","forest","grasslands","rivers/floodplains","settlement"))
color = c("white", "darkgreen", "green", "blue", "bisque2")
plot(sc2015$map, col = color, axis.arg = arg, main = "Supervised Classification for 2015")

#Supervised classification for the 2019 data. 
td2 <- rgdal::readOGR("C:/Users/Emma/Documents/C7b/training2019.dbf")
td2 <- spTransform(td2, projection(map2019))
sc2019 <- superClass(map2019, trainData = td2, responseCol = "training", filename = "training2019.tif")
plot(sc2019$map, col = color, axis.arg = arg, main = "Supervised Classification image for 2019")

#change detection analysis
change <- sc2015$map * 10 + sc2019$map 
plot(change, col = color, axis.arg = arg, main = "changes in water levels btw 2015 & 2019")

#Distance between cities and rivers/floodplains
r <- raster(xmn=600410, xmx=618670, ymn=5531030, ymx=5548690, res=1)

# Rasterize and set land pixels to NA
td1 <- td$id == 2
r2 <- rasterize(td1, r, 1)
r3 <- mask(is.na(r2), r2, maskvalue=1, updatevalue=NA)
plot(r2)


d <- distance(r3)
d <- d*r2