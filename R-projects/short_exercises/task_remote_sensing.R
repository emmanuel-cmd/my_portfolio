
install.packages("RStoolbox")
library(rgdal)
library(RStoolbox)
library(raster)
install.packages("glcm")
library(glcm)

image1988 <- raster("C:/Users/Emma/Filr/My Files/Remotesensing/Data/RSE_book_Brazil_deforestation/p224r63_1988.grd")
plot(image1988)
image2011 <- raster("C:/Users/Emma/Filr/My Files/Remotesensing/Data/RSE_book_Brazil_deforestation/p224r63_2011.grd")
plot(image2011)


library(RStoolbox)
data(lsat)
remove.packages(c("RStoolbox", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

## PCA for multiple window analyses, #on raster data
lsat_pca <- rasterPCA(lsat, spca = TRUE)
ggR(lsat_pca$map, 1:6, geom_raster = TRUE, stretch = "lin")
lsat_foc <- focal(lsat_pca$map[[1]], w = matrix(1,3,3), fun = sd)
ggR(lsat_foc, stretch = "lin")

weights <- focalWeight(lsat, d = 100, type = "Gauss")
weights #matrix
ggR(raster(weights)) #convert to a raster and plot
lsat_smooth <- focal(lsat[[1]], weights)
ggR(lsat_smooth, stretch = "lin")
ggR(lsat[[1]] - lsat_smooth)

setwd("Y:/")
##Supervised classification for 1986 data
td <- rgdal::readOGR("Y:\training.shp")
projection(td)
map1987 <- raster("Y:/map1987.tif")
projection(map1987)
td <- spTransform(td, projection(map1987))

sc <- superClass(map1987, trainData = td, responseCol = "training", filename = "training.tif")
plot(sc$map)

##Supervised classification for 2016 data
map2016 <- raster("Y:/map2016.tif")
sc1 <- superClass(map2016, trainData = td, responseCol = "training", filename = "training.tif")
plot(sc1$map)

#
change <- map1986*10 - map2016
plot(change)
freq(map1986)

old <- 1:4
new <- 1:4 * 10

tab <- data.frame(expand.grid(old,new))
tab$changeID <- tab$Var1 - tab$Var2
tab

classes <- c("forest", "agri", "water", "urban")
tab$oldclass <- classes[tab$Var1]
tab$newclass <- classes[tab$Var2/10]

#download images directly from the internet
devtools::install_github("16EAGLE/getSpatialData")

