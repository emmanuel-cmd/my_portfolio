# Rasterdiv: Derive indices of diversity from NDVI
require(rasterdiv)
require(rasterVis)
require(RColorBrewer)
require(dplyr)

# Stretch values to (-1,1) scale 
raster::stretch(copNDVI,minv=-1,maxv=1) %>% plot()

# Reclassify NDVI
copNDVInew <- raster::reclassify(copNDVI, cbind(252,255, NA), right=TRUE)

# Resample NDVI to a coarser resolution
# Resample using raster::aggregate and a linear factor of 20
copNDVIlr <- raster::aggregate(copNDVInew, fact=20)
#Set float numbers as integers to further speed up the calculation
storage.mode(copNDVIlr[]) = "integer"

# Compare NDVI low and high resolution
levelplot(copNDVI,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~8km pixel resolution")
# 150km resolution
levelplot(copNDVIlr,layout=c(0,1,1), main="NDVI 21st of June 1999-2017 - ~150km pixel resolution")

# COMPUTE AREA BASED RAO'S INDEX
RaoC <- paRao(x=copNDVIlr, area=world, field='CONTINENT', alpha=c(1,2))

#Plot area-based RAo's index
plot(RaoC, col=hcl(RaoC$alpha.1*10), main="Rao's index per continent alpha 1")
text(RaoC, label=paste("Rao'Q =", round(RaoC$alpha.1,1)), col="black", family="Arial")

# Create continous NDVI raster values 
copNDVIcont <- raster::aggregate(copNDVInew, fact=20)

# Compute all indexes in rasterdiv
#Shannon's Diversity
sha <- Shannon(copNDVIcont,window=9,na.tolerance=0.1,np=1)
plot(sha)
#Pielou's Evenness
pie <- Pielou(copNDVIcont,window=9,na.tolerance=0.1,np=1)

#Berger-Parker's Index
ber <- BergerParker(copNDVIcont,window=9,na.tolerance=0.1,np=1)

#Parametric Rao's quadratic entropy with alpha ranging from 1 to 5
prao <- paRao(copNDVIcont,window=9,alpha=1:5,na.tolerance=0.1,dist_m="euclidean",np=1)

#Cumulative Residual Entropy
cre <- CRE(copNDVIcont,window=9,na.tolerance=0.1,np=1)

#Hill's numbers
hil <- Hill(copNDVIcont,window=9,alpha=seq(0,2,0.5),na.tolerance=0.1,np=1)

#Rényi's Index
ren <- Renyi(copNDVIcont,window=9,alpha=seq(0,2,0.5),na.tolerance=0.1,np=1)