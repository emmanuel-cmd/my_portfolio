################

# Load required libraries
library(raster)
library(ncdf4)
library(dplyr)

# open NCDFile as raster object
prcp <- brick("prcp-195101-grd-scaled.nc", var = "prcp")
prcp[prcp == NaN] <- NA

# plot raster map (e.g. day 1)
plot(prcp$X1951.01.01)

# read BDY file as text file
bound <- read.delim("02387000.BDY")

# create empty matrix to populate later in a for loop
out <- matrix(ncol=2, nrow=length(bound$X023870....621))

for (i in 1:length(bound$X023870....621)){
  out[i,] <- strsplit(bound$X023870....621[i], split = " ", fixed = T)[[1]][c(2,4)]
}

# convert matrix to dataframe
df_bound <- as.data.frame(out)

# convert string columns to numeric type and rename columns 
df_bound$V1 <- as.numeric(df_bound$V1)
df_bound$V2 <- as.numeric(df_bound$V2)
names(df_bound) <- c("Longitude", "Latitude")

# convert coordinates to SpatialPolygon object 
poly <- Polygon(df_bound[,c(1,2)], hole = T)
poly_list <- Polygons(list(poly), ID = "polygon1")
boundPolygon <- SpatialPolygons(list(poly_list), proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))

# plot polygon 
plot(boundPolygon)

# create empty list results to populate in a for loop 
results <- list()
for (i in 1:nlayers(prcp)){
  
  # overlay basin on raster and extract daily precip data and percentage of overlapping region
  results[[i]] <- extract(prcp[[i]], boundPolygon, weights = T, cellnumbers=T)
  
  # extract coordinates of polygons and bind to data
  results[[i]] <- cbind(results[[i]][[1]], coordinates(prcp[[i]])[results[[i]][[1]][,1],])
  
}
# Generated 'results' is a list containing raster cell number(ID), 
# daily precipitation estimate in that cell, fraction(%) of cell that is covered by polygon, 
# and the coordinates of the cells  


# convert list 'results' to a dataframe
finalDF<- do.call("rbind", results) %>% as.data.frame()
finalDF$weight <- finalDF$weight*100

# rename columns
names(finalDF) <- c("cell_number", "daily_precip", "cell_percentage", "Longitude", "Latitude")

# We can also calculate weighted average for each cell that overlaps with the polygon
finalDF$weighted_avg <- (finalDF$daily_precip*finalDF$cell_percentage)/finalDF$cell_percentage
head(finalDF)

# Finally, we calculate daily average precipitation (pavg) 
pavg <- c()
for (i in 1:length(results)){
  pavg[i] <- sum(results[[i]][,2] * (results[[i]][,3]*100)) / sum(results[[i]][,3]*100)
  
}

# create a dataframe from this data and export 
pavg <- as.data.frame(pavg)

# print dataframe
pavg

# Export results in .csv files
write.csv(finalDF, "results/weightedavgprecip.csv")
write.csv(pavg, "results/weightedavgdailyprecip.csv")


