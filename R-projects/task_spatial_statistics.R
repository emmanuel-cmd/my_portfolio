
###################
## Homework: Assume all cells in the raster have the same area.
##       Calculate the percentage of artificial/agricultural/forest/... area
##       in the Landkreis Bayreuth.
##
## Hint: landcover2@data@values is all you need for that.

#landcover@data@values - Check the structure of landcover2
landcover2@data@values
str(landcover2)

#total number of cells
sub_total <- subset(landcover2@data@values, subset = !is.na(landcover2@data@values))
sub_total <- length(sub_total) # =21453=

# Number of cells with values 1
sub_1 <- subset(landcover2@data@values, subset = landcover2@data@values == 1)
sub_1 <- length(sub_1)

# Number of cells with values 2
sub_2 <- subset(landcover2@data@values, subset = landcover2@data@values == 2)
sub_2 <- length(sub_2)

# Number of cells with values 3
sub_3 <- subset(landcover2@data@values, subset = landcover2@data@values == 3)
sub_3 <- length(sub_3)

# Number of cells with values 4
sub_4 <- subset(landcover2@data@values, subset = landcover2@data@values == 4)
sub_4 <- length(sub_4)

# Number of cells with values 5
sub_5 <- subset(landcover2@data@values, subset = landcover2@data@values == 5)
sub_5 <- length(sub_5)

# Confirm 
sum(sub_1, sub_2, sub_3, sub_4, sub_5)

# Check for accuracy 
p1 + p2 + p3 + p4 + p5 # =100%=
