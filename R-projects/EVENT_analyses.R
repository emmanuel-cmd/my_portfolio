########################################################################
########### ---- DATA ANALYSIS FOR B2 EVENT II PROJECT ----- ###########
# Topic: The effect of winter warming and summer warming on productivity, 
# biodiversity and competition in grassland communities.
# Authors: Brandt Coleman, Cara Schulte, Emmanuel Adeleke.  

rm(list=ls())

##Load useful packages
library(vegan)
library(stats)
library(reshape)
library(xlsx)
library(pgirmess)

## Set working directory
setwd("Y:/winter_semester20192020/Neuer Ordner")## Load dataset
data <- read.csv("new_data.csv", header = T, sep = ",")
str(data)
View(data)

## Data preparation
class(data$cover_1x1)
data$cover_1x1 <- as.numeric(data$cover_1x1)
levels(data$treatment_2d_order)[c(1,2,3,4)] <- "control"
levels(data$Plotname)[17:21] <- c("CA-N1-1", "CA-N1-2", "CA-N1-3", "CA-N1-4", "CA-N1-5")
data <- data[-161,]

#remove 2008 and 2009 datasets
data <- data[!(c(data$year == 2008) | c(data$year == 2009)), ]
View(data)

#transform to a community data matrix
data.matrix <- cast(data, Plotname + treatment_2d_order + year + tme_from ~ Species.names, fun.aggregate = mean, value='cover_1x1')
#data.matrix <- data.matrix[-1,-5]
nrow(data.matrix)
rownames(data.matrix) <- 1:150
View(data.matrix)

#replace NA values with 0
data.matrix[is.na(data.matrix)] <- 0
#data.matrix[, 5:84] <- decostand(data.matrix[, 5:84], method = "standardize")
#data.matrix[, 5:84] <- data.matrix[, 5:84] + 10
View(data.matrix)

# shorten species names 
names(data.matrix) <- make.cepnames(names(data.matrix), seconditem = T)
shnam <- names(data.matrix[, 5:84])
#write out community data matrix
write.xlsx(data.matrix, file = "data.xlsx")

#DCA
data_rana <- decorana(data.matrix[, 5:84])

#RDA
data_rda.ctrl <- rda(data.matrix[1:50, 5:84], scale = T)
data_rda.winter <- rda(data.matrix[51:100, 5:84], scale = T)
data_rda.summer <- rda(data.matrix[101:150, 5:84], scale = T)
plot(data_rda.ctrl)
#ordination plot for the control plot
#pl <- plot(data_rda.ctrl, dis = "sp")
#identify(pl, "sp", labels = shnam)
stems <- colSums(data.matrix[1:50, 5:84])
plot(data_rda.ctrl, dis = "sp", type = "n", main = "Control plot")
sel <- orditorp(data_rda.ctrl, dis="sp", lab=shnam, priority=stems, pcol = "red", pch="+")

#ordination plot for the winter plot
#pl <- plot(data_rda.summer, dis = "sp")
#identify(pl, "sp", labels = shnam)
stems <- colSums(data.matrix[51:100, 5:84])
plot(data_rda.winter, dis = "sp", type = "n", main = "Winter Warming")
sel <- orditorp(data_rda.winter, dis="sp", lab=shnam, priority=stems, pcol = "red", pch="+")

#ordination plot for the summer plot
#pl <- plot(data_rda.summer, dis = "sp")
#identify(pl, "sp", labels = shnam)
stems <- colSums(data.matrix[101:150, 5:84])
plot(data_rda.summer, dis = "sp", type = "n", main = "Summer Warming")
sel <- orditorp(data_rda.summer, dis="sp", lab=shnam, priority=stems, pcol = "red", pch="+")

par(mfrow = c(2,2))
par(mfrow = c(1,1))

# Calculate the simpson diversity index
a <- diversity(data.matrix[1:50, 5:84], index = "simpson", MARGIN = 1, base = exp(1))
mean(a) #control         

b <- diversity(data.matrix[51:100, 5:84], index = "simpson", MARGIN = 1, base = exp(1))
mean(b) #winter_warming

c <- diversity(data.matrix[101:150, 5:84], index = "simpson", MARGIN = 1, base = exp(1))
mean(c) #summer_warming

# Prepare datasets for ANOVA
new <- matrix(rbind(a,b,c)) 
new <- as.data.frame(new, optional = T, row.names = 1:150)
treatment2dorder <- c(rep("control", 50), rep("winter_warming", 50), rep("summer_warming", 50))
new <- cbind(treatment2dorder, new)
colnames(new) <- c("treatment2dorder", "simpson_index")
shapiro.test(new$simpson_index)

#ANOVA test comparing simpson diversity index among treatment groups 
anova_model <- aov(simpson_index ~ treatment2dorder, data = new)
anova(anova_model)
TukeyHSD(anova_model)
par(mfrow = c(1,1))
boxplot(simpson_index ~ treatment2dorder, data = new, ylab = "simpson_diversity_index", main = "")
boxplot(simpson_index ~ treatment2dorder, data = new, ylim = c(.90, .96), ylab = "simpson_diversity_index", main = "")

kruskalmc(simpson_index ~ treatment2dorder, data = new)
kruskal.test(simpson_index ~ treatment2dorder, data = new)
##########2. ABOVE GROUND COMPETITION ######
new_light_height <- read.xlsx("new_biomass_light_height.xlsx", header = T, sheetIndex = 1)
new_light_height <- new_light_height[-c(121:160),]

#average
new_light_height$PAR <- (new_light_height$PAR_links + new_light_height$PAR_recht)/2
View(new_light_height) #overview

#write to file
write.xlsx(new_light_height, "new_biomass_light_height2.xlsx")

#####read file
new_light_height <- read.xlsx("new_biomass_light_height3.xlsx", sheetIndex = 1, header = T)
new_light_height$PAR <- new_light_height$PAR_above/new_light_height$PAR_within
View(new_light_height)

#relationship between light and height
lm_model <- lm(PAR ~ Height, data = new_light_height)
summary(lm_model)
as.vector(lm_model$coefficients[2])
#lm_mod_ctrl <- lm(PAR ~ Height, data = new_light_height[new_light_height$treatment_2d_order=="control" ,])
#summary(lm_mod_ctrl)

res_collection_df <- NULL
for(i in levels(new_light_height$Plot.ID)){
  tmp_df <- subset(new_light_height, new_light_height$Plot.ID==i)
  tmp_df <- droplevels(tmp_df)
  tmp_lm <- lm(PAR ~ Height, data = tmp_df)
  excoeff <- tmp_lm$coefficients[2]
  tmp_treat <- levels(tmp_df$Plot.ID)
  res_df <- cbind(excoeff, tmp_treat)
  res_collection_df <- rbind(res_collection_df, res_df)
}
res_collection_df <- as.data.frame(res_collection_df)
res_collection_df$excoeff <- as.numeric(as.vector(res_collection_df$excoeff))
res_collection_df$treatment <- c(rep('winterwarming', 5), rep('summerwarming', 5), rep('control', 5))

subset(new_light_height, new_light_height$Height == 0)

new_light_height$Plot.ID <- as.character(new_light_height$Plot.ID)
new_light_height$Height <- as.factor(new_light_height$Height)
lm(tmp_df$PAR ~ as.numeric(as.vector(tmp_df$Height)))

res_collection_df <- NULL
for(i in levels(new_light_height$Height)){
  tmp_df <- subset(new_light_height, new_light_height$Height==i)
  tmp_df <- droplevels(tmp_df)
  tmp_lm <- lm(tmp_df$PAR ~ tmp_df$Height)
  excoeff <- tmp_lm$coefficients[2]
  tmp_treat <- levels(tmp_df$Height)
  res_df <- cbind(excoeff, tmp_treat)
  res_collection_df <- rbind(res_collection_df, res_df)
}
res_collection_df <- as.data.frame(res_collection_df)
res_collection_df$excoeff <- as.numeric(as.vector(res_collection_df$excoeff))
res_collection_df$treatment <- c(rep('winterwarming', 5), rep('summerwarming', 5), rep('control', 5))


#ANOVA -comparison between treatments
shapiro.test(res_collection_df$excoeff)
aov_modell <- aov(excoeff ~ treatment, data = res_collection_df)
TukeyHSD(aov_modell)
boxplot(excoeff ~ treatment, data = res_collection_df)
boxplot(excoeff ~ treatment, data = res_collection_df, ylim = c(-0.06, 0.00))

#################################################################################
install.packages("pgirmess")
library(pgirmess)
#set working directory
getwd()
setwd("C:/Users/Emma/Documents/Course materials/Winter20192020/Neuer Ordner")

#load the table
biom_data <- read.xlsx(file="OurB2_Biomass_Light_Height.xlsx", sheetIndex = T, header=TRUE,dec=",",sep=";")
colnames(biom_data)[10] <- "total_biomass"#rename the columnname
#plot treatments
boxplot(total_biomass~treatment_2d_order, data = biom_data)

levels(biom_data$treatment_2d_order)[1] <-"control"
levels(biom_data$treatment_2d_order)[2] <- "SummerWarming"
levels(biom_data$treatment_2d_order)[3] <- "WinterWarming"


#creating subsets for the years
biom_2008<-subset(biom_data, year ==2008)
biom_2009<-subset(biom_data, year ==2009)
biom_2010<-subset(biom_data, year ==2010)
biom_2011<-subset(biom_data, year ==2011)
biom_2012<-subset(biom_data, year ==2012)
biom_2013<-subset(biom_data, year ==2013)
biom_2014<-subset(biom_data, year ==2014)
biom_2015<-subset(biom_data, year ==2015)
biom_2016<-subset(biom_data, year ==2016)
biom_2017<-subset(biom_data, year ==2017)
biom_2018<-subset(biom_data, year ==2018)
biom_2019<-subset(biom_data, year ==2019)
#plot year 2008
boxplot(total_biomass~treatment_2d_order, data = biom_2008)
mtext(text=c("control","SummerWarming","WinterWarming"),at=c(1,2,3))
#checking for normal distribution of the total 
shapiro.test(biom_data$total_biomass)#not normally distributed


kruskalmc(total_biomass~treatment_2d_order, data = biom_data)
# no difference between summer and control, but between summer and winter and winter and control

?log
biom_data$total_biomass <- log10(biom_data$total_biomass)
biom_data$total_biomass <- exp(biom_data$total_biomass)
shapiro.test(biom_data$total_biomass)

hist(biom_data$total_biomass)

aov_model <- aov(total_biomass ~ treatment_2d_order, data = biom_data)
anova(aov_model)
str(biom_data)
TukeyHSD(aov_model)
#plot(aov_model,which=2)
boxplot(total_biomass~treatment_2d_order, data = biom_data,ylab="Biomass",main="Productivity",col="darkgreen",border="orange") #ylim =c(2,6))
boxplot(total_biomass~treatment_2d_order, data = biom_data,ylab="Biomass",main="Productivity")
mtext(text=c("control","SummerWarming","WinterWarming"),at=c(1,2,3))


