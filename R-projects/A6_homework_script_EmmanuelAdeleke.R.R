##############################################################################
## This script was used to calculate parameters relevant
## for fluxes of matter and energy between the biosphere, 
## atmosphere and hydrosphere
## Author(s): Emmanuel Adeleke 
##############################################################################

######## Set the working directory and prepare data #########
rm(list=ls())
setwd("C:/Users/Emma/Documents/Course materials/Summer Semester/A6TransportFluxes")
BRdata <- read.csv("2019_BRdata.csv", sep = ",", header = T)
View(BRdata)
str(BRdata)

######## DATA PREPARATION AND UNIT CONVERSIONS #########
## Create a new column converting temperature values to Kelvin
BRdata$t_degK <- 273.15 + BRdata$t_degC
BRdata <- BRdata[, c(1,2,8,3,4,5,6,7)] #re-order columns

## Convert net radiation in W/m2 to MgJ/m2/day
BRdata$Rnet_Mg.Jm2.day <- (BRdata$Rnet_Wm2*24*60*60)/1000000 

## Convert hPa to kPa 
BRdata$es_KPa <- BRdata$es_hPa/10
BRdata$e_KPa <- BRdata$e_hPa/10
BRdata$p_KPa <- BRdata$p_hPa/10

## Calculate the ground heat flux ## 
# G = 0.1Rn
BRdata$QG <- 0.1 * BRdata$Rnet_Mg.Jm2.day

## Calculate the VPD
BRdata$VPD_KPa <- BRdata$es_KPa - BRdata$e_KPa

## Remove previous columns
BRdata <- BRdata[, -c(4,5,6,8)]

########### CALCULATIONS ##############

## Latent heat of vaporization ##
# According to Stull(1988), 
# LV (latent heat of vaporization) = [2.501 - 0.00237.T(degree celsius)]10^6
# Converted to MJ/Kg. FAO(2009)
BRdata$LV_MJ.Kg <- ((2.501 - (0.00237 * BRdata$t_degC)) * 10^6)/1000

## Calculate the Clausius-Clapeyron Equation ## 
# According to Stull(1988), RL: specific gas constant of dry air = 287.04J/KgK = 0.0028704MJ/KgK
BRdata$S_CC <- (0.622 * BRdata$es_KPa * BRdata$LV_MJ.Kg) / (0.0028704 * (BRdata$t_degK)^2)

## calculate the psychometric constant ## 
# Gamma = p * cp / 0.622 * LV_MJ.kg, where cp = 1005J/KgK (0.001005MgJKg.K). Stull (1988)
BRdata$gamma <- BRdata$p_KPa * 0.001005 / (0.622 * BRdata$LV_MJ.Kg)

## Calculate the stomatal resistance and aerodynamic resistance 
# Canopy resistance(r_s) = Rsi / LAIactive. FAO (2009) 
# Where, LAI = 2, Rsi = 100sm-1, LAIactive = 0.5LAI = 1 
# Aerodynamic resistance(r_a) = (log((z - d)/Z0) * log((z - d)/z0q)) / (k^2 * u(z))
# Where, z-d = 2m; d= 2/3 zB; z0 =0.123 zB; z0q = 0.1 z0; z0 = 0.02m; k = 0.41
# therefore, z0q = 0.002; zB = 0.163; d = 0.1087; z0 = 0.02m; z = 2.1087

BRdata$r_s <- 100 / 1
BRdata$r_a <- (log(2/0.02) * log(2/0.002)) / ((0.41^2) * BRdata$u_ms)

## Now that we have all parameters, we calculate the 
# hourly potential evapotranspiration according to Priestley and Taylor
# alpha = 1.25. Stull (1988)

BRdata$E_PT <- 1.25 * (((BRdata$S_CC) * (BRdata$Rnet_Mg.Jm2.day - BRdata$QG))/
                         (BRdata$S_CC + BRdata$gamma)) 

## We also calculate the actual potential evapotranspiration according to 
# the Penman Monteith Equation

BRdata$E_PM <- ((BRdata$p_KPa * 0.001005 * ((BRdata$es_KPa - BRdata$e_KPa)/BRdata$r_a)) + 
                  (BRdata$S_CC * (BRdata$Rnet_Mg.Jm2.day - BRdata$QG)))/
  (BRdata$S_CC + (BRdata$gamma * (1 + (BRdata$r_s/BRdata$r_a))))

######### PLOT THE DATA ##########

# First convert the index column to a date and time datatype in R
BRdata$Index <- as.POSIXct(BRdata$Index, tz = "", "%Y-%m-%d %H:%M:%S")
str(BRdata$Index)

# Time series plots of ET
par(mai = c(0.5,1,1,0.5)) # set graphic parameters with inner margin for plot

plot(BRdata$E_PT ~ BRdata$Index , col = 'black', type = 'l', 
     xlab = 'Date', ylab = expression('evapotranspiration(mm)'),
     # expression for accuratly labeling the y-axis
     main = 'Evapotranspiration according to PT & PM')
lines(BRdata$E_PM ~ BRdata$Index, col = 'blue', type = "l",
     xlab = 'Date', ylab = expression('evapotranspiration(mm)'))
legend("topleft", lty = 1, cex = 0.60, bty = "n", col = c("black","blue"), legend = c("Potential ETo (PT)", "Actual ETo (PM)"))


# More... 
require(ggplot2)
ggplot(BRdata, aes(Index)) +    # basic graphical object
  geom_line(aes(y=E_PT), colour="black") +  # first layer
  geom_line(aes(y=E_PM), colour="green") + 
  ylab("Evapotranspiration") + xlab("Date and Time")  # second layer

# Time Series plots of important drivers of the fluxes
plot(BRdata$Rnet_Mg.Jm2.day ~ BRdata$Index , col = 'brown4', type = 'l',
     xlab = 'Time(hrs)', ylab = expression('Energy fluxes'))
lines(BRdata$VPD_KPa ~ BRdata$Index, col = 'blue4', type = "l",
     xlab = 'Date', ylab = expression('VPD_hPa'), lty = 3)
lines(BRdata$t_degC ~ BRdata$Index, col = 'red', type = "l",
     xlab = 'Date', ylab = expression('Temp(K)'), lty = 2)
lines(BRdata$u_ms ~ BRdata$Index, col = 'chartreuse4', type = "l",
     xlab = 'Date', ylab = expression('u(ms)'), lty = 5)
legend("topleft", cex = 0.75, 
       col = c('brown4','blue4', 'red', 'chartreuse4'), lty = c(1,3,2,5), bty = "n",
       legend = c('Net radiation', 'Vapour Pressure deficit(VPD)', 'Temperature(C)', 'Windspeed(m/s)'))

write.table(BRdata, "2019BRdata_new.csv", dec=".", row.names = T, sep = " ")
