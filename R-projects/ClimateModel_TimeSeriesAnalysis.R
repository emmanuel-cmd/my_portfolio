########################################################################## ###########
# ################# INFORMATION & COMPLEXITY QUANTIFIERS ################## #
# ################# Time Series Analysis 2019/2020 ######################## #
# ################# Instructors: Holger Lange, Michael Hauhs ############## #
# ######################## Author: Emmanuel Adeleke ############# #
# Please press + 0 for easier navigation through this script. 


# ---------------------------------------------------
# Calculate statistical complexity etc. for CMIP5 data
# ---------------- GET.ALL.METRICS FUNCTION ------------------
# Note: including the q-complexity makes the function slow

get.all.metrics <- function(x) {
  # x = hydrochem[,10]
  
  # NA's in time series?
  ret.list = list()
  ret.list$NAN = which(is.na(x))
  
  if ( any( is.na(x)) ) {
    x.nona = x[-ret.list$NAN]
  } else {
    x.nona = x
  }
  
  # Standard complexity measures:
  opd = weighted_ordinal_pattern_distribution(x = x, ndemb = 6); ret.list = list()
  ret.list$PE = permutation_entropy(opd = opd)
  ret.list$MPR = MPR_complexity(opd=opd)
  ret.list$FIS = fis(opd=opd)
  ret.list$opd = opd
  # q-complexity:
 # ret.list$q = seq(0, 100, 0.01)
  #ret.list$PEq = c(sapply(X = ret.list$q, FUN=function(q1) permutation_entropy_qlog(opd = opd, q = q1)))
  #ret.list$MPRq = c(sapply(X = ret.list$q, FUN=function(q1) q_complexity(opd = opd, q = q1)))
  
  # Tarnopolski:
  ret.list$Abbe = Abbe(x.nona)
  ret.list$Turn_point = Turning_point(x)
  return(ret.list)
}

# ---------------- GET.Q.METRICS FUNCTION ------------------

get.q.metrics <- function(x) {
  # x = hydrochem[,10]
  
  # NA's in time series?
  ret.list = list()
  ret.list$NAN = which(is.na(x))
  
  if ( any( is.na(x)) ) {
    x.nona = x[-ret.list$NAN]
  } else {
    x.nona = x
  }
  
  # Standard complexity measures:
  opd = weighted_ordinal_pattern_distribution(x = x, ndemb = 6); ret.list = list()
  #ret.list$PE = permutation_entropy(opd = opd)
 #ret.list$MPR = MPR_complexity(opd=opd)
 # ret.list$FIS = fis(opd=opd)
  ret.list$opd = opd
  # q-complexity:
   ret.list$q = seq(0, 100, 0.01)
   ret.list$PEq = c(sapply(X = ret.list$q, FUN=function(q1) permutation_entropy_qlog(opd = opd, q = q1)))
   ret.list$MPRq = c(sapply(X = ret.list$q, FUN=function(q1) q_complexity(opd = opd, q = q1)))
  
  # Tarnopolski:
 # ret.list$Abbe = Abbe(x.nona)
 # ret.list$Turn_point = Turning_point(x)
  return(ret.list)
}





###################################################################
##################### MODEL: BCC_CSM, SCENARIO: RCP 26, VAR: GPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_bcc-csm1-1_rcp26_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2085, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_GPP_26 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
result_BCC_GPP_26[[1]]

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_GPP_26), nrow=length(result_BCC_GPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_GPP_26 <- colMeans(df_new)

##################### MODEL: BCC_CSM, SCENARIO: RCP 45, VAR: GPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_bcc-csm1-1_rcp45_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2085, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_GPP_45 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_GPP_45[[1]]

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_GPP_45), nrow=length(result_BCC_GPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_GPP_45 <- colMeans(df_new)
##################### MODEL: BCC_CSM, SCENARIO: RCP 85, VAR: GPP ##########################

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_bcc-csm1-1_rcp85_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2085, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_GPP_85 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_GPP_85[[1]]

#calculate q-entropy and q-complexity
opd <- ordinal_pattern_distribution(gppnew_t_1, 6)
result_BCC_GPP_85_q <- list()
result_BCC_GPP_85_q$q = seq(0, 100, 0.01)
result_BCC_GPP_85_q$PEq = c(sapply(X = result_BCC_GPP_85_q$q, FUN=function(q1) permutation_entropy_qlog(opd = opd, q = q1)))
result_BCC_GPP_85_q$MPRq = c(sapply(X = result_BCC_GPP_85_q$q, FUN=function(q1) q_complexity(opd = opd, q = q1)))

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_GPP_85), nrow=length(result_BCC_GPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#convert q-list to dataframe
df_q <- data.frame(result_BCC_GPP_85_q)

#they do not follow a gaussian distribution
hist(df_new$PE, main = 'BCC_CSM, GPP, RCP 8.5', xlab = "Permutation Entropy")
hist(df_new$MPR, main = 'BCC_CSM, GPP, RCP 8.5', xlab = "MPR Complexity")
hist(df_new$FIS, main = 'BCC_CSM, GPP, RCP 8.5', xlab = "Fisher Information")

# calculate column means
df_BCC_GPP_85 <- colMeans(df_new)

#calculate column means of q-entropy and q-complexity
df_BCC_GPP_85_q <- colMeans(df_q)
-------
  
##################### MODEL: BCC_CSM, SCENARIO: RCP 26, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_bcc-csm1-1_rcp26_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_HFLS_26 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_HFLS_26), nrow=length(result_BCC_HFLS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_HFLS_26 <- colMeans(df_new)


##################### MODEL: BCC_CSM, SCENARIO: RCP 45, VAR: HFLS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_bcc-csm1-1_rcp45_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_HFLS_45 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_HFLS_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_HFLS_45), nrow=length(result_BCC_HFLS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_HFLS_45 <- colMeans(df_new)


##################### MODEL: BCC_CSM, SCENARIO: RCP 85, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_bcc-csm1-1_rcp85_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_HFLS_85 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_HFLS_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_HFLS_85), nrow=length(result_BCC_HFLS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_HFLS_85 <- colMeans(df_new)
-------
  
-------
-------
-------
  
##################### MODEL: BCC_CSM, SCENARIO: RCP 26, VAR: NPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_bcc-csm1-1_rcp26_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2085, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_NPP_26 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_HFLS_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_NPP_26), nrow=length(result_BCC_NPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_NPP_26 <- colMeans(df_new)

##################### MODEL: BCC_CSM, SCENARIO: RCP 45, VAR: NPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_bcc-csm1-1_rcp45_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2085, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_NPP_45 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_HFLS_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_NPP_45), nrow=length(result_BCC_NPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_NPP_45 <- colMeans(df_new)

##################### MODEL: BCC_CSM, SCENARIO: RCP 85, VAR: NPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_bcc-csm1-1_rcp85_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2085, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_NPP_85 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_HFLS_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_NPP_85), nrow=length(result_BCC_NPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_NPP_85 <- colMeans(df_new)

-------
-------
--------
  
##################### MODEL: BCC_CSM, SCENARIO: RCP 26, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_bcc-csm1-1_rcp26_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_PR_26 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_PR_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_PR_26), nrow=length(result_BCC_PR_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_PR_26 <- colMeans(df_new)

##################### MODEL: BCC_CSM, SCENARIO: RCP 45, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_bcc-csm1-1_rcp45_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_PR_45 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_PR_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_PR_45), nrow=length(result_BCC_PR_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_PR_45 <- colMeans(df_new)

##################### MODEL: BCC_CSM, SCENARIO: RCP 85, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_bcc-csm1-1_rcp85_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_PR_85 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_PR_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_PR_85), nrow=length(result_BCC_PR_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_BCC_PR_85 <- colMeans(df_new)

-------
-------
-------
  
##################### MODEL: BCC_CSM, SCENARIO: RCP 26, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_bcc-csm1-1_rcp26_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_TAS_26 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_TAS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_TAS_26), nrow=length(result_BCC_TAS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they follow a gaussian distribution
hist(df_new$PE)#they follow a gaussian distribution
hist(df_new$MPR)
hist(df_new$FIS)#they follow a gaussian distribution

# calculate column means
df_BCC_TAS_26 <- colMeans(df_new)

##################### MODEL: BCC_CSM, SCENARIO: RCP 45, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_bcc-csm1-1_rcp45_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_TAS_45 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_TAS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_TAS_45), nrow=length(result_BCC_TAS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they follow a gaussian distribution
hist(df_new$PE)#they follow a gaussian distribution
hist(df_new$MPR)
hist(df_new$FIS)#they follow a gaussian distribution

# calculate column means
df_BCC_TAS_45 <- colMeans(df_new)
##################### MODEL: BCC_CSM, SCENARIO: RCP 85, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_bcc-csm1-1_rcp85_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_BCC_TAS_85 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_BCC_TAS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_BCC_TAS_85), nrow=length(result_BCC_TAS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they follow a gaussian distribution
hist(df_new$PE, main = 'BCC_CSM, TAS, RCP 8.5', xlab = "Permutation Entropy")#they follow a gaussian distribution
hist(df_new$MPR, main = 'BCC_CSM, TAS, RCP 8.5', xlab = "MPR Complexity")
hist(df_new$FIS, main = 'BCC_CSM, TAS, RCP 8.5', xlab = "Fisher Information")#they follow a gaussian distribution

# calculate column means
df_BCC_TAS_85 <- colMeans(df_new)
##################### MODEL: BCC_CSM,





######################################################################
##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 26, VAR: GPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2136, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_GPP_26 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_GPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_GPP_26), nrow=length(result_GFDL_GPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_GPP_26 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 45, VAR: GPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_GFDL-ESM2G_rcp45_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2136, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_GPP_45 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_GPP_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_GPP_45), nrow=length(result_GFDL_GPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_GPP_45 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 85, VAR: GPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_GFDL-ESM2G_rcp85_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2136, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_GPP_85 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_GPP_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_GPP_85), nrow=length(result_GFDL_GPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_GPP_85 <- colMeans(df_new)



##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 26, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_HFLS_26 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_HFLS_26), nrow=length(result_GFDL_HFLS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_HFLS_26 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 45, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_GFDL-ESM2G_rcp45_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_HFLS_45 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_HFLS_45), nrow=length(result_GFDL_HFLS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_HFLS_45 <- colMeans(df_new)
##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 85, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_GFDL-ESM2G_rcp85_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_HFLS_85 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_HFLS_85), nrow=length(result_GFDL_HFLS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_HFLS_85 <- colMeans(df_new)



##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 26, VAR: NPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2136, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_NPP_26 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_NPP_26), nrow=length(result_GFDL_NPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_NPP_26 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 45, VAR: NPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_GFDL-ESM2G_rcp45_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2136, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_NPP_45 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_NPP_45), nrow=length(result_GFDL_NPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_NPP_45 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 85, VAR: NPP ######


# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_GFDL-ESM2G_rcp85_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2136, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_NPP_85 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_NPP_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_NPP_85), nrow=length(result_GFDL_NPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_NPP_85 <- colMeans(df_new)



##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 26, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_PR_26 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_PR_26), nrow=length(result_GFDL_PR_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_PR_26 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 45, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_GFDL-ESM2G_rcp45_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_PR_45 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_PR_45), nrow=length(result_GFDL_PR_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_PR_45 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 85, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_GFDL-ESM2G_rcp85_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_PR_85 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_PR_85), nrow=length(result_GFDL_PR_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_PR_85 <- colMeans(df_new)



##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 26, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_TAS_26 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_TAS_26), nrow=length(result_GFDL_TAS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_TAS_26 <- colMeans(df_new)

##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 45, VAR: TAS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_GFDL-ESM2G_rcp45_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_TAS_45 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_TAS_45), nrow=length(result_GFDL_TAS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_GFDL_TAS_45 <- colMeans(df_new)
##################### MODEL: GFDL-ESM2G, SCENARIO: RCP 85, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_GFDL-ESM2G_rcp85_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_GFDL_TAS_85 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_TAS_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_GFDL_TAS_85), nrow=length(result_GFDL_TAS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do follow a gaussian distribution
hist(df_new$PE) #follows perfectly a gaussian distribution
hist(df_new$MPR)
hist(df_new$FIS)#follows perfectly a gaussian distribution

# calculate column means
df_GFDL_TAS_85 <- colMeans(df_new)


###################################################################
##################### MODEL: MPI-ESM, SCENARIO: RCP 26, VAR: GPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_MPI-ESM-LR_rcp26_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 1897, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_GPP_26 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_TAS_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_GPP_26), nrow=length(result_MPI_GPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE) #they do not follow a gaussian distribution
hist(df_new$MPR) #they do not follow a gaussian distribution
hist(df_new$FIS)#follows perfectly a gaussian distribution

# calculate column means
df_MPI_GPP_26 <- colMeans(df_new)
##################### MODEL: MPI-ESM, SCENARIO: RCP 45, VAR: GPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_MPI-ESM-LR_rcp45_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 1897, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_GPP_45 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_TAS_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_GPP_45), nrow=length(result_MPI_GPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE) #they do not follow a gaussian distribution
hist(df_new$MPR) #they do not follow a gaussian distribution
hist(df_new$FIS)#follows perfectly a gaussian distribution

# calculate column means
df_MPI_GPP_45 <- colMeans(df_new)
##################### MODEL: MPI-ESM, SCENARIO: RCP 85, VAR: GPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 1897, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_GPP_85 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_TAS_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_GPP_85), nrow=length(result_MPI_GPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE) #they do not follow a gaussian distribution
hist(df_new$MPR) #they do not follow a gaussian distribution
hist(df_new$FIS)#follows perfectly a gaussian distribution

# calculate column means
df_MPI_GPP_85 <- colMeans(df_new)



##################### MODEL: MPI-ESM, SCENARIO: RCP 26, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_MPI-ESM-LR_rcp26_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_HFLS_26 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_HFLS_26), nrow=length(result_MPI_HFLS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_HFLS_26 <- colMeans(df_new)

##################### MODEL: MPI-ESM, SCENARIO: RCP 45, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_MPI-ESM-LR_rcp45_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_HFLS_45 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_HFLS_45), nrow=length(result_MPI_HFLS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_HFLS_45 <- colMeans(df_new)
##################### MODEL: MPI-ESM, SCENARIO: RCP 85, VAR: HFLS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_HFLS_85 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_HFLS_85), nrow=length(result_MPI_HFLS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_HFLS_85 <- colMeans(df_new)


##################### MODEL: MPI-ESM, SCENARIO: RCP 26, VAR: NPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_MPI-ESM-LR_rcp26_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 1897, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_NPP_26 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_NPP_26), nrow=length(result_MPI_NPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_NPP_26 <- colMeans(df_new)

##################### MODEL: MPI-ESM, SCENARIO: RCP 45, VAR: NPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_MPI-ESM-LR_rcp45_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 1897, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_NPP_45 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_NPP_45), nrow=length(result_MPI_NPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_NPP_45 <- colMeans(df_new)

##################### MODEL: MPI-ESM, SCENARIO: RCP 85, VAR: NPP ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 1897, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_NPP_85 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_NPP_85), nrow=length(result_MPI_NPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_NPP_85 <- colMeans(df_new)



##################### MODEL: MPI-ESM, SCENARIO: RCP 26, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_MPI-ESM-LR_rcp26_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_PR_26 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_PR_26), nrow=length(result_MPI_PR_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_PR_26 <- colMeans(df_new)

##################### MODEL: MPI-ESM, SCENARIO: RCP 45, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_MPI-ESM-LR_rcp45_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_PR_45 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_PR_45), nrow=length(result_MPI_PR_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_PR_45 <- colMeans(df_new)

##################### MODEL: MPI-ESM, SCENARIO: RCP 85, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_PR_85 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_MPI_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_PR_85), nrow=length(result_MPI_PR_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_PR_85 <- colMeans(df_new)


##################### MODEL: MPI-ESM, SCENARIO: RCP 26, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_MPI-ESM-LR_rcp26_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_TAS_26 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_TAS_26), nrow=length(result_MPI_TAS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_TAS_26 <- colMeans(df_new)

##################### MODEL: MPI-ESM, SCENARIO: RCP 45, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_MPI-ESM-LR_rcp45_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_TAS_45 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_TAS_45), nrow=length(result_MPI_TAS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_TAS_45 <- colMeans(df_new)

##################### MODEL: MPI-ESM, SCENARIO: RCP 85, VAR: TAS ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_MPI_TAS_85 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_GFDL_PR_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_MPI_TAS_85), nrow=length(result_MPI_TAS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_MPI_TAS_85 <- colMeans(df_new)





##################################################################
##################### MODEL: NorESM1, SCENARIO: RCP 26, VAR: GPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_NorESM1-M_rcp26_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2137, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_GPP_26 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_GPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_GPP_26), nrow=length(result_NorESM1_GPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_NorESM1_GPP_26 <- colMeans(df_new)

##################### MODEL: NorESM1, SCENARIO: RCP 45, VAR: GPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_NorESM1-M_rcp45_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2137, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_GPP_45 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_GPP_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_GPP_45), nrow=length(result_NorESM1_GPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS)

# calculate column means
df_NorESM1_GPP_45 <- colMeans(df_new)
##################### MODEL: NorESM1, SCENARIO: RCP 85, VAR: GPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_NorESM1-M_rcp85_r1i1p1_g025.nc", var="gpp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(gppvalues)
length(nas)
real <- gppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
gppnew <- matrix(real, nrow = 2137, ncol = 2772)
gppnew

gppnew_t <- t(gppnew)

# remove 0 value
gppnew_t_1 <- gppnew_t[, colSums(gppnew_t != 0) > 0]
length(gppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_GPP_85 <- apply(X = gppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_GPP_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_GPP_85), nrow=length(result_NorESM1_GPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE, main = 'NorESM1, GPP, RCP 8.5', xlab = "Permutation Entropy")
hist(df_new$MPR, main = 'NorESM1, GPP, RCP 8.5', xlab = "MPR Complexity")
hist(df_new$FIS, main = 'NorESM1, GPP, RCP 8.5', xlab = "Fisher Information")

# calculate column means
df_NorESM1_GPP_85 <- colMeans(df_new)


#################

##################### MODEL: NorESM1, SCENARIO: RCP 26, VAR: HFLS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_NorESM1-M_rcp26_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_HFLS_26 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_HFLS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_HFLS_26), nrow=length(result_NorESM1_HFLS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) #follows a gaussian distribution

# calculate column means
df_NorESM1_HFLS_26 <- colMeans(df_new)

##################### MODEL: NorESM1, SCENARIO: RCP 45, VAR: HFLS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_NorESM1-M_rcp45_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_HFLS_45 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_HFLS_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_HFLS_45), nrow=length(result_NorESM1_HFLS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) 

# calculate column means
df_NorESM1_HFLS_45 <- colMeans(df_new)

##################### MODEL: NorESM1, SCENARIO: RCP 85, VAR: HFLS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_NorESM1-M_rcp85_r1i1p1_g025.nc", var="hfls")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(hflsvalues)
length(nas)
real <- hflsvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
hflsnew <- matrix(real, nrow = 10368, ncol = 2772)
hflsnew

hflsnew_t <- t(hflsnew)

# remove 0 value
hflsnew_t_1 <- hflsnew_t[, colSums(hflsnew_t != 0) > 0]
length(hflsnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_HFLS_85 <- apply(X = hflsnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_HFLS_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_HFLS_85), nrow=length(result_NorESM1_HFLS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) #follows a gaussian distribution

# calculate column means
df_NorESM1_HFLS_85 <- colMeans(df_new)



#####################

##################### MODEL: NorESM1, SCENARIO: RCP 26, VAR: NPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_NorESM1-M_rcp26_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2137, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_NPP_26 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_NPP_26), nrow=length(result_NorESM1_NPP_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) 

# calculate column means
df_NorESM1_NPP_26 <- colMeans(df_new)
##################### MODEL: NorESM1, SCENARIO: RCP 45, VAR: NPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_NorESM1-M_rcp45_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2137, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_NPP_45 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_NPP_45

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_NPP_45), nrow=length(result_NorESM1_NPP_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) 

# calculate column means
df_NorESM1_NPP_45 <- colMeans(df_new)
##################### MODEL: NorESM1, SCENARIO: RCP 85, VAR: NPP ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_NorESM1-M_rcp85_r1i1p1_g025.nc", var="npp")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(nppvalues)
length(nas)
real <- nppvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
nppnew <- matrix(real, nrow = 2137, ncol = 2772)
nppnew

nppnew_t <- t(nppnew)

# remove 0 value
nppnew_t_1 <- nppnew_t[, colSums(nppnew_t != 0) > 0]
length(nppnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_NPP_85 <- apply(X = nppnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_NPP_85

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_NPP_85), nrow=length(result_NorESM1_NPP_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE, main = 'NorESM1, NPP, RCP 8.5', xlab = "Permutation Entropy")
hist(df_new$MPR, main = 'NorESM1, NPP, RCP 8.5', xlab = "MPR Complexity")
hist(df_new$FIS, main = 'NorESM1, NPP, RCP 8.5', xlab = "Fisher Information") 

# calculate column means
df_NorESM1_NPP_85 <- colMeans(df_new)


#####################

##################### MODEL: NorESM1, SCENARIO: RCP 26, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_NorESM1-M_rcp26_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_PR_26 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_PR_26), nrow=length(result_NorESM1_PR_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) 

# calculate column means
df_NorESM1_PR_26 <- colMeans(df_new)

##################### MODEL: NorESM1, SCENARIO: RCP 45, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_NorESM1-M_rcp45_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_PR_45 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_PR_45), nrow=length(result_NorESM1_PR_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) 

# calculate column means
df_NorESM1_PR_45 <- colMeans(df_new)
##################### MODEL: NorESM1, SCENARIO: RCP 85, VAR: PR ######

# Example call

setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_NorESM1-M_rcp85_r1i1p1_g025.nc", var="pr")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(prvalues)
length(nas)
real <- prvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
prnew <- matrix(real, nrow = 10368, ncol = 2772)
prnew

prnew_t <- t(prnew)

# remove 0 value
prnew_t_1 <- prnew_t[, colSums(prnew_t != 0) > 0]
length(prnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_PR_85 <- apply(X = prnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_NPP_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_PR_85), nrow=length(result_NorESM1_PR_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE)
hist(df_new$MPR)
hist(df_new$FIS) 

# calculate column means
df_NorESM1_PR_85 <- colMeans(df_new)

########################

##################### MODEL: NorESM1, SCENARIO: RCP 26, VAR: TAS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_NorESM1-M_rcp26_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_TAS_26 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_TAS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_TAS_26), nrow=length(result_NorESM1_TAS_26), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE) #follows a gaussian distribution
hist(df_new$MPR)
hist(df_new$FIS) #follows a gaussian distribution

# calculate column means
df_NorESM1_TAS_26 <- colMeans(df_new)
##################### MODEL: NorESM1, SCENARIO: RCP 45, VAR: TAS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_NorESM1-M_rcp45_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_TAS_45 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_TAS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_TAS_45), nrow=length(result_NorESM1_TAS_45), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE) 
hist(df_new$MPR)
hist(df_new$FIS) #follows a gaussian distribution

# calculate column means
df_NorESM1_TAS_45 <- colMeans(df_new)
##################### MODEL: NorESM1, SCENARIO: RCP 85, VAR: TAS ######
# Example call

setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_NorESM1-M_rcp85_r1i1p1_g025.nc", var="tas")

#testts=gppvalues[5,56,]
#analys=get.all.metrics(testts)

#remove NAs 
nas <- is.na(tasvalues)
length(nas)
real <- tasvalues[!nas]
length(real)

length(real)/2772

#convert to a matrix and transpose
tasnew <- matrix(real, nrow = 10368, ncol = 2772)
tasnew

tasnew_t <- t(tasnew)

# remove 0 value
tasnew_t_1 <- tasnew_t[, colSums(tasnew_t != 0) > 0]
length(tasnew_t_1)

#Check whether this works, not clear:
#cmip5.ALLMETRICS = apply(X = gppnew_t, MARGIN = 2, FUN=get.all.metrics)
result_NorESM1_TAS_85 <- apply(X = tasnew_t_1, MARGIN = 2, FUN = get.all.metrics)
#result_NorESM1_TAS_26

#convert list to dataframe 
df <- data.frame(matrix(unlist(result_NorESM1_TAS_85), nrow=length(result_NorESM1_TAS_85), byrow=T))
#ncol(df)
df_new <- df[, c(1:3, 724, 725)]
colnames(df_new) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

#they do not follow a gaussian distribution
hist(df_new$PE) #follows a gaussian distribution
hist(df_new$MPR)
hist(df_new$FIS) #follows a gaussian distribution

# calculate column means
df_NorESM1_TAS_85 <- colMeans(df_new)






############# CREATE DATAFRAME OF ALL VALUES 
DF_total <- data.frame(cbind(rbind("df_BCC_GPP_26", "df_BCC_GPP_45", "df_BCC_GPP_85", "df_BCC_HFLS_26", "df_BCC_HFLS_45", "df_BCC_HFLS_85",
            "df_BCC_NPP_26", "df_BCC_NPP_45", "df_BCC_NPP_85", "df_BCC_PR_26", "df_BCC_PR_45", "df_BCC_PR_85", "df_BCC_TAS_26", 
            "df_BCC_TAS_45", "df_BCC_TAS_85", "df_GFDL_GPP_26", "df_GFDL_GPP_45", "df_GFDL_GPP_85", "df_GFDL_HFLS_26", 
            "df_GFDL_HFLS_45", "df_GFDL_HFLS_85", "df_GFDL_NPP_26", "df_GFDL_NPP_45", "df_GFDL_NPP_85", "df_GFDL_PR_26", 
            "df_GFDL_PR_45", "df_GFDL_PR_85", "df_GFDL_TAS_26", "df_GFDL_TAS_45", "df_GFDL_TAS_85", "df_MPI_GPP_26", 
            "df_MPI_GPP_45", "df_MPI_GPP_85", "df_MPI_HFLS_26", "df_MPI_HFLS_45", "df_MPI_HFLS_85", "df_MPI_NPP_26", 
            "df_MPI_NPP_45", "df_MPI_NPP_85", "df_MPI_PR_26", "df_MPI_PR_45", "df_MPI_PR_85", "df_MPI_TAS_26", "df_MPI_TAS_45",
            "df_MPI_TAS_85", "df_NorESM1_GPP_26", "df_NorESM1_GPP_45", "df_NorESM1_GPP_85", "df_NorESM1_HFLS_26", "df_NorESM1_HFLS_45", 
            "df_NorESM1_HFLS_85", "df_NorESM1_NPP_26", "df_NorESM1_NPP_45", "df_NorESM1_NPP_85", "df_NorESM1_PR_26", 
            "df_NorESM1_PR_45", "df_NorESM1_PR_85", "df_NorESM1_TAS_26", "df_NorESM1_TAS_45", "df_NorESM1_TAS_85"), 
      rbind(df_BCC_GPP_26, df_BCC_GPP_45, df_BCC_GPP_85, df_BCC_HFLS_26, df_BCC_HFLS_45, df_BCC_HFLS_85,
      df_BCC_NPP_26, df_BCC_NPP_45, df_BCC_NPP_85, df_BCC_PR_26, df_BCC_PR_45, df_BCC_PR_85, df_BCC_TAS_26, 
      df_BCC_TAS_45, df_BCC_TAS_85, df_GFDL_GPP_26, df_GFDL_GPP_45, df_GFDL_GPP_85, df_GFDL_HFLS_26, 
      df_GFDL_HFLS_45, df_GFDL_HFLS_85, df_GFDL_NPP_26, df_GFDL_NPP_45, df_GFDL_NPP_85, df_GFDL_PR_26, 
      df_GFDL_PR_45, df_GFDL_PR_85, df_GFDL_TAS_26, df_GFDL_TAS_45, df_GFDL_TAS_85, df_MPI_GPP_26, 
      df_MPI_GPP_45, df_MPI_GPP_85, df_MPI_HFLS_26, df_MPI_HFLS_45, df_MPI_HFLS_85, df_MPI_NPP_26, 
      df_MPI_NPP_45, df_MPI_NPP_85, df_MPI_PR_26, df_MPI_PR_45, df_MPI_PR_85, df_MPI_TAS_26, df_MPI_TAS_45,
      df_MPI_TAS_85, df_NorESM1_GPP_26, df_NorESM1_GPP_45, df_NorESM1_GPP_85, df_NorESM1_HFLS_26, df_NorESM1_HFLS_45, 
      df_NorESM1_HFLS_85, df_NorESM1_NPP_26, df_NorESM1_NPP_45, df_NorESM1_NPP_85, df_NorESM1_PR_26, 
      df_NorESM1_PR_45, df_NorESM1_PR_85, df_NorESM1_TAS_26, df_NorESM1_TAS_45, df_NorESM1_TAS_85)))
      

DF_total$V1 <- as.character(DF_total$V1)
DF_total$PE <- as.numeric(as.character(DF_total$PE))
DF_total$MPR <- as.numeric(as.character(DF_total$MPR))
DF_total$FIS <- as.numeric(as.character(DF_total$FIS))
DF_total$Abbe <- as.numeric(as.character(DF_total$Abbe))
DF_total$Turn_point <- as.numeric(as.character(DF_total$Turn_point))
row.names(DF_total) <- 1:60

## separate column V1
newc <- str_split_fixed(DF_total$V1, "_", 4)
newc <- as.data.frame(newc)

# merge new columns to DF_total
DF_total <- DF_total[,-1]
newc <- newc[,-1]
DF_total <- cbind(newc, DF_total)

#rename columns
names(DF_total) <- c("Model", "Variables", "Scenarios", "PE", "MPR", "FIS", "Abbe", "Turn_point")

########################################## ENTROPY-COMPLEXITY PLANE ######################################
#Entropy-Complexity plane of variables and models
scen6 <- subset(DF_total, subset = DF_total$Scenarios == '26')
scen6$colour <- c(rep('darkgreen', 5), rep('dodgerblue4', 5), rep('firebrick', 5), rep('darkorchid4', 5))
plot(scen6$PE, scen6$MPR, ylab = "MPR Complexity", xlab = "Weighted permutation Entropy", type = "n", xlim = c(0.4,1), ylim = c(0, 0.5))
points(scen6$PE, scen6$MPR, pch = c(15,11,17,18,8), col = scen6$colour, xlim = c(0.2,1))
legend("topright", pch = c(15,11,17,23,8), legend = c('GPP', 'HFLS', 'NPP', 'PR', 'TAS'))
legend("bottomleft", col = unique(scen6$colour), legend = c('BCC-CSM 1.1', 'GFDL-ESM 2G', 'MPI-ESM-LR', 'NorESM1-M'), pch = 19, bty = "o")

# Add limit curves:
min_curve <-  limit_curves(6, fun = "min")
max_curve <- readRDS(file="max_curve.rds")

lines(min_curve[[1]], min_curve[[2]])
lines(max_curve[[1]], max_curve[[2]])

##Entropy-Complexity plane of all models
DF_total$colour = c(rep('darkgreen', 15), rep('dodgerblue4', 15), rep('firebrick',15), rep('darkorchid4', 15))
plot(DF_total$PE, DF_total$MPR, ylab = "MPR Complexity", xlab = "Permutation Entropy", col = DF_total$colour, pch = 20, type = "p" , xlim = c(0.4,1), ylim = c(0,0.6))
?points
lines(min_curve[[1]], min_curve[[2]])
lines(max_curve[[1]], max_curve[[2]])
legend("topright",  col = unique(DF_total$colour), legend = c('BCC-CSM 1.1', 'GFDL-ESM 2G', 'MPI-ESM-LR', 'NorESM1-M'), pch = 19, bty = "o")

##Entropy-Complexity plane of models and scenarios of precipitation
PR_all <- subset(DF_total, DF_total$Variables == "PR")

plot(PR_all$PE, PR_all$MPR, ylab = "MPR Complexity", xlab = "Permutation Entropy", type = "n", col = PR_all$colour, ylim = c(0,0.3), xlim = c(0.870, 1))
points(PR_all$PE, PR_all$MPR, pch = c(8,15,17), col = PR_all$colour)
legend("topright", pch = c(8,15,17), legend = c('RCP 2.6', 'RCP 4.5', 'RCP 8.5'))
legend("bottomleft", col = unique(PR_all$colour), legend = c('BCC-CSM 1.1', 'GFDL-ESM 2G', 'MPI-ESM-LR', 'NorESM1-M'), pch = 19, bty = "o")
lines(min_curve[[1]], min_curve[[2]])
lines(max_curve[[1]], max_curve[[2]])

#Entropy-Complexity plane of variables and scenarios of the GFDL model
GFDL_all <- subset(DF_total, DF_total$Model == "GFDL")
GFDL_all <- GFDL_all[,-9]
GFDL_all$colour <- c(rep('darkgreen', 3), rep('firebrick', 3), rep('darkorchid4', 3), rep('cornflowerblue', 3), rep('cyan', 3))

plot(GFDL_all$PE, GFDL_all$MPR, ylab = "MPR Complexity", xlab = "Permutation Entropy", type = "n", xlim = c(0.4, 1), ylim = c(0, 0.55))
points(GFDL_all$PE, GFDL_all$MPR, col = GFDL_all$colour, pch = c(8,15,17))
legend("topright", pch = c(8,15,17), legend = c('RCP 2.6', 'RCP 4.5', 'RCP 8.5'))
legend("bottomleft", col = unique(GFDL_all$colour), legend = c('GPP', 'HFLS', 'NPP', 'PR', 'TAS'), pch = 19, bty = "o")
lines(min_curve[[1]], min_curve[[2]])
lines(max_curve[[1]], max_curve[[2]])

########################################## ENTROPY-FISHER PLANE #####################################
plot(DF_total$PE, DF_total$FIS, ylab = "Fisher Information", xlab = "Permutation Entropy", type = "p", pch = 20, col = DF_total$colour, main = "Entropy-Fisher plane")
legend("topright", col = unique(DF_total$colour), legend = c('BCC-CSM 1.1', 'GFDL-ESM 2G', 'MPI-ESM-LR', 'NorESM1-M'), pch = 19, bty = "o")

plot(DF_total$PE, DF_total$FIS, ylab = "Fisher Information", xlab = "Permutation Entropy", type = "p", pch = c(15,11,17,23,8), col = DF_total$colour, main = "Entropy-Fisher plane")
legend("topright", col = unique(DF_total$colour), legend = c('BCC-CSM 1.1', 'GFDL-ESM 2G', 'MPI-ESM-LR', 'NorESM1-M'), pch = 19, bty = "o")
legend("bottomleft", legend = c('GPP', 'HFLS', 'NPP', 'PR', 'TAS'), pch = c(15,11,17,23,8), bty = "o")

############################## ENTROPY-COMPLEXITY PLANE FOR LOW, MEDIAN AND HIGH PIXELS ########
### Q1: Find pixels close to the median of the H,C, and F, and extreme values(low/high):
###  Where are they? Plot the time series for these pixels

### MODEL: GFDL-ESM2G, SCENARIO: RCP 26, VAR: GPP 

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues_1=get_CMIP5_array("gpp_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc", var="gpp")

gpp_1 <- nc_open("gpp_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc")

#set time
tid_1 <- gpp_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=gpp_1$dim$lat$vals
longi=gpp_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(gppvalues_1,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 
# median GPP: row 1069, latitude/longitude: (23.75, 66.25), 10 X 63

View(gppsort) #highest GPP: latitude/longitude: (23.75, 1.25), 10 X 37
#lowest GPP: latitude/longitude: (26.25, 23.75), 11 X 46

gpp_gfdl_metrics1 <- get.all.metrics(gppvalues_1[11,46, ]) # lowest
gpp_gfdl_metrics2 <- get.all.metrics(gppvalues_1[10,63, ]) # median
gpp_gfdl_metrics3 <- get.all.metrics(gppvalues_1[10,37, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

df_min <- data.frame(matrix(unlist(gpp_gfdl_metrics1), nrow=1, byrow=T))
df_min <- df_min[,c(1:3, 724, 725)]
colnames(df_min) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

df_med <- data.frame(matrix(unlist(gpp_gfdl_metrics2), nrow=1, byrow=T))
df_med <- df_med[,c(1:3, 724, 725)]
colnames(df_med) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

df_max <- data.frame(matrix(unlist(gpp_gfdl_metrics3), nrow=1, byrow=T))
df_max <- df_max[,c(1:3, 724, 725)]
colnames(df_max) <- c('PE', 'MPR', 'FIS', 'Abbe', 'Turn_point')

df_all <- rbind(df_min, df_med, df_max)
df_all$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")

#Plot on Entropy-complexity plane
plot(df_all$PE, df_all$MPR, ylab = "MPR Complexity", xlab = "Permutation Entropy", type = "n", xlim = c(0,1), ylim = c(0, 0.5))
points(df_all$PE, df_all$MPR, pch = 19, col = c('black', 'green', 'red'), xlim = c(0,1))
legend("topleft", pch = 19, legend = c('Lowest GPP(Russia)', 'Median GPP(Arabian Sea)', 'Highest GPP(Algeria)'), col = c('black', 'green', 'red'))
title("GFDL-ESM2G RCP2.6: Entropy-Complexity Plane",mgp=c(2,1,1))
lines(min_curve[[1]], min_curve[[2]])
lines(max_curve[[1]], max_curve[[2]])

#Plot on Entropy-Fisher plane
plot(df_all$PE, df_all$FIS, ylab = "Fisher Information", xlab = "Permutation Entropy", type = "n") #xlim = c(0,1), ylim = c(0, 0.5))
points(df_all$PE, df_all$FIS, pch = 19, col = c('black', 'green', 'red'))
legend("bottomleft", pch = 19, legend = c('Lowest GPP(Russia)', 'Median GPP(Arabian Sea)', 'Highest GPP(Algeria)'), col = c('black', 'green', 'red'))
title("GFDL-ESM2G RCP2.6: Entropy-Fisher Plane",mgp=c(2,1,1))


#Plot time series of lowest, median and highest GPP pixels
timeaxis=as.Date.numeric(tid_1,origin="1800-01-01")

plot(timeaxis[1600:2772],31536000*10000*gppvalues_1[11,46, ][1600:2772]/1000,type="l",xlab="",ylab="")
title("GFDL-ESM2G RCP2.6: Time series for the lowest GPP mean value",xlab="Year",ylab=expression(t~C ~ ha^-1*yr^-1),mgp=c(2,1,1))

plot(timeaxis[1600:2772],31536000*10000*gppvalues_1[10,63, ][1600:2772]/1000,type="l",xlab="",ylab="")
title("GFDL-ESM2G RCP2.6: Time series for the median GPP mean value",xlab="Year",ylab=expression(t~C ~ ha^-1*yr^-1),mgp=c(2,1,1))

plot(timeaxis[1600:2772],31536000*10000*gppvalues_1[10,37, ][1600:2772]/1000,type="l",xlab="",ylab="")
title("GFDL-ESM2G RCP2.6: Time series for the highest GPP mean value",xlab="Year",ylab=expression(t~C ~ ha^-1*yr^-1),mgp=c(2,1,1))

############################## TARNOPOLSKI DIAGRAMS #######################################
sine_wave <- sin(seq(0,2772,0.1))
t <- seq(0,2772,0.1)
fGN<- simFGN0(2772, 0.7)
fBM<- fbm(hurst = 0.7, n=2772)

#plot(fGN)
#plot(fBM)
#plot(sine_wave ~ t, type = "l", xlim=c(0,50))

sw_est<- get.all.metrics(sine_wave)
fGN_est<- get.all.metrics(fGN)
fBM_est<- get.all.metrics(fBM)

#convert list to dataframe 
sw_df <- data.frame(sw_est)
sw_df <- colMeans(sw_df)
fgn_df <- data.frame(fGN_est)
fgn_df <- colMeans(fgn_df)
fbm_df <- data.frame(fBM_est)
fbm_df <- colMeans(fbm_df)

noise_df <- as.data.frame(rbind(sw_df, fgn_df, fbm_df))
noise_df$Model <- c('SW', 'fGN', 'fBM')
row.names(noise_df) <- 1:3

new_d <- subset(DF_total, Variables == 'GPP' & DF_total$Scenarios == '85')
new_df <- merge(new_d, noise_df, all = T)

#plot
plot(new_df$Abbe, new_df$Turn_point, type = "n", xlab= "Abbe value", ylab = 'Turning point', main = "Tarnopolski diagram")
points(new_df$Abbe, new_df$Turn_point, pch = 17, col = c('dodgerblue4','lightgreen','darkorchid4','cyan','black','darkgreen', 'firebrick'))
text(new_df$Abbe, new_df$Turn_point,labels = new_df$Model, pos = 4)
legend('bottomright', pch = 17, legend = c('BCC-CSM 1.1', 'GFDL-ESM 2G', 'MPI-ESM-LR', 'NorESM1-M', 'Sine wave', 'White noise', 'Red noise'), col = c('dodgerblue4','lightgreen','darkorchid4','cyan','black','darkgreen', 'firebrick'))
################################### CALCULATE Q-ENTROPY & Q-COMPLEXITY ####################### 
#### For selected models,variables and scenarios,
#### calculate the q-entropy and q-complexity for high, median and low GPP pixels

####################################################### qH-qC plane for all models #########
### RCP 2.6, GPP
### Taking the BCC model

# Example call

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_bcc-csm1-1_rcp26_r1i1p1_g025.nc", var="gpp")
gpp_1=nc_open("gpp_mon_bcc-csm1-1_rcp26_r1i1p1_g025.nc")

#set time
tid_1 <- gpp_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=gpp_1$dim$lat$vals
longi=gpp_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(gppvalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 
# median GPP: row 1043, latitude/longitude: (128.75, 48.75), 52 X 56

View(gppsort) #highest GPP: latitude/longitude: (11.25, 1.25), 5 X 37
#lowest GPP: latitude/longitude: (365.25, 23.75), 143 X 46

bcc_gpp_qmetrics1 <- get.q.metrics(gppvalues[143,46, ]) # lowest
bcc_gpp_qmetrics2 <- get.q.metrics(gppvalues[52,56, ]) # median
bcc_gpp_qmetrics3 <- get.q.metrics(gppvalues[5,37, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

BCC_min <- data.frame(matrix(unlist(bcc_gpp_qmetrics1), ncol=3, byrow=F))
colnames(BCC_min) <- c('q', 'PEq', 'MPRq')
BCC_min <- colMeans(BCC_min)

BCC_med <- data.frame(matrix(unlist(bcc_gpp_qmetrics2), ncol=3, byrow=F))
colnames(BCC_med) <- c('q', 'PEq', 'MPRq')
BCC_med <- colMeans(BCC_med)

BCC_max <- data.frame(matrix(unlist(bcc_gpp_qmetrics3), ncol=3, byrow=F))
colnames(BCC_max) <- c('q', 'PEq', 'MPRq')
BCC_max <- colMeans(BCC_max)

df_all_bcc <- as.data.frame(rbind(BCC_min, BCC_med, BCC_max))
df_all_bcc$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")

### Taking the GFDL model

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gfdlvalues=get_CMIP5_array("gpp_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc", var="gpp")
gfdl_1 <- nc_open("gpp_mon_GFDL-ESM2G_rcp26_r1i1p1_g025.nc")

#set time
tid_1 <- gfdl_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=gfdl_1$dim$lat$vals
longi=gfdl_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(gfdlvalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 
# median GPP: row 1069, latitude/longitude: (23.75, 66.25), 10 X 63

View(gppsort) #highest GPP: latitude/longitude: (23.75, 1.25), 10 X 37
#lowest GPP: latitude/longitude: (26.25, 23.75), 11 X 46

gfdl_gpp_qmetrics1 <- get.q.metrics(gfdlvalues[11,46, ]) # lowest
gfdl_gpp_qmetrics2 <- get.q.metrics(gfdlvalues[10,63, ]) # median
gfdl_gpp_qmetrics3 <- get.q.metrics(gfdlvalues[10,37, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

GFDL_min <- data.frame(matrix(unlist(gfdl_gpp_qmetrics1), ncol=3, byrow=F))
colnames(GFDL_min) <- c('q', 'PEq', 'MPRq')
GFDL_min <- colMeans(GFDL_min)

GFDL_med <- data.frame(matrix(unlist(gfdl_gpp_qmetrics2), ncol=3, byrow=F))
colnames(GFDL_med) <- c('q', 'PEq', 'MPRq')
GFDL_med <- colMeans(GFDL_med)

GFDL_max <- data.frame(matrix(unlist(gfdl_gpp_qmetrics3), ncol=3, byrow=F))
colnames(GFDL_max) <- c('q', 'PEq', 'MPRq')
GFDL_max <- colMeans(GFDL_max)

df_all_gfdl <- as.data.frame(rbind(GFDL_min, GFDL_med, GFDL_max))
df_all_gfdl$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")

### Taking the MPI-ESM model

setwd("D:/TimeSeriesAnalysis/Data/gpp/")
mpivalues=get_CMIP5_array("gpp_mon_MPI-ESM-LR_rcp26_r1i1p1_g025.nc", var="gpp")
mpi_1 <- nc_open("gpp_mon_MPI-ESM-LR_rcp26_r1i1p1_g025.nc")

#set time
tid_1 <- mpi_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=mpi_1$dim$lat$vals
longi=mpi_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(mpivalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 
# median GPP: row 949, latitude/longitude: (63.75, 58.75), 26 X 60

View(gppsort) #highest GPP: latitude/longitude: (116.25, -1.25), 47 X 36
#lowest GPP: latitude/longitude: (33.75, 26.25), 14 X 47

mpi_gpp_qmetrics1 <- get.q.metrics(mpivalues[14,47, ]) # lowest
mpi_gpp_qmetrics2 <- get.q.metrics(mpivalues[26,60, ]) # median
mpi_gpp_qmetrics3 <- get.q.metrics(mpivalues[47,36, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

MPI_min <- data.frame(matrix(unlist(mpi_gpp_qmetrics1), ncol=3, byrow=F))
colnames(MPI_min) <- c('q', 'PEq', 'MPRq')
MPI_min <- colMeans(MPI_min)

MPI_med <- data.frame(matrix(unlist(mpi_gpp_qmetrics2), ncol=3, byrow=F))
colnames(MPI_med) <- c('q', 'PEq', 'MPRq')
MPI_med <- colMeans(MPI_med)

MPI_max <- data.frame(matrix(unlist(mpi_gpp_qmetrics3), ncol=3, byrow=F))
colnames(MPI_max) <- c('q', 'PEq', 'MPRq')
MPI_max <- colMeans(MPI_max)

df_all_mpi <- as.data.frame(rbind(MPI_min, MPI_med, MPI_max))
df_all_mpi$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")

### Taking the NOR model
setwd("D:/TimeSeriesAnalysis/Data/gpp/")
norvalues=get_CMIP5_array("gpp_mon_NorESM1-M_rcp26_r1i1p1_g025.nc", var="gpp")
nor_1 <- nc_open("gpp_mon_NorESM1-M_rcp26_r1i1p1_g025.nc")

#set time
tid_1 <- nor_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=nor_1$dim$lat$vals
longi=nor_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(norvalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 
# median GPP: row 1069, latitude/longitude: (88.75, 31.25), 36 X 49

View(gppsort) #highest GPP: latitude/longitude: (23.75, 1.25), 10 X 37
#lowest GPP: latitude/longitude: (358.75, 21.25), 144 X 45

nor_gpp_qmetrics1 <- get.q.metrics(mpivalues[144,45, ]) # lowest
nor_gpp_qmetrics2 <- get.q.metrics(mpivalues[10,37, ]) # median
nor_gpp_qmetrics3 <- get.q.metrics(mpivalues[36,49, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

NOR_min <- data.frame(matrix(unlist(nor_gpp_qmetrics1), ncol=3, byrow=F))
colnames(NOR_min) <- c('q', 'PEq', 'MPRq')
NOR_min <- colMeans(NOR_min)

NOR_med <- data.frame(matrix(unlist(nor_gpp_qmetrics2), ncol=3, byrow=F))
colnames(NOR_med) <- c('q', 'PEq', 'MPRq')
NOR_med <- colMeans(NOR_med)

NOR_max <- data.frame(matrix(unlist(nor_gpp_qmetrics3), ncol=3, byrow=F))
colnames(NOR_max) <- c('q', 'PEq', 'MPRq')
NOR_max <- colMeans(NOR_max)

df_all_nor <- as.data.frame(rbind(NOR_min, NOR_med, NOR_max))
df_all_nor$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")

##merge dataframes, prepare dataframe for plotting
df_all_models <- rbind(df_all_bcc, df_all_gfdl, df_all_mpi, df_all_nor)
df_all_models$models <- c(rep("BCC", 3), rep("GFDL", 3), rep("MPI", 3), rep("NOR", 3))
df_all_models$colours <-  rep(c('darkorchid4','darkgreen','firebrick') , 4)
df_all_models$symbols <- c(rep(0, 3), rep(2, 3), rep(1, 3), rep(4, 3))
row.names(df_all_models) <- 1:12

range01 <- function(x){(x-min(x))/(max(x)-min(x))} # function scales values between 0 and 1
?points
df_all_models$PEq <- range01(df_all_models$PEq)

plot(df_all_models$PEq, df_all_models$MPRq, pch = df_all_models$symbols, 
     xlab = "q-entropy", ylab = "q-complexity", col = df_all_models$colours, cex = 1.5, lwd = 2)
legend(0,0.7, pch = 19, col = unique(df_all_models$colours), legend = c('Lowest GPP', 'Median GPP', 'Highest GPP'))
legend(0.3,0.7, pch = unique(df_all_models$symbols), legend = c('BCC-CSM 1.1', 'GFDL-ESM 2G', 'MPI-ESM-LR', 'NorESM1-M'), bty = "o")

####################################################### qH-qC plane for all variables ############
##Taking the MPI_ESM LR model 
# RCP 8.5 

## GPP
setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="gpp")
gpp_1=nc_open("gpp_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc")

#set time
tid_1 <- gpp_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=gpp_1$dim$lat$vals
longi=gpp_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(gppvalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 
# median GPP: row 949, latitude/longitude: (258.75, 56.25), 104 X 59

View(gppsort) #highest GPP: latitude/longitude: (116.25, -1.25), 47 X 36
#lowest GPP: latitude/longitude: (33.75, 26.25), 14 X 47

mpi_gpp_qmetrics1 <- get.q.metrics(gppvalues[14,47, ]) # lowest
mpi_gpp_qmetrics2 <- get.q.metrics(gppvalues[104,59, ]) # median
mpi_gpp_qmetrics3 <- get.q.metrics(gppvalues[47,36, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

MPI_min <- data.frame(matrix(unlist(mpi_gpp_qmetrics1), ncol=3, byrow=F))
colnames(MPI_min) <- c('q', 'PEq', 'MPRq')
MPI_min <- colMeans(MPI_min)

MPI_med <- data.frame(matrix(unlist(mpi_gpp_qmetrics2), ncol=3, byrow=F))
colnames(MPI_med) <- c('q', 'PEq', 'MPRq')
MPI_med <- colMeans(MPI_med)

MPI_max <- data.frame(matrix(unlist(mpi_gpp_qmetrics3), ncol=3, byrow=F))
colnames(MPI_max) <- c('q', 'PEq', 'MPRq')
MPI_max <- colMeans(MPI_max)

df_all_gpp <- as.data.frame(rbind(MPI_min, MPI_med, MPI_max))
df_all_gpp$pixel <- c("lowest", "median", "highest")

### HFLS
setwd("D:/TimeSeriesAnalysis/Data/hfls/")
hflsvalues=get_CMIP5_array("hfls_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="hfls")
hfls_1=nc_open("hfls_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc")

#set time
tid_1 <- hfls_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=hfls_1$dim$lat$vals
longi=hfls_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
hflstempmean_1 = apply(hflsvalues,c(1,2),mean)

hflsmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  hflsmatrix[k,1]=hflstempmean_1[i,j]
  hflsmatrix[k,2]=longi[i]
  hflsmatrix[k,3]=lati[j]
  hflsmatrix[k,4]=i
  hflsmatrix[k,5]=j
  }
}  

hflssort = na.omit(hflsmatrix[order(hflsmatrix[,1],decreasing = T),])

which(hflssort[,1] == quantile(hflssort[,1], .5, type = 1)) 

View(hflssort) 
mpi_hfls_qmetrics1 <- get.q.metrics(hflsvalues[8,68, ]) # lowest
mpi_hfls_qmetrics2 <- get.q.metrics(hflsvalues[107,57, ]) # median
mpi_hfls_qmetrics3 <- get.q.metrics(hflsvalues[116,51, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

MPI_min <- data.frame(matrix(unlist(mpi_hfls_qmetrics1), ncol=3, byrow=F))
colnames(MPI_min) <- c('q', 'PEq', 'MPRq')
MPI_min <- colMeans(MPI_min)

MPI_med <- data.frame(matrix(unlist(mpi_hfls_qmetrics2), ncol=3, byrow=F))
colnames(MPI_med) <- c('q', 'PEq', 'MPRq')
MPI_med <- colMeans(MPI_med)

MPI_max <- data.frame(matrix(unlist(mpi_hfls_qmetrics3), ncol=3, byrow=F))
colnames(MPI_max) <- c('q', 'PEq', 'MPRq')
MPI_max <- colMeans(MPI_max)

df_all_hfls <- as.data.frame(rbind(MPI_min, MPI_med, MPI_max))
df_all_hfls$pixel <- c("lowest", "median", "highest")

###NPP

setwd("D:/TimeSeriesAnalysis/Data/npp/")
nppvalues=get_CMIP5_array("npp_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="npp")
npp_1 <- nc_open("npp_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc")

#set time
tid_1 <- npp_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=npp_1$dim$lat$vals
longi=npp_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
npptempmean_1 = apply(nppvalues,c(1,2),mean)

nppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  nppmatrix[k,1]=npptempmean_1[i,j]
  nppmatrix[k,2]=longi[i]
  nppmatrix[k,3]=lati[j]
  nppmatrix[k,4]=i
  nppmatrix[k,5]=j
  }
}  

nppsort = na.omit(nppmatrix[order(nppmatrix[,1],decreasing = T),])

which(nppsort[,1] == quantile(nppsort[,1], .5, type = 1)) 

View(nppsort) 
mpi_npp_qmetrics1 <- get.q.metrics(nppvalues[14,48, ]) # lowest
mpi_npp_qmetrics2 <- get.q.metrics(nppvalues[109,58, ]) # median
mpi_npp_qmetrics3 <- get.q.metrics(nppvalues[11,36, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

MPI_min <- data.frame(matrix(unlist(mpi_npp_qmetrics1), ncol=3, byrow=F))
colnames(MPI_min) <- c('q', 'PEq', 'MPRq')
MPI_min <- colMeans(MPI_min)

MPI_med <- data.frame(matrix(unlist(mpi_npp_qmetrics2), ncol=3, byrow=F))
colnames(MPI_med) <- c('q', 'PEq', 'MPRq')
MPI_med <- colMeans(MPI_med)

MPI_max <- data.frame(matrix(unlist(mpi_npp_qmetrics3), ncol=3, byrow=F))
colnames(MPI_max) <- c('q', 'PEq', 'MPRq')
MPI_max <- colMeans(MPI_max)

df_all_npp <- as.data.frame(rbind(MPI_min, MPI_med, MPI_max))
df_all_npp$pixel <- c("lowest", "median", "highest")

###PR
setwd("D:/TimeSeriesAnalysis/Data/pr/")
prvalues=get_CMIP5_array("pr_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="pr")
pr_1 <- nc_open("pr_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc")

#set time
tid_1 <- pr_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=pr_1$dim$lat$vals
longi=pr_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
prtempmean_1 = apply(prvalues,c(1,2),mean)

prmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  prmatrix[k,1]=prtempmean_1[i,j]
  prmatrix[k,2]=longi[i]
  prmatrix[k,3]=lati[j]
  prmatrix[k,4]=i
  prmatrix[k,5]=j
  }
}  

prsort = na.omit(prmatrix[order(prmatrix[,1],decreasing = T),])

which(prsort[,1] == quantile(prsort[,1], .5, type = 1)) 

View(prsort) 
mpi_pr_qmetrics1 <- get.q.metrics(prvalues[9,46, ]) # lowest
mpi_pr_qmetrics2 <- get.q.metrics(prvalues[11,63, ]) # median
mpi_pr_qmetrics3 <- get.q.metrics(prvalues[83,40, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

MPI_min <- data.frame(matrix(unlist(mpi_pr_qmetrics1), ncol=3, byrow=F))
colnames(MPI_min) <- c('q', 'PEq', 'MPRq')
MPI_min <- colMeans(MPI_min)

MPI_med <- data.frame(matrix(unlist(mpi_pr_qmetrics2), ncol=3, byrow=F))
colnames(MPI_med) <- c('q', 'PEq', 'MPRq')
MPI_med <- colMeans(MPI_med)

MPI_max <- data.frame(matrix(unlist(mpi_pr_qmetrics3), ncol=3, byrow=F))
colnames(MPI_max) <- c('q', 'PEq', 'MPRq')
MPI_max <- colMeans(MPI_max)

df_all_pr <- as.data.frame(rbind(MPI_min, MPI_med, MPI_max))
df_all_pr$pixel <- c("lowest", "median", "highest")

###TAS
setwd("D:/TimeSeriesAnalysis/Data/tas/")
tasvalues=get_CMIP5_array("tas_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc", var="tas")
tas_1 <- nc_open("tas_mon_MPI-ESM-LR_rcp85_r1i1p1_g025.nc")

#set time
tid_1 <- tas_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=tas_1$dim$lat$vals
longi=tas_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
tastempmean_1 = apply(tasvalues,c(1,2),mean)

tasmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  tasmatrix[k,1]=tastempmean_1[i,j]
  tasmatrix[k,2]=longi[i]
  tasmatrix[k,3]=lati[j]
  tasmatrix[k,4]=i
  tasmatrix[k,5]=j
  }
}  

tassort = na.omit(tasmatrix[order(tasmatrix[,1],decreasing = T),])

which(tassort[,1] == quantile(tassort[,1], .5, type = 1)) 

View(tassort) 
mpi_tas_qmetrics1 <- get.q.metrics(tasvalues[32,4, ]) # lowest
mpi_tas_qmetrics2 <- get.q.metrics(tasvalues[106,54, ]) # median
mpi_tas_qmetrics3 <- get.q.metrics(tasvalues[14,42, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

MPI_min <- data.frame(matrix(unlist(mpi_tas_qmetrics1), ncol=3, byrow=F))
colnames(MPI_min) <- c('q', 'PEq', 'MPRq')
MPI_min <- colMeans(MPI_min)

MPI_med <- data.frame(matrix(unlist(mpi_tas_qmetrics2), ncol=3, byrow=F))
colnames(MPI_med) <- c('q', 'PEq', 'MPRq')
MPI_med <- colMeans(MPI_med)

MPI_max <- data.frame(matrix(unlist(mpi_tas_qmetrics3), ncol=3, byrow=F))
colnames(MPI_max) <- c('q', 'PEq', 'MPRq')
MPI_max <- colMeans(MPI_max)

df_all_tas <- as.data.frame(rbind(MPI_min, MPI_med, MPI_max))
df_all_tas$pixel <- c("lowest", "median", "highest")

##merge dataframes, prepare dataframe for plotting
df_all_var <- rbind(df_all_gpp, df_all_hfls, df_all_npp, df_all_pr, df_all_tas)
df_all_var$var <- c(rep('gpp', 3), rep('hfls', 3), rep('npp', 3), rep('pr', 3), rep('tas', 3))
df_all_var$symbols <- c(rep(0, 3), rep(2, 3), rep(1, 3), rep(4, 3), rep(3, 3))
df_all_var$colours <-  rep(c('darkorchid4','darkgreen','firebrick') , 5)

row.names(df_all_var) <- 1:15

range01 <- function(x){(x-min(x))/(max(x)-min(x))} # function scales values between 0 and 1
df_all_var$PEq <- range01(df_all_var$PEq)

plot(df_all_var$PEq, df_all_var$MPRq, pch = df_all_var$symbols, 
     xlab = "q-entropy", ylab = "q-complexity", col = df_all_var$colours, cex = 1.5, lwd = 2)
legend(0,0.7, pch = 19, col = unique(df_all_var$colours), legend = c('lowest', 'median', 'highest'))
legend(0.3,0.7, pch = unique(df_all_var$symbols), legend = c('GPP', 'HFLS', 'NPP', 'PR', 'TAS'), bty = "o")
title("MPI-ESM2G RCP8.5: qH-qC plane",mgp=c(2,1,1))

####################################################### qH-qC plane for all scenarios ############
##Taking the NorESM1 model 
# GPP; RCP 26,45,85

#RCP 26
setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_NorESM1-M_rcp26_r1i1p1_g025.nc", var="gpp")
gpp_1=nc_open("gpp_mon_NorESM1-M_rcp26_r1i1p1_g025.nc")

#set time
tid_1 <- gpp_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=gpp_1$dim$lat$vals
longi=gpp_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(gppvalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 

View(gppsort)

nor_gpp_qmetrics1 <- get.q.metrics(gppvalues[67,63, ]) # lowest
nor_gpp_qmetrics2 <- get.q.metrics(gppvalues[36,49, ]) # median
nor_gpp_qmetrics3 <- get.q.metrics(gppvalues[10,37, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

BCC_min <- data.frame(matrix(unlist(nor_gpp_qmetrics1), ncol=3, byrow=F))
colnames(BCC_min) <- c('q', 'PEq', 'MPRq')
BCC_min <- colMeans(BCC_min)

BCC_med <- data.frame(matrix(unlist(nor_gpp_qmetrics2), ncol=3, byrow=F))
colnames(BCC_med) <- c('q', 'PEq', 'MPRq')
BCC_med <- colMeans(BCC_med)

BCC_max <- data.frame(matrix(unlist(nor_gpp_qmetrics3), ncol=3, byrow=F))
colnames(BCC_max) <- c('q', 'PEq', 'MPRq')
BCC_max <- colMeans(BCC_max)

df_all_nor26 <- as.data.frame(rbind(BCC_min, BCC_med, BCC_max))
df_all_nor26$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")


#RCP 45
setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_NorESM1-M_rcp45_r1i1p1_g025.nc", var="gpp")
gpp_1=nc_open("gpp_mon_NorESM1-M_rcp45_r1i1p1_g025.nc")

#set time
tid_1 <- gpp_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=gpp_1$dim$lat$vals
longi=gpp_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(gppvalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 

View(gppsort)

nor_gpp_qmetrics1 <- get.q.metrics(gppvalues[67,63, ]) # lowest
nor_gpp_qmetrics2 <- get.q.metrics(gppvalues[115,58, ]) # median
nor_gpp_qmetrics3 <- get.q.metrics(gppvalues[10,37, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

BCC_min <- data.frame(matrix(unlist(nor_gpp_qmetrics1), ncol=3, byrow=F))
colnames(BCC_min) <- c('q', 'PEq', 'MPRq')
BCC_min <- colMeans(BCC_min)

BCC_med <- data.frame(matrix(unlist(nor_gpp_qmetrics2), ncol=3, byrow=F))
colnames(BCC_med) <- c('q', 'PEq', 'MPRq')
BCC_med <- colMeans(BCC_med)

BCC_max <- data.frame(matrix(unlist(nor_gpp_qmetrics3), ncol=3, byrow=F))
colnames(BCC_max) <- c('q', 'PEq', 'MPRq')
BCC_max <- colMeans(BCC_max)

df_all_nor45 <- as.data.frame(rbind(BCC_min, BCC_med, BCC_max))
df_all_nor45$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")

#RCP 85
setwd("D:/TimeSeriesAnalysis/Data/gpp/")
gppvalues=get_CMIP5_array("gpp_mon_NorESM1-M_rcp85_r1i1p1_g025.nc", var="gpp")
gpp_1=nc_open("gpp_mon_NorESM1-M_rcp85_r1i1p1_g025.nc")

#set time
tid_1 <- gpp_1$dim$time$vals
timeaxis_1 = as.Date.numeric(tid_1,origin="1800-01-01")
lati=gpp_1$dim$lat$vals
longi=gpp_1$dim$lon$vals
longicorrect = longi
longicorrect[longicorrect > 180] = longicorrect[longicorrect > 180] - 360

#calculate mean of each pixels
gpptempmean_1 = apply(gppvalues,c(1,2),mean)

gppmatrix = matrix(nrow=72*144,ncol=5) 
k=0
for (i in 1:144)
{
  for (j in 1:72)
  { k=k+1
  gppmatrix[k,1]=gpptempmean_1[i,j]
  gppmatrix[k,2]=longi[i]
  gppmatrix[k,3]=lati[j]
  gppmatrix[k,4]=i
  gppmatrix[k,5]=j
  }
}  

gppsort = na.omit(gppmatrix[order(gppmatrix[,1],decreasing = T),])

which(gppsort[,1] == quantile(gppsort[,1], .5, type = 1)) 

View(gppsort)

nor_gpp_qmetrics1 <- get.q.metrics(gppvalues[67,63, ]) # lowest
nor_gpp_qmetrics2 <- get.q.metrics(gppvalues[49,27, ]) # median
nor_gpp_qmetrics3 <- get.q.metrics(gppvalues[10,37, ]) # highest

#data.frame(matrix(unlist(sapply(gpp_gfdl_metrics1, "[", c(1,2,3,5,6)), nrow=length(gpp_gfdl_metrics1), byrow=T))

BCC_min <- data.frame(matrix(unlist(nor_gpp_qmetrics1), ncol=3, byrow=F))
colnames(BCC_min) <- c('q', 'PEq', 'MPRq')
BCC_min <- colMeans(BCC_min)

BCC_med <- data.frame(matrix(unlist(nor_gpp_qmetrics2), ncol=3, byrow=F))
colnames(BCC_med) <- c('q', 'PEq', 'MPRq')
BCC_med <- colMeans(BCC_med)

BCC_max <- data.frame(matrix(unlist(nor_gpp_qmetrics3), ncol=3, byrow=F))
colnames(BCC_max) <- c('q', 'PEq', 'MPRq')
BCC_max <- colMeans(BCC_max)

df_all_nor85 <- as.data.frame(rbind(BCC_min, BCC_med, BCC_max))
df_all_nor85$pixel <- c("lowest_GPP", "median_GPP", "highest_GPP")

##merge dataframes, prepare dataframe for plotting
df_all_rcp <- rbind(df_all_nor26, df_all_nor45, df_all_nor85)
df_all_rcp$rcp <- c(rep('2.6', 3), rep('4.5', 3), rep('8.5', 3))
df_all_rcp$colours <- c(rep('darkorchid4', 3), rep('darkgreen', 3), rep('firebrick', 3))
df_all_rcp$symbols <-  rep(c(0,2,3) , 3)

#df_all_rcp$colours <-  rep(c('darkorchid4','darkgreen','firebrick') , 5)
#df_all_rcp$symbols <- c(rep(0, 3), rep(2, 3), rep(1, 3), rep(4, 3), rep(3, 3))

row.names(df_all_rcp) <- 1:9

range01 <- function(x){(x-min(x))/(max(x)-min(x))} # function scales values between 0 and 1
df_all_rcp$PEq <- range01(df_all_rcp$PEq)

plot(df_all_rcp$PEq, df_all_rcp$MPRq, pch = df_all_rcp$symbols, 
     xlab = "q-entropy", ylab = "q-complexity", col = df_all_rcp$colours, cex = 1.5, lwd = 2)
legend(0,0.7, pch = df_all_rcp$symbols, legend = c('lowest', 'median', 'highest'))
legend(0.3,0.7, pch = 20, col = unique(df_all_rcp$colours), legend = c('RCP 2.6', 'RCP 4.5', 'RCP 8.5'), bty = "o")
title("NorESM1: qH-qC plane",mgp=c(2,1,1))
