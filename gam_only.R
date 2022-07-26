library(randomForest)
library(tree)
library(gam)
library(boot)
AirQualityDirty = read.csv("AirQualityUCI.csv", sep= ";")
AirQuality = AirQualityDirty[!is.na(AirQualityDirty),]
AirQuality = AirQuality[-c(16:17)]
AirQuality = AirQuality[!is.na(AirQuality$T),]
AirQuality = na.omit(AirQuality)
AirQuality$CO.GT. = as.numeric(gsub(",", ".", AirQuality$CO.GT))
AirQuality$C6H6.GT. = as.numeric(gsub(",", ".", AirQuality$C6H6.GT.))
AirQuality$T = as.numeric(gsub(",", ".", AirQuality$T))
AirQuality$RH = as.numeric(gsub(",", ".", AirQuality$RH))
AirQuality$AH = as.numeric(gsub(",", ".", AirQuality$AH))
AirQuality = AirQuality[-c(1:2)]
gam.ns1 = lm(T ~ ns(CO.GT., 4) + ns(C6H6.GT., 5) + NOx.GT., data = AirQuality)
gam.ns1
