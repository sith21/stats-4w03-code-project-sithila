#library(GGally)
library(randomForest)
library(tree)
#library(GauPro)
#library(gam) #doesnt work for gam(...~ s(..., bs = ""))
library(mgcv)
library(boot)
library(splines)
library(janitor)
#install.packages("GauPro")
#library(GauPro)


AirQuality = (read.csv2("AirQualityUCI.csv")
    |> remove_empty(which = c("rows", "cols"))
)
AirQuality = AirQuality[!is.na(AirQuality),]
AirQuality = AirQuality[-c(16:17)]
AirQuality = AirQuality[-c(1:2)]
AirQuality = na.omit(AirQuality)

#gam.ns1 = lm(T ~ ns(CO.GT., 4) + ns(C6H6.GT., 5) + NOx.GT., data = AirQuality)
gam.s1 = gam(T ~ s(CO.GT.,k = 4, bs = "gp") + s(C6H6.GT., k = 5, bs = "gp") + s(NOx.GT., bs = "gp"), data = AirQuality)
gam.s2 = gam(T ~ s(CO.GT., bs = "gp") + s(C6H6.GT., bs = "gp") + NOx.GT., data = AirQuality)
gam.s3 = gam(T ~ s(C6H6.GT., bs = "gp") + s(NO2.GT., bs = "gp"), data = AirQuality)
gam.s4 = gam(T ~ s(C6H6.GT., bs = "gp")+ s(NO2.GT., bs = "gp") + s(NOx.GT.,bs = "gp"), data = AirQuality)
gam.s5 = gam(T ~ C6H6.GT. + s(NO2.GT., bs = "gp") + s(NOx.GT.,bs = "gp"), data = AirQuality)

summary(gam.s1)
gam.check(gam.s1)
#i re do gam.s1/s2/s3... so that the p value doesnt have the *** (according to this https://noamross.github.io/gams-in-r-course/chapter2)
gam.s1 = gam(T ~ s(CO.GT.,k = 5, bs = "gp") + s(C6H6.GT., k = 13, bs = "gp") + s(NOx.GT., bs = "gp", k = 13), data = AirQuality)
summary(gam.s1)
gam.check(gam.s1)

plot.gam(gam.s1, residuals = TRUE, pch =1, cex = 1, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

summary(gam.s2)
gam.check(gam.s2)

plot.gam(gam.s2, residuals = TRUE, pch =1, cex = 1, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

summary(gam.s3)
gam.check(gam.s3)

gam.s3 = gam(T ~ s(C6H6.GT., bs = "gp", k = 100) + s(NO2.GT., bs = "gp", k = 15), data = AirQuality)
summary(gam.s3)
gam.check(gam.s3)

plot.gam(gam.s3, residuals = TRUE, pch =1, cex = 1, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

summary(gam.s4)
gam.check(gam.s4)

gam.s4 = gam(T ~ s(C6H6.GT., bs = "gp", k = 15)+ s(NO2.GT., bs = "gp", k = 15) + s(NOx.GT.,bs = "gp", k = 15), data = AirQuality)
summary(gam.s4)
gam.check(gam.s4)

plot.gam(gam.s4, residuals = TRUE, pch =1, cex = 1, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

summary(gam.s5)
gam.check(gam.s5)

gam.s5 = gam(T ~ C6H6.GT. + s(NO2.GT., bs = "gp", k = 13) + s(NOx.GT.,bs = "gp", k = 13), data = AirQuality)
summary(gam.s5)
gam.check(gam.s5)

plot.gam(gam.s5, residuals = TRUE, pch =1, cex = 1, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)
lines()












