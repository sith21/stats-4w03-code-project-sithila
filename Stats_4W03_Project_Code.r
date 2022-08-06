library(randomForest)
library(tree)
library(mgcv)
library(boot)
library(mvtnorm)
library(splines)
library(janitor)
library(MASS)
library(mvtnorm)
library(tibble)
library(ggplot2)
library(tidyverse)
library(tidybayes)
library(fields)
library(GGally)
require(plyr)
require(reshape2)
require(MASS)

AirQuality = (read.csv2("AirQualityUCI.csv")
    |> remove_empty(which = c("rows", "cols"))
)
AirQuality = AirQuality[!is.na(AirQuality),]
AirQuality = AirQuality[-c(16:17)]
AirQuality = AirQuality[-c(1:2)]
AirQuality = na.omit(AirQuality)
AirQuality = AirQuality = subset(AirQuality, T!=-200 & NMHC.GT. != -200 & RH!= -200 & NOx.GT. != -200 & CO.GT. != -200 & PT08.S2.NMHC. != - 200
                                & PT08.S5.O3.!= -200 & AH != -200)

head(AirQuality)

ggpairs(AirQuality,                 # Data frame
        columns = 1:10,        # Columns
        aes(color = "smooth",  # Color by group (cat. variable)
            alpha = 0.5)) 
#co PT08.S1.CO.	PT08.S2.NMHC. NOx.GT. PT08.S4.NO2.

#From the ggpairs plot we can see that C6H6 has the highest correlation amongst the chemicals. 
#Benzene contributes to smog and smog consists of sulphur oxides, NOx and ammonia fas

gam.s1 = gam(C6H6.GT. ~ s(CO.GT.,k = 4, bs = "gp") + 
             s(PT08.S1.CO., k = 5, bs = "gp") + 
             s(PT08.S2.NMHC., k = 5, bs = "gp") +
             s(NOx.GT., k = 15,bs = "gp") +
             s(PT08.S4.NO2., k = 5, bs = "gp") , data = AirQuality)
             
gam.s2 = gam(C6H6.GT. ~ s(CO.GT.,k = 4, bs = "gp") + 
             s(PT08.S1.CO., k = 5, bs = "gp") + 
             s(PT08.S2.NMHC., k = 5, bs = "gp") +
             s(NOx.GT., k = 15 , bs = "gp") + PT08.S4.NO2. , data = AirQuality)            
             
gam.s3 = gam(C6H6.GT. ~ s(CO.GT.,k = 4, bs = "gp") + 
             s(PT08.S1.CO., k = 5, bs = "gp") + 
             s(PT08.S2.NMHC., k = 5, bs = "gp") +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)     

gam.s4 =  gam(C6H6.GT. ~ s(CO.GT.,k = 4, bs = "gp") + 
             s(PT08.S1.CO., k = 5, bs = "gp") + 
             PT08.S2.NMHC. +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)


gam.s5 = gam(C6H6.GT. ~ s(CO.GT.,k = 4, bs = "gp") + 
             PT08.S1.CO. + 
             PT08.S2.NMHC. +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)

gam.s6 = gam(C6H6.GT. ~ CO.GT. + 
             PT08.S1.CO. + 
             PT08.S2.NMHC. +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)

summary(gam.s1)
gam.check(gam.s1)
plot.gam(gam.s1, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

gam.s1 = gam(C6H6.GT. ~ s(CO.GT.,k = 15, bs = "gp") + 
             s(PT08.S1.CO., k = 15, bs = "gp") + 
             s(PT08.S2.NMHC., k = 15, bs = "gp") +
             s(NOx.GT., k = 15, bs = "gp") +
             s(PT08.S4.NO2., k = 15, bs = "gp") , data = AirQuality)
summary(gam.s1)
gam.check(gam.s1)
plot.gam(gam.s1, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

vis.gam(gam.s1)

summary(gam.s2)
gam.check(gam.s2)
plot.gam(gam.s2, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

gam.s2 = gam(C6H6.GT. ~ s(CO.GT.,k = 15, bs = "gp") + 
             s(PT08.S1.CO., k = 15, bs = "gp") + 
             s(PT08.S2.NMHC., k = 15, bs = "gp") +
             s(NOx.GT., k = 15 , bs = "gp") + PT08.S4.NO2. , data = AirQuality)  
summary(gam.s2)
gam.check(gam.s2)
plot.gam(gam.s2, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

vis.gam(gam.s2)

summary(gam.s3)
gam.check(gam.s3)
plot.gam(gam.s3, residuals = TRUE, pch = 1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

gam.s3 = gam(C6H6.GT. ~ s(CO.GT.,k = 4, bs = "gp") + 
             s(PT08.S1.CO., k = 15, bs = "gp") + 
             s(PT08.S2.NMHC., k = 15, bs = "gp") +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)     
summary(gam.s3)
gam.check(gam.s3)
plot.gam(gam.s3, residuals = TRUE, pch = 1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

vis.gam(gam.s3)

summary(gam.s4)
gam.check(gam.s4)
plot.gam(gam.s4, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

gam.s4 =  gam(C6H6.GT. ~ s(CO.GT.,k = 12, bs = "gp") + 
             s(PT08.S1.CO., k = 10, bs = "gp") + 
             PT08.S2.NMHC. +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)
summary(gam.s4)
gam.check(gam.s4)
plot.gam(gam.s4, residuals = TRUE, pch = 1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

vis.gam(gam.s4)

summary(gam.s5)
gam.check(gam.s5)
plot.gam(gam.s5, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

gam.s5 = gam(C6H6.GT. ~ s(CO.GT.,k = 4, bs = "gp") + 
             PT08.S1.CO. + 
             PT08.S2.NMHC. +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)
summary(gam.s5)
gam.check(gam.s5)
plot.gam(gam.s5, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

vis.gam(gam.s5)

summary(gam.s6)
gam.check(gam.s6)
plot.gam(gam.s6, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

gam.s6 = gam(C6H6.GT. ~ CO.GT. + 
             PT08.S1.CO. + 
             PT08.S2.NMHC. +
             + NOx.GT. + PT08.S4.NO2. , data = AirQuality)
summary(gam.s6)
gam.check(gam.s6)
plot.gam(gam.s6, residuals = TRUE, pch =1, cex = 0.5, shade = 
         TRUE, shade.col = "lightblue", seWithMean = TRUE, 
         pages = 1, all.terms = TRUE)

vis.gam(gam.s6)

k = gam(C6H6.GT. ~ te(CO.GT.,PT08.S1.CO.) , data = AirQuality)
plot(k)
vis.gam(k)
l = gam(C6H6.GT. ~ te(CO.GT.,PT08.S1.CO.,PT08.S2.NMHC.), data = AirQuality)
plot(l)
vis.gam(l)
f = gam(C6H6.GT. ~ te(CO.GT.,PT08.S2.NMHC.), data = AirQuality)
plot(f)
vis.gam(f)

response = 'C6H6.GT.'

terms = c('s(CO.GT.)', 's(PT08.S1.CO.)', 's(PT08.S2.NMHC.)')

g = gam(as.formula(sprintf('%s~%s', response, paste(terms,collapse = '+'))), data = AirQuality)
g

plot(g)
vis.gam(g)

response = 'C6H6.GT.'

terms = c('s(CO.GT.)', 's(PT08.S2.NMHC.)')

m = gam(as.formula(sprintf('%s~%s', response, paste(terms,collapse = '+'))), data = AirQuality)
m
vis.gam(m)


