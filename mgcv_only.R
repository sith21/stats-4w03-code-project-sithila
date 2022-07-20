library(mgcv)
library(splines)
library(janitor)  ## for remove_empty
AirQuality = (read.csv2("AirQualityUCI.csv")
    |> remove_empty(which = "cols")
)

gam.ns1 = lm(T ~ ns(CO.GT., 4) + ns(C6H6.GT., 5) + NOx.GT., data = AirQuality)
gam.ns2 = gam(T ~ s(CO.GT., bs = "gp") + s(C6H6.GT., bs = "gp") + NOx.GT.,
              data = AirQuality)
gam.ns1
gam.ns2
