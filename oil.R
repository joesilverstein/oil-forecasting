# Forecasts the price that US refiners pay for imported crude oil.
# Based on Kilian and Murphy (2014)

library("Quandl")
library("vars")
library("tseries")

setwd("/Users/joesilverstein/Google Drive/Forecasting/Kilian Oil Forecasting Model")
load("oil.RData")

# Load data
cpi0 = read.csv("data/CPIAUCNS.csv") # raw CPI
price0 = read.csv("data/MER_T09_01.csv") # refiner acquisition cost of crude
production0 = read.csv("data/MER_T11_01B.csv") # world crude production
inventories0 = read.csv("data/MCRSTUS1m/Data 1-Table 1.csv") # oil inventories
shipping0 = read.table("data/dry_shipping_deviations_4-9-16.txt") # measure of global economic activity

Quandl.api_key("zHcHWQZmtHzS4nUBGqty")
usl0 = Quandl(code = "ZEP/USL", type = "ts", collapse = "monthly", order = "asc") # USL end-of-month prices (https://www.quandl.com/data/ZEP/USL)

# Subset raw data
cpi1 = cpi0
price1 = price0[which(price0$Description == 'Refiner Acquisition Cost of Crude Oil, Imported' & price0$YYYYMM >= 197401 & price0$YYYYMM %% 100 != 13), c("YYYYMM","Value")]
production1 = production0[which(production0$Description=='Crude Oil Production, World' & production0$YYYYMM %% 100 != 13), c("YYYYMM","Value")]
inventories1 = inventories0[3:nrow(inventories0),1:2]
inventories1 = inventories1[-nrow(inventories1),]
shipping1 = shipping0

# Rename variables in raw data
names(cpi1) = c("date","cpi")
names(price1) = c("date","price")
names(production1) = c("date","production")
names(inventories1) = c("date","inventory")
names(shipping1) = c("shipping")

# Reformat raw data as time series
cpi.ts = ts(data=cpi1$cpi, frequency=12, start=c(1973,1), end=c(2016,2))
price1$price = as.numeric(levels(price1$price))[price1$price]
price.ts = ts(data=price1$price, frequency=12, start=c(1974,1), end=c(2016,1))
production1$production = as.numeric(levels(production1$production))[production1$production]
production.ts = ts(data=production1$production, frequency=12, start=c(1973,1), end=c(2015,12))
inventories1$inventory = as.numeric(levels(inventories1$inventory))[inventories1$inventory]
inventories.ts = ts(inventories1$inventory, frequency=12, start=c(1920,1), end=c(2016,1))
shippingDevs.ts = ts(data=shipping1$shipping, frequency=12, start=c(1968,1), end=c(2015,12))
usl.ts = usl0[, "CLOSE"]

# Extend price data back to 1973 using Barsky and Kilian methodology (gives 3 extra observations)


# Create percent changes in production
percChangeProduction.ts = production.ts/lag(production.ts,-1) - 1

# Take first difference of inventories to stationarize the series
diffInventories.ts = diff(inventories.ts)

# Deflate price by CPI
# (It's smart enough to know to line up the dates when dividing)
realPrice.ts = price.ts / cpi.ts

# Express oil in log-levels as in Kilian 2009
logRealPrice.ts = log(realPrice.ts)

# Merge raw data by month
y = ts.intersect(logRealPrice.ts, percChangeProduction.ts, diffInventories.ts, shippingDevs.ts)

# Check to make sure all series are stationary
# Remember that the p-value is the probability that the null hypothesis is true.
adf.test(logRealPrice.ts, k=24) # probably non-stationary
adf.test(percChangeProduction.ts, k=24) # definitely stationary
adf.test(diffInventories.ts, k=24) # definitely stationary
adf.test(shippingDevs.ts, k=24) # probably stationary

# Estimate VAR. Remember to tell it to include seasonal dummies.
mod = VAR(y, p=24, season=4) # 4 seasonal dummies per year

# Use model to forecast oil prices
mod.pred = predict(mod, n.ahead=18)

View(mod.pred$fcst$logRealPrice.ts)
logPrice.fcst = mod.pred$fcst$logRealPrice.ts[, 1] # this is the spot oil price forecast
completeLogPrice.fcst = append(y[, 1], logPrice.fcst) # append onto end
completeLogPrice.ts = ts(data=completeLogPrice.fcst, frequency=12, start=c(1974,1), end=c(2017,6)) # convert to ts
completePrice.ts = exp(completeLogPrice.ts)

plot(completePrice.ts)
forecast.ts = window(completePrice.ts, start=c(2015,12), c(2016,6))
plot(forecast.ts)

# Should also re-inflate by forecasted CPI, although it won't matter very much

# Add-in time series of USL, to get a direct prediction of it.
# Some of these are end of month values, so split year into 12 periods and use the closing price on the last day of each of those periods.
# Problem: USL has only been around since 2007. Could use crude oil futures, but they've only been around since 1986. 
# Could use missing data methods, since USL is likely to be highly correlated with logPrice.
logUSL.ts = log(usl.ts)
y = ts.intersect(percChangeProduction.ts, diffInventories.ts, shippingDevs.ts, logUSL.ts)
modWithUSL = VAR(y, p=24, season=4) # Some of the coefficients are NA, which carries over into the next line.
modWithUSL.pred = predict(modWithUSL, n.ahead = 18) # it's all NA's for some reason
logUSL.fcst = modWithUSL.pred$fcst$logUSL.ts[, 1] # this is the spot oil price forecast
completeLogUSL.fcst = append(y[, 1], logUSL.fcst) # append onto end
completeLogUSL.ts = ts(data=completeLogUSL.fcst, frequency=12, start=c(2007,12), end=c(2017,6)) # convert to ts
completeUSL.ts = exp(completeLogUSL.ts)
plot(completeUSL.ts)
forecastUSL.ts = window(completeUSL.ts, start=c(2015,12), c(2016,6))
plot(forecastUSL.ts)

# Should load-in more data, because it's now available.

# Test whether forecast is not spurious using White (2000) reality check

# Try vector autoregressive random forest
