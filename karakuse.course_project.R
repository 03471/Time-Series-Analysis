packages <- c('xts','astsa', 'forecast','rstudioapi','pander')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)  
}

for(package in packages){
  require(package, character.only = TRUE)
}

#data source
#https://www.nrel.gov/grid/solar-power-data.html
"NREL's Solar Power Data for Integration Studies are synthetic solar photovoltaic (PV) 
power plant data points for the United States representing the year 2006."

#Table 3.1. Behavior of the ACF and PACF for ARMA models
table.3.1 <- c("","AR(p)","MA(q)","ARMA(p,q)")
table.3.1 <- rbind(table.3.1,c("ACF","Tails off","Cuts off after lag q","Tails off"))
table.3.1 <- rbind(table.3.1,c("PACF","Cuts off after lag p","Tails off q","Tails off"))
pandoc.table(table.3.1)

#rstudio only
#extract time series data from raw file
setwd(paste(dirname(rstudioapi::getActiveDocumentContext()$path),'tx-east-pv-2006',sep = '/'))
data.raw <- read.csv("DA_35.35_-101.85_2006_DPV_28MW_60_Min.csv",header = TRUE)

#assing formatted data and col names
data.raw.formatted <- data.frame(c(strptime(data.raw$LocalTime, "%m/%d/%y %H:%M")), c(as.double(data.raw$Power.MW.)))
colnames(data.raw.formatted) <- c("TimeStamp","Power.MW")

#aggregation to generate daily data
data.daily <- aggregate(data.raw.formatted$Power.MW, list(hour=cut(data.raw.formatted$TimeStamp,"day")), sum)

#check size of the data
dim(data.daily)
data.daily.train <- data.daily[1:90,]
data.daily.test <- data.daily[91:97,]

colnames(data.daily.train) <- c("TimeStamp","Power.MW")
names(data.daily.train)

min(as.Date(data.daily.train$TimeStamp))
max(as.Date(data.daily.train$TimeStamp))

data.daily.train.formatted <- data.frame(as.Date(data.daily.train$TimeStamp),as.double(data.daily.train$Power.MW))
colnames(data.daily.train.formatted) <- c("TimeStamp","Power.MW")

data.daily.train.ts <- ts(data.daily.train.formatted$Power.MW,start=c(2006,60),frequency=365)

par(mfrow=c(1,1))
plot(data.daily.train.ts, col="blue")
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
acf2(data.daily.train.ts)
#ACF cuts off PACF tails off

#ARMA(1,1)
sarima(data.daily.train.ts, 1, 1, 1)   


#FORECAST
#option-1
sarima.forecast <- sarima.for(data.daily.train.ts, 7,1,1,1)

data.daily.fit = arima(data.daily.train.ts, order=c(1,1,1))
tsdiag(data.daily.fit, gof.lag=30) 

#option-2
data.daily.pr = predict(data.daily.fit, n.ahead=7)  # forecasts
data.daily.pr$pred

U = data.daily.pr$pred + 1.96*data.daily.pr$se
L = data.daily.pr$pred - 1.96*data.daily.pr$se

minx = min(data.daily.train.ts,L) 
maxx = max(data.daily.train.ts,U)

ts.plot(data.daily.train.ts, col="gray",xlim=c(2006.30,2006.45), data.daily.pr$pred, ylim=c(minx,maxx), main="TX-2006 Solar Power Plant Production Forecast") 
lines(data.daily.pr$pred, col="red", type="o")
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")

#validation
#plot predicted and actual values
ylim.min <- min(data.daily.test[,2],c(data.daily.pr$pred))-5
ylim.max <- max(data.daily.test[,2],c(data.daily.pr$pred))+5
plot(data.daily.test[,2],ylim=c(ylim.min, ylim.max), col="red", pch = 15, main="TX-2006 Solar Power Plant Production Forecast Comparison", ylab = "Power (MW)", xlab = "Predicted days")
text(x=1:7, y=data.daily.test[,2]+3,cex =.7, offset = 3, labels=data.daily.test[,2], col="red") 
axis(side=1, at=c(1:7))
points(c(data.daily.pr$pred), col="blue", pch = 16)
text(x=1:7, y=c(data.daily.pr$pred)+3,cex =.7, offset = 3, labels=c(round(data.daily.pr$pred,2)), col="blue") 
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

#conclusion
"
3- residuals are correlated and p-values > .05 
4- prediction outputs found in expected range when compred with validation data
"
