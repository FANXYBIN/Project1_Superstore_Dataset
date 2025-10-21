
superstore <- read.csv("/Users/jamesli/Downloads/superstore_dataset2011-2015.csv")
superstore
dim(superstore)

# Check the datatype of each column 
sapply(superstore, class)
str(superstore)

# Check the existence of NA
is.na(superstore[, ])
# Find total NA values in superstore
sum(is.na(superstore))
# Find total NA values by column
sapply(superstore, function(superstore) sum(is.na(superstore)))
# Create a logical vector indicating whether each column has no missing values
new_superstore <- superstore[colSums(is.na(superstore)) == 0]
new_superstore
nrow(new_superstore)
ncol(new_superstore)

attach(new_superstore)

# Total number of orders by Market, Region, Ship Mode, Segment
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
counts_market <- table(Market)
barplot(counts_market, main = "Total Orders by Market", col = "lightblue", ylim = c(0, 12000))
counts_region <- table(Region)
barplot(counts_region, main = "Total Orders by Region", col = "lightblue", ylim = c(0, 12000), las = 2, cex.names = 0.85)
counts_shipmode <- table(Ship.Mode)
barplot(counts_shipmode, main = "Total Orders by Ship Mode", col = "lightblue")
counts_segment <- table(Segment)
barplot(counts_segment, main = "Total Orders by Segment", col = "lightblue", ylim = c(0, 30000))
par(opar)

# Total Sales/Profit by Market/Region
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
barplot(tapply(new_superstore$Sales, new_superstore$Region, FUN = sum), main = "Total Sales by Region", col = "mediumpurple1", las = 2, cex.names = 0.85)
barplot(tapply(new_superstore$Profit, new_superstore$Region, FUN = sum), main = "Total Profit by Region", col = "mediumpurple1", las = 2, cex.names = 0.85)
barplot(tapply(new_superstore$Sales, new_superstore$Market, FUN = sum), main = "Total Sales by Market", col = "lavender", ylim = c(0, 4000000))
barplot(tapply(new_superstore$Profit, new_superstore$Market, FUN = sum), main = "Total Profit by Market", col = "lavender", ylim = c(0, 500000))
par(opar)

# Create bar plots of Total orders by Category and Sub.Category
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,1))
category_bar <- table(Category)
category_bar
barplot(category_bar, main = "Total orders by Category", col = "orange")
sub.category_bar <- table(Sub.Category)
sub.category_bar
barplot(sub.category_bar, main = "Total orders by Sub.Category", col = "orange", las = 2, cex.names = 0.8)
par(opar)

# Create a pie chart of Total orders by sub.category
set.seed(10000)
sub.category_sample <- sample(Sub.Category, 10000, replace = TRUE)
sub.category_pie <- table(sub.category_sample)
sub.category_pie = as.data.frame(sub.category_pie)
sub.category_pie
pct <- round(100 * sub.category_pie$Freq / sum(sub.category_pie$Freq))
pie(sub.category_pie$Freq, main = "Pie Chart of sub.category", 
    labels = paste(sub.category_pie$sub.category_sample, sep = " ", pct, "%"),
    sub = paste("Assignment#4", format(Sys.time(),"%Y-%b-%d %H:%S")))

# Total Sales/Profit by Category/Sub.Category
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
barplot(tapply(new_superstore$Sales, new_superstore$Sub.Category, FUN = sum), main = "Total Sales by Sub.Category", col = "darkkhaki", las = 2, cex.names = 0.85)
barplot(tapply(new_superstore$Profit, new_superstore$Sub.Category, FUN = sum), main = "Total Profit by Sub.Category", col = "darkkhaki", las = 2, cex.names = 0.85)
barplot(tapply(new_superstore$Sales, new_superstore$Category, FUN = sum), main = "Total Sales by Category", col = "khaki", ylim = c(0, 5000000))
barplot(tapply(new_superstore$Profit, new_superstore$Category, FUN = sum), main = "Total Profit by Category", col = "khaki", ylim = c(0, 700000))
par(opar)

# Create tree map
library(treemap)
treemap(new_superstore, index = c("Category", "Sub.Category"), vSize = "Sales", 
        vColor = "Profit", type = "value", palette = "RdYlGn", 
        title = "Sales Treemap For categories", fontsize.labels = c(15, 10), 
        align.labels = list(c("centre", "centre"), c("left", "top")))




# Create scatterplot
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
plot(Sales, Discount, main = "Scatterplot of Sales vs Discount")
plot(Sales, Quantity, main = "Scatterplot of Sales vs Quantity", col = "red4")
plot(Sales, Profit, main = "Scatterplot of Sales vs Profit", col= "darkgreen")
abline(lm(Profit~Sales),col="red")
plot(Sales, Shipping.Cost, main = "Scatterplot of Sales vs Shipping.Cost", col = "darkblue")
abline(lm(Shipping.Cost~Sales),col="red")
par(opar)

# Create density plot of Sales, Profit, Quantity, Discount
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
sales_density <- density(Sales)
hist(Sales, prob = TRUE, 
     ylim = c(0, 0.002), col = "green", main = "Distribution of Sales variable")
lines(sales_density, col = "blue", lwd = 2)

profit_density <- density(Profit)
hist(Profit, prob = TRUE, 
     ylim = c(0, 0.01), col = "green", main = "Distribution of Profit variable")
lines(profit_density, col = "blue", lwd = 2)

quantity_density <- density(Quantity)
hist(Quantity, prob = TRUE,  
     xlim = c(0, 16), ylim = c(0, 0.5), col = "green", main = "Distribution of Quantity variable")
lines(quantity_density, col = "blue", lwd = 2)

discount_density <- density(Discount)
hist(Discount, prob = TRUE, 
     ylim = c(0, 14), col = "green", main = "Distribution of Discount variable")
lines(discount_density, col = "blue", lwd = 2)
par(opar)

# Correlation between Sales, Quantity, Discount, Profit and Shipping.Cost
library(corrplot, quietly = TRUE)
superstore2 <- new_superstore[,18:22]
cor(superstore2)
COR1 <- cor(superstore2, method = "pearson")
corrplot(COR1, method = "color", mar = c(0, 0, 1, 0))
corrplot.mixed(COR1, order = 'AOE') 
corrplot(COR1, method = 'ellipse', order = 'AOE', type = 'upper')

# Create boxplot of Quantity
summary(new_superstore)
boxplot(superstore2, col = 2:6, main="Boxplot of superstore2", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")), ylim = c(-50, 300))
boxplot(Sales, col = "brown", main = "Boxplot of Sales", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")))
boxplot(Sales, col = "brown", main = "Boxplot of Sales", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")),ylim = c(0, 400))
boxplot(Quantity, col = "aquamarine2", main = "Boxplot of Quantity", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")))
boxplot(Discount, col = "cornflowerblue", main = "Boxplot of Discount", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")))
boxplot(Profit, col = "darkorange", main = "Boxplot of profit", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")))
boxplot(Profit, col = "darkorange", main = "Boxplot of profit", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")), ylim = c(-100, 100))
boxplot(Shipping.Cost, col = "darkolivegreen1", main = "Boxplot of Shipping.Cost", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")))
boxplot(Shipping.Cost, col = "darkolivegreen1", main = "Boxplot of Shipping.Cost", 
        sub = paste("The Final Project", format(Sys.time(),"%Y-%b-%d %H:%S")), ylim = c(0, 100))



# Year/Month Sales and Profit
library(tidyr)
library(data.table)
library(forecast)
library(tseries)
library(tidyverse)

# Seperate Order.Date into "Month", "Day", "Year", and subset four features
new_superstore_year <- separate(new_superstore, Order.Date, c("Month", "Day", "Year"), sep = "/")
new_superstore_year
year_data <- subset(new_superstore_year, select = c("Month", "Year", "Sales", "Profit"))
year_data

# Annual Sales & Annual Profit
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
barplot(tapply(year_data$Sales, year_data$Year, FUN = sum), main = "Annual Sales", ylim = c(0, 2000000), col = "burlywood1")
barplot(tapply(year_data$Profit, year_data$Year, FUN = sum), main = "Annual Profit", ylim = c(0, 200000), col = "burlywood1")
labelyear <- c('2011','2012','2013','2014')
plot(tapply(year_data$Sales, year_data$Year, FUN = sum), pch = 15, lty = 1, lwd = 2, 
     col="coral", type = "b", xlab = "Year", ylab = "Total Sales", xaxt = "n")
text(tapply(year_data$Sales, year_data$Year, FUN = sum), labelyear)
plot(tapply(year_data$Profit, year_data$Year, FUN = sum), pch = 15, lty = 1, lwd = 2, 
     col="coral", type = "b", xlab = "Year", ylab = "Total Profit", xaxt = "n")
text(tapply(year_data$Profit, year_data$Year, FUN = sum), labelyear)
par(opar)

# Ordering by Month
year_data_sales <- aggregate(Sales ~ Month + Year, year_data, FUN = sum)
i <- stringr::str_order(year_data_sales$Month, numeric = TRUE)
year_data_sales <- year_data_sales[i, ]
year_data_sales
year_data_profit <- aggregate(Profit ~ Month + Year, year_data, FUN = sum)
j <- stringr::str_order(year_data_profit$Month, numeric = TRUE)
year_data_profit <- year_data_profit[j, ]
year_data_profit

# 2011-2014 ARIMA Model
data_SaleSellected <- new_superstore[, c("Order.Date", "Sales", "Profit")]
head(data_SaleSellected)
unique(grepl('/',data_SaleSellected$Order.Date))
unique(grepl('-',data_SaleSellected$Order.Date))
# Convert Date column to Date fromat
data_SaleSellected$Date1 = as.Date(data_SaleSellected$Order.Date, format = "%m-%d-%Y")
data_SaleSellected$Date2 = as.Date(data_SaleSellected$Order.Date, format = "%m/%d/%Y")
data_SaleSellected$Date = data_SaleSellected$Date2
head(data_SaleSellected)
data_SaleSellected[is.na(Date)]$Date = data_SaleSellected[is.na(Date)]$Date1
nrow(data_SaleSellected[is.na(Date)])
data_SaleSellected[is.na(Date)]
data_SaleSellected$Date1 = NULL
data_SaleSellected$Date2 = NULL
data_SaleSellected$Order.Date = NULL
head(data_SaleSellected, 10)
# Generate Time series
data_SaleSellected = data_SaleSellected[order(as.Date(data_SaleSellected$Date)),]
head(data_SaleSellected, 10)
# Generate Month
data_SaleSellected$YM = substr(data_SaleSellected$Date, 1, 7)
head(data_SaleSellected, 10)
# group by sale and profit per month
data_SaleSellected = data.table(data_SaleSellected)
Monthly_sale = data_SaleSellected[,.(Sale = sum(Sales)), by = YM]
head(Monthly_sale, 12)
Monthly_Profit = data_SaleSellected[,.(Profit = sum(Profit)), by = YM]
head(Monthly_Profit, 12)
SalePrice = Monthly_sale$Sale
head(SalePrice, 10)
ProfitPrice = Monthly_Profit$Profit
head(ProfitPrice, 10)
# Generate time series object with 12 frequency (month)
tsSale = ts(SalePrice, start = c(2011, 1), end = c(2014, 12), frequency = 12)
min(Monthly_sale$YM)
max(Monthly_sale$YM)
tsSale
tsProfit = ts(ProfitPrice, start = c(2011, 1), end = c(2014, 12), frequency = 12)
min(Monthly_Profit$YM)
max(Monthly_Profit$YM)
tsProfit

# Automatically create ARIMA model
sale_model <- auto.arima(tsSale)
sale_model
accuracy(sale_model)
# Create a Simple Plot with a forecast for the next year
plot(forecast(sale_model, 12), xlab = "Date", ylab = "Sale", main = "ARIMA Forecast for Sale")
#get forcast value 
pred_values1 <- forecast(sale_model, 12)
pred_values1

# 2011-2014, Monthly Sales
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
year2011s <- year_data_sales[year_data_sales$Year == '2011',]
year2012s <- year_data_sales[year_data_sales$Year == '2012',]
year2013s <- year_data_sales[year_data_sales$Year == '2013',]
year2014s <- year_data_sales[year_data_sales$Year == '2014',]
plot(year2011s$Month, year2011s$Sales, type = "b", ylim = c(0, 120000), xlab = "Month", ylab = "Sales", main = "2011 Monthly Sales", pch = 19, col = c("red"))
plot(year2012s$Month, year2012s$Sales, type = "b", xlab = "Month", ylab = "Sales", main = "2012 Monthly Sales", pch = 19, col = c("dodgerblue2"))
plot(year2013s$Month, year2013s$Sales, type = "b", xlab = "Month", ylab = "Sales", main = "2013 Monthly Sales", pch = 19, col = c("lightskyblue4"))
plot(year2014s$Month, year2014s$Sales, type = "b", xlab = "Month", ylab = "Sales", main = "2014 Monthly Sales", pch = 19, col = c("green"))
par(opar)

# Create a Season Plot
seasonplot(tsSale, year.labels = "TRUE", main = " Season Plot of Sales by Month", 
           xlab = "Month", ylab = "Sale", 
           col = c("red", "dodgerblue2", "lightskyblue4", "green"),
           lwd = 2)

# Automatically create ARIMA model
profit_model <- auto.arima(tsProfit, D = 1)
profit_model
accuracy(profit_model)
# Create a Simple Plot with a forecast for the next year
plot(forecast(profit_model, 12), xlab = "Date", ylab = "Profit", main = "ARIMA Forecast for Profit")
#get forcast value 
pred_values2 <- forecast(profit_model, 12)
pred_values2


# 2011-2014, Monthly Profit
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
year2011p <- year_data_profit[year_data_profit$Year == '2011',]
year2012p <- year_data_profit[year_data_profit$Year == '2012',]
year2013p <- year_data_profit[year_data_profit$Year == '2013',]
year2014p <- year_data_profit[year_data_profit$Year == '2014',]
plot(year2011p$Month, year2011p$Profit, type = "b", xlab = "Month", ylab = "Profit", main = "2011 Monthly Profit", pch = 19, col = c("red"))
plot(year2012p$Month, year2012p$Profit, type = "b", xlab = "Month", ylab = "Profit", main = "2012 Monthly Profit", pch = 19, col = c("orange"))
plot(year2013p$Month, year2013p$Profit, type = "b", xlab = "Month", ylab = "Profit", main = "2013 Monthly Profit", pch = 19, col = c("gray"))
plot(year2014p$Month, year2014p$Profit, type = "b", xlab = "Month", ylab = "Profit", main = "2014 Monthly Profit", pch = 19, col = c("green"))
par(opar)

# Create a Season Plot
seasonplot(tsProfit, year.labels = "TRUE", main = " Season Plot of Profit by Month", 
           xlab = "Month", ylab = "Profit", 
           col = c("red", "orange", "gray", "green"),
           lwd = 2)


detach(new_superstore)

