library(ggplot2)
library(tidyverse)
library(dplyr)

#1
retail <- read.csv("C:/Users/Owner/OneDrive - University of Cincinnati/BANA 4137/US_Retail_sales_2.csv")
data <- data.frame(
  DATE = c("Jan-01", "Feb-01", "Mar-01", "Apr-01", "May-01", "Jun-01", "Jul-01", "Aug-01", "Sep-01", "Oct-01"),
  SALES_millions = c(226791, 223971, 253439, 249062, 268658, 260315, 251504, 266460, 236210, 265188)
)

data$DATE <- as.Date(paste("01-", data$DATE, sep = ""), format = "%d-%b-%y")

ggplot(data, aes(x = DATE, y = SALES_millions)) +
  geom_line() +
  labs(title = "Retail Sales Over Time",
       x = "Date",
       y = "Sales (millions)")


data <- data.frame(
  DATE = seq(as.Date("2020-01-01"), by = "month", length.out = 48),
  SALES_millions = runif(48, 200, 300) 
)

data$Year <- format(data$DATE, "%Y")
data$Month <- format(data$DATE, "%m")

monthly_sales <- aggregate(SALES_millions ~ Year + Month, data, sum)

plot(monthly_sales$SALES_millions, type = "l", xaxt = "n", xlab = "Month", ylab = "Sales (millions)", 
     main = "Seasonal Plot of Retail Sales (Last 4 Years)")

axis(1, at = 1:48, labels = paste(month.abb, rep(2019:2022, each = 12)), las = 2)


#2

rail <- read.csv("C:/Users/Owner/OneDrive - University of Cincinnati/BANA 4137/Rail_safety.csv")

#a)

ggplot(rail, aes(x = Train.miles, y = Injuries)) +
  geom_point() +
  labs(title = "Injuries vs Train Miles",
       x = "Train Miles (millions)",
       y = "Injuries")

#b)
rail$Year <- as.Date(paste0(rail$Year, "-01-01"))

ggplot(rail, aes(x = Year)) +
  geom_line(aes(y = Injuries, color = "Injuries")) +
  geom_line(aes(y = Train.miles, color = "Train Miles (millions)")) +
  geom_line(aes(y = Injuries.per.T.M, color = "Injuries per 100M Miles")) +
  labs(title = "Rail Safety Over Time",
       x = "Year",
       y = "Value") +
  scale_color_manual(values = c("Injuries" = "blue", "Train Miles (millions)" = "red", "Injuries per 100M Miles" = "green"))

#c)
ggplot(rail, aes(x = Year, y = Injuries)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Trend of Injuries Over Time",
       x = "Year",
       y = "Injuries")

#d)
summary_stats <- data.frame(
  Variable = c("Injuries", "Train.miles", "Injuries.per.T.M"),
  Mean = sapply(rail[, c("Injuries", "Train.miles", "Injuries.per.T.M")], mean),
  Median = sapply(rail[, c("Injuries", "Train.miles", "Injuries.per.T.M")], median),
  MAD = sapply(rail[, c("Injuries", "Train.miles", "Injuries.per.T.M")], mad),
  S = sapply(rail[, c("Injuries", "Train.miles", "Injuries.per.T.M")], sd)
)

summary_stats

#3)
electricity <- read.csv("C:/Users/Owner/OneDrive - University of Cincinnati/BANA 4137/Electricity.csv")

#a)
electricity$Period <- as.Date(paste0("01-", electricity$Period), format = "%d-%b-%y")

ggplot(electricity, aes(x = Actual, y = Forecast)) +
  geom_point() +
  labs(title = "Predicted vs Actual Monthly Electricity Consumption",
       x = "Actual Consumption",
       y = "Predicted Consumption")

#b)


ggplot(electricity, aes(x = Actual, y = Forecast)) +
  geom_point() +
  labs(title = "Predicted vs Actual Monthly Electricity Consumption",
       x = "Actual Consumption",
       y = "Predicted Consumption")

correlation <- cor(electricity$Actual, electricity$Forecast)

correlation

#c)
#0.95 is a strong linear positive relationship



#4)
#a)
actual <- c(12.5, 16.3, 11.6, 21.4, 10.3, 12.9, 18.2, 14.9, 12.2, 8.0, 20.0, 18.1, 21.6, 12.7, 9.8, 20.2, 14.8, 19.7, 12.6, 16.6)

mean_actual <- mean(actual)
sd_actual <- sd(actual)

z_score_5th <- (actual[5] - mean_actual) / sd_actual
z_score_5th

#b)
prediction_1 <- c(13.1, 15.4, 11.6, 20.7, 9.1, 11.9, 19.5, 14.2, 14.7, 7.4, 18.1, 19.3, 22.3, 11.4, 10.5, 21.1, 15.6, 22.0, 14.8, 16.8)
prediction_2 <- c(18.0, 13.8, 7.8, 22.1, 11.5, 13.1, 15.0, 14.7, 12.0, 6.3, 24.0, 16.4, 23.4, 15.2, 5.5, 21.2, 20.8, 20.2, 15.9, 16.7)

rmse_model_1 <- sqrt(mean((actual - prediction_1)^2))
rmse_model_1

rmse_model_2 <- sqrt(mean((actual - prediction_2)^2))
rmse_model_2

#c) Model 1 is more accurate as its RMSE is a 1.27 and model 2 is a 2.84. A lower RMSE is better.

#d)
residuals_1 <- actual - prediction_1

se_model_1 <- sd(residuals_1)

forecast_value <- 19.5

z_value <- 1.96

lower_bound <- forecast_value - z_value * se_model_1
upper_bound <- forecast_value + z_value * se_model_1

c(lower_bound, upper_bound)
