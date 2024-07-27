# IMPORTING NECCESSARY LIBRARIES
library(forecast)
library(tidyverse)
library(patchwork)

# IMPORTING DATASET
daily_rainfall_wb <- read.csv(file.choose(), header = TRUE)
daily_rainfall_wb$MONTH <- factor(daily_rainfall_wb$MONTH, levels = c("Jan", "Feb", "Mar","Apr", "May", "Jun", 
                                                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# MAKING JUNE DATA
june_rainfall_wb <- daily_rainfall_wb[daily_rainfall_wb$MONTH == "Jun", ]

# MAKING YEARLY JUNE DATA FOR 120 YEARS
june_yearly_wb <- june_rainfall_wb %>% 
  group_by(YEAR) %>% summarise(TOTAL.RAINFALL = sum(RAINFALL.ON.DAY))

# MAKING YEARLY JUNE DATA FOR THE YEARS 1991 - 2020
june_1991_20_wb <- june_yearly_wb[june_yearly_wb$YEAR >= 1991, ]

# FITTING ARIMA MODEL AND FORECAST RAINFALL FOR 2021 - 2030
arima_model <- auto.arima(june_1991_20_wb$TOTAL.RAINFALL, xreg = june_1991_20_wb$YEAR)

# GENERATE FITTED VALUES FOR HISTORICAL DATA
fitted_values <- fitted(arima_model)

# ADDING FITTED VALUES TO HISTORICAL DATA
june_1991_20_wb <- june_1991_20_wb %>%
  mutate(FITTED = fitted_values)

# FORECASTING FUTURE VALUES
arima_forecast <- forecast(arima_model, xreg = c(2021:2030))

# PREPARING FORECAST DATA
forecast_data <- data.frame(
  YEAR = c(2021:2030),
  TOTAL.RAINFALL = arima_forecast$mean,
  UPPER = arima_forecast$upper[,2],  # Taking the 95% confidence interval
  LOWER = arima_forecast$lower[,2]   # Taking the 95% confidence interval
)

# PLOTTING BOTH HISTORICAL AND FORECASTED VALUES
ggplot() + 
  geom_point(data = june_1991_20_wb, aes(x = YEAR, y = TOTAL.RAINFALL), color = "blue") + 
  geom_line(data = june_1991_20_wb, aes(x = YEAR, y = FITTED), color = "red") +
  geom_line(data = forecast_data, aes(x = YEAR, y = TOTAL.RAINFALL), color = "red") +
  geom_ribbon(data = forecast_data, aes(x = YEAR, ymin = LOWER, ymax = UPPER), alpha = 0.2, fill = "red") +
  annotate("text", x = 2005, y = 448, label = "Historical Data", color = "blue") +
  annotate("text", x = 2025, y = 448, label = "Forecasted Data", color = "red") +
  labs(title = "Historical and Forecasted Rainfall Values from 1991 - 2030", x = "Year", y = "Total Rainfall") + 
  theme_minimal()