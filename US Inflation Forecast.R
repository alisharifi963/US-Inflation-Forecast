# Load required libraries
library(forecast)
library(ggplot2)
library(zoo)

# Create sample monthly data from 2000 to 2023
dates <- seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = "month")
n <- length(dates)

# Create sample time series for inflation (replace this with actual data)
set.seed(123)
inflation_data <- ts(rnorm(n, mean = 2.5, sd = 0.5), frequency = 12, start = c(2000, 1))

# Convert to time series object
ts_inflation <- ts(inflation_data, frequency = 12)

# Fit ARIMA model
model_fit <- auto.arima(ts_inflation, seasonal = TRUE)

# Print model summary
summary(model_fit)

# Forecast for next 12 months
forecast_values <- forecast(model_fit, h = 12)

# Calculate errors
accuracy_metrics <- accuracy(forecast_values)

# Create plot with ggplot2
# Convert data to dataframe for ggplot
actual_df <- data.frame(
  date = dates,
  value = as.vector(ts_inflation),
  type = "Actual"
)

forecast_df <- data.frame(
  date = seq(max(dates), by = "month", length.out = 13)[-1],
  value = as.vector(forecast_values$mean),
  type = "Forecast"
)

# Combine actual and forecast data
plot_data <- rbind(actual_df, forecast_df)

# Create plot
ggplot(plot_data, aes(x = date, y = value, color = type)) +
  geom_line() +
  geom_ribbon(data = forecast_df,
              aes(ymin = as.vector(forecast_values$lower[,"95%"]),
                  ymax = as.vector(forecast_values$upper[,"95%"]),
                  fill = type),
              alpha = 0.2) +
  theme_minimal() +
  labs(title = "US Inflation Rate - Actual and Forecast",
       x = "Date",
       y = "Inflation Rate (%)",
       color = "Data Type") +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  scale_fill_manual(values = c("Actual" = NA, "Forecast" = "red"))

# Print forecast results
print("Forecast for next 12 months:")
print(forecast_values)

# Print accuracy metrics
print("Model accuracy metrics:")
print(accuracy_metrics)