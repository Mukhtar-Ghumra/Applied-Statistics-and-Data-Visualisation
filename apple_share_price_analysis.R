# Install and load necessary libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")
if (!require(TSA)) install.packages("TSA")
if (!require(xts)) install.packages("xts")

library(ggplot2)
library(forecast)
library(tseries)
library(TSA)
library(xts)

# Load dataset
data <- read.csv("AAPL.csv")

# View the first few rows of the data
head(data)

# Check data structure
str(data)
summary(data)







# Convert Date column to Date format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Enhanced visualization of Close prices over time
ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color = "steelblue", size = 1.2) +                  # Smooth line with appealing color and thickness
  geom_point(color = "darkblue", size = 2, alpha = 0.8) +       # Highlight data points
  geom_smooth(method = "loess", color = "darkred", size = 1,    # Smooth trend line with distinct style
              linetype = "dotted", se = TRUE, alpha = 0.2) +    # Add confidence interval
  ggtitle("AAPL Close Prices Over Time") +                     # Title
  xlab("Year") +                                               # X-axis label
  ylab("Close Price (USD)") +                                  # Y-axis label
  scale_x_date(date_labels = "%Y",                             # Format x-axis with year
               date_breaks = "3 years") +                      # Add more frequent breaks
  theme_minimal(base_size = 14) +                              # Clean and modern theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "darkblue"), # Centered title with color
    axis.title = element_text(size = 16, face = "bold"),                                 # Bold axis titles
    axis.text = element_text(size = 14),                                                # Larger axis text
    panel.grid.major = element_line(color = "grey80", size = 0.5),                      # Subtle major grid
    panel.grid.minor = element_blank(),                                                 # Remove minor grid
    panel.background = element_rect(fill = "white", color = "grey85"),                  # Light background and border
    plot.background = element_rect(fill = "ivory"),                                     # Soft ivory background
    plot.margin = margin(10, 10, 10, 10)                                                # Add space around the plot
  )










library(ggplot2)
library(forecast)
library(scales)

# time series object for Close prices
close_prices <- ts(data$Close, start = c(2000, 1), frequency = 252)  # 252 trading days/year

# visualization of time series
autoplot(close_prices, ts.colour = "blue", size = 1) +  # Set color and line thickness
  geom_smooth(method = "loess", color = "red", linetype = "dashed", size = 1, se = FALSE) +  # Add a trend line
  ggtitle("Time Series of AAPL Close Prices (2000 Onward)") +
  xlab("Year") +
  ylab("Close Price (USD)") +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +  # Customize year breaks
  theme_minimal() +                                      # Apply a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Center and bold title
    axis.title = element_text(size = 14),                            # Adjust axis title font size
    axis.text = element_text(size = 12),                             # Adjust axis text font size
    panel.grid.major = element_line(color = "grey80", size = 0.5),   # Light grid lines
    panel.grid.minor = element_blank(),                              # Remove minor grid lines
    panel.background = element_rect(fill = "white", color = NA),     # White background
    plot.background = element_rect(fill = "white", color = NA)       # White plot background
  )









# Load necessary libraries
library(ggplot2)
library(forecast)

# Step 1: Create a time series object for Close prices
# Assuming the `data` dataframe contains columns 'Close' and the prices are daily.
close_prices <- ts(data$Close, start = c(2000, 1), frequency = 252)  # 252 trading days/year

# Step 2: Decompose the time series into trend, seasonal, and random components
# Using the 'multiplicative' decomposition method
decomposed <- decompose(close_prices, type = "multiplicative")

# Step 3: Plot the decomposed components
# This will show the original series, trend, seasonal, and random components in a single plot
plot(decomposed)

# Step 4: Extract individual components from the decomposition
trend_component <- decomposed$trend      # Extract the trend component
seasonal_component <- decomposed$seasonal  # Extract the seasonal component
random_component <- decomposed$random    # Extract the random (residual) component

# Step 5: Print insights about the components
# Describe what each component represents
cat("Insights into the decomposition:\n")
cat("- The trend component reveals the long-term movement of AAPL Close Prices.\n")
cat("- The seasonal component indicates regular fluctuations or recurring patterns in the data.\n")
cat("- The random component captures any remaining variability after removing trend and seasonality.\n")

# Step 6: Optional - View a summary of each component
cat("\nSummary of the trend component:\n")
print(summary(trend_component))

cat("\nSummary of the seasonal component:\n")
print(summary(seasonal_component))

cat("\nSummary of the random component:\n")
print(summary(random_component))










# Load necessary libraries
library(ggplot2)
library(forecast)
library(tseries)

# Perform the Augmented Dickey-Fuller test
adf_test <- adf.test(close_prices, alternative = "stationary")
print(adf_test)

# Apply differencing 
if (adf_test$p.value > 0.05) {
  close_prices_diff <- diff(close_prices)
  
  # Plot the differenced time series with a simple, plain white theme
  autoplot(close_prices_diff) +
    geom_line(color = "darkorange", size = 1.5) +  # Vibrant orange for the line
    ggtitle("Differenced Time Series of AAPL Close Prices") +
    xlab("Year") +
    ylab("Differenced Close Price (USD)") +
    theme_minimal() +  # Apply a minimal theme for a clean, simple look
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "black"),  # Bold black title
      axis.title = element_text(size = 14, color = "black"),                             # Black axis titles
      axis.text = element_text(size = 12, color = "black"),                              # Black axis labels
      panel.grid.major = element_line(color = "grey90", size = 0.5),                     # Subtle grey grid lines
      panel.grid.minor = element_blank(),                                                # Remove minor grid lines
      plot.background = element_rect(fill = "white", color = NA),                        # White background
      panel.background = element_rect(fill = "white", color = NA),                       # White panel background
      plot.margin = margin(10, 10, 10, 10)                                               # Add a small margin
    )
}











# Automatically fit ARIMA model
arima_model <- auto.arima(close_prices)
summary(arima_model)

# Residual diagnostics
checkresiduals(arima_model)

# Plot ARIMA forecast
arima_forecast <- forecast(arima_model, h = 30)
autoplot(arima_forecast) +
  ggtitle("ARIMA Model Forecast") +
  xlab("Year") +
  ylab("Close Price (USD)")




# Fit an ETS model
ets_model <- ets(close_prices)
summary(ets_model)

# Residual diagnostics
checkresiduals(ets_model)

# Plot ETS forecast
ets_forecast <- forecast(ets_model, h = 30)
autoplot(ets_forecast) +
  ggtitle("ETS Model Forecast") +
  xlab("Year") +
  ylab("Close Price (USD)")




# Fit a TBATS model
tbats_model <- tbats(close_prices)
summary(tbats_model)

# Residual diagnostics
checkresiduals(tbats_model)

# Plot TBATS forecast
tbats_forecast <- forecast(tbats_model, h = 30)
autoplot(tbats_forecast) +
  ggtitle("TBATS Model Forecast") +
  xlab("Year") +
  ylab("Close Price (USD)")







# Load necessary libraries
library(forecast)

# Example data preparation (replace with actual time series data)
data <- data.frame(
  Date = seq.Date(from = as.Date("2000-01-01"), to = as.Date("2020-01-01"), by = "year"),
  Close = cumsum(rnorm(21, mean = 5, sd = 2))
)
close_prices <- ts(data$Close, start = c(2000, 1), frequency = 1)  # Annual frequency for example

# ARIMA, ETS, and TBATS models
arima_model <- auto.arima(close_prices)
ets_model <- ets(close_prices)
tbats_model <- tbats(close_prices)

# Forecasts for the next 5 periods
arima_forecast <- forecast(arima_model, h = 5)
ets_forecast <- forecast(ets_model, h = 5)
tbats_forecast <- forecast(tbats_model, h = 5)

# Compare accuracy of the models
arima_accuracy <- accuracy(arima_forecast)
ets_accuracy <- accuracy(ets_forecast)
tbats_accuracy <- accuracy(tbats_forecast)

# Organize accuracy metrics into a data frame
accuracy_table <- data.frame(
  Model = c("ARIMA", "ETS", "TBATS"),
  ME = c(arima_accuracy[1, "ME"], ets_accuracy[1, "ME"], tbats_accuracy[1, "ME"]),
  RMSE = c(arima_accuracy[1, "RMSE"], ets_accuracy[1, "RMSE"], tbats_accuracy[1, "RMSE"]),
  MAE = c(arima_accuracy[1, "MAE"], ets_accuracy[1, "MAE"], tbats_accuracy[1, "MAE"]),
  MAPE = c(arima_accuracy[1, "MAPE"], ets_accuracy[1, "MAPE"], tbats_accuracy[1, "MAPE"]),
  ACF1 = c(arima_accuracy[1, "ACF1"], ets_accuracy[1, "ACF1"], tbats_accuracy[1, "ACF1"])
)

# Print the accuracy table
print("Model Accuracy Comparison:")
print(accuracy_table)

# Visualization of Accuracy Metrics
library(ggplot2)

# Reshape data for plotting
library(reshape2)
accuracy_melted <- melt(accuracy_table, id.vars = "Model", variable.name = "Metric", value.name = "Value")

# Plot accuracy metrics
ggplot(accuracy_melted, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Model Accuracy Metrics Comparison") +
  xlab("Metric") +
  ylab("Value") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )









# Load necessary libraries
library(ggplot2)
library(forecast)

# Example data preparation (replace with actual time series data)
data <- data.frame(
  Date = seq.Date(from = as.Date("2000-01-01"), to = as.Date("2020-01-01"), by = "year"),
  Close = cumsum(rnorm(21, mean = 5, sd = 2))
)
close_prices <- ts(data$Close, start = c(2000, 1), frequency = 1)  # Annual frequency for example

# ARIMA, ETS, and TBATS forecasts
arima_model <- auto.arima(close_prices)
ets_model <- ets(close_prices)
tbats_model <- tbats(close_prices)

arima_forecast <- forecast(arima_model, h = 5)
ets_forecast <- forecast(ets_model, h = 5)
tbats_forecast <- forecast(tbats_model, h = 5)

# Convert forecast data into a data frame for clearer plotting
forecast_data <- data.frame(
  Year = c(time(arima_forecast$mean), time(ets_forecast$mean), time(tbats_forecast$mean)),
  Forecast = c(as.numeric(arima_forecast$mean), as.numeric(ets_forecast$mean), as.numeric(tbats_forecast$mean)),
  Model = rep(c("ARIMA", "ETS", "TBATS"), each = length(arima_forecast$mean))
)

# Combine observed data for plotting
observed_data <- data.frame(
  Year = time(close_prices),
  Close = as.numeric(close_prices)
)

# Plot observed data and forecasts
ggplot() +
  geom_line(data = observed_data, aes(x = Year, y = Close, color = "Observed"), size = 1.2) +
  geom_line(data = forecast_data, aes(x = Year, y = Forecast, color = Model), size = 1.2) +
  ggtitle("Comparison of ARIMA, ETS, and TBATS Forecasts") +
  xlab("Year") +
  ylab("Close Price (USD)") +
  scale_color_manual(
    values = c("Observed" = "black", "ARIMA" = "red", "ETS" = "blue", "TBATS" = "green"),
    name = "Series"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "darkblue"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

