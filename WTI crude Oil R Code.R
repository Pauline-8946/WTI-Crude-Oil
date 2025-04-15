# load packages
library(quantmod)
library(ggplot2)
library(tseries)
library(forecast)

# Specify the ticker symbol for WTI Crude Oil
oil_symbol <- "CL=F"

# Download historical oil price data from Yahoo Finance
getSymbols(oil_symbol, src = "yahoo", from = "2019-01-01", to = Sys.Date())

# View the structure of the loaded data
str(get(oil_symbol))

# Assuming 'oil_symbol' is an xts object with OHLCV data
# Extract the closing prices
closing_prices <- Cl(get(oil_symbol))

# Use summary to get summary statistics
summary(closing_prices)

# Additional individual summary statistics
mean_value <- mean(closing_prices)
median_value <- median(closing_prices)
sd_value <- sd(closing_prices)
min_value <- min(closing_prices)
max_value <- max(closing_prices)

# Display the results
cat("Mean:", mean_value, "\n")
cat("Median:", median_value, "\n")
cat("Standard Deviation:", sd_value, "\n")
cat("Minimum:", min_value, "\n")
cat("Maximum:", max_value, "\n")

head(closing_prices)
closing_prices

# Time Series plots
# Assuming 'closing_prices' is your xts object
plot(closing_prices, type = "l", col = "blue", lwd = 2,
     main = "WTI Crude Oil Closing Prices Over Time",
     xlab = "Date", ylab = "Closing Price")

# Time Series plots
# Assuming 'closing_prices' is your xts object
plot(index(closing_prices), coredata(closing_prices), type = "l", col = "blue", lwd = 2,
     main = "WTI Crude Oil Closing Prices Over Time",
     xlab = "Date", ylab = "Closing Price")


# Histogram
# Assuming 'closing_prices' is your xts object
# Convert xts to data frame for ggplot
closing_prices_df <- data.frame(Close = as.numeric(closing_prices))

# Create histogram with ggplot2
ggplot(closing_prices_df, aes(x = Close)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of WTI Crude Oil Closing Prices", x = "Closing Price", y = "Frequency")

# Box plot
# Assuming 'closing_prices' is your xts object
boxplot(as.numeric(closing_prices), col = "skyblue", main = "Boxplot of WTI Crude Oil Closing Prices", ylab = "Closing Price")

# Test if series is stationary, it's not
adf.test(closing_prices)
kpss.test(closing_prices)

# hold on
#######
# Check the length of your time series, it's saying it's not periodic
# not necessary? as we're not decomposing
length(closing_prices)

sum(is.na(closing_prices))

sum(is.infinite(closing_prices) | is.nan(closing_prices))

plot(closing_prices, main = "Closing Prices Over Time")


########
# check this
# Forecast future values
forecast_values <- forecast(arima_model, h = 10)  # Adjust the horizon as needed
plot(forecast_values, main = "ARIMA Forecast")


# DIFFERENCING
# Assuming 'closing_prices' is your time series data
differenced_series <- diff(closing_prices)
plot(differenced_series)

# Check for missing values
any(is.na(differenced_series)) # returns TRUE

# Assuming 'differenced_series' is your differenced time series
count_missing <- sum(is.na(differenced_series))

# Print the count, 1 missing
print(count_missing)

# Assuming 'differenced_series' is your differenced time series
indices_missing <- which(is.na(differenced_series))

# Print the indices
print(indices_missing)
# Remove the missing value at index 1
differenced_series_no_na <- na.omit(differenced_series)


# Compute ACF for the differenced series without missing values
acf_result_diff <- acf(differenced_series_no_na, lag.max = 20, main="ACF for the differenced series")
pacf_result_diff <- pacf(differenced_series_no_na, lag.max = 20, main="PACF for the differnced series")

#######
# doesn't work, maybe omit?
# Assuming 'differenced_series' is the differenced time series data
acf_result_diff <- acf(differenced_series, lag.max = 20) #doesn't work
pacf_result_diff <- pacf(differenced_series, lag.max = 20)

# same as for the above
# Plot ACF of differenced series
plot(acf_result_diff, main = "ACF of Differenced Series")

# Plot PACF of differenced series
plot(pacf_result_diff, main = "PACF of Differenced Series")





# Fitting an ARIMA model
# Assuming 'differenced_series' is your differenced time series
arima_model <- arima(differenced_series, order = c(2,1,2)) #change to (2, 1, 2)
#extract residuals
model_residuals <- resid(arima_model)

# Remove missing values from residuals
clean_residuals <- na.omit(model_residuals)

# Plot ACF for cleaned residuals
acf(clean_residuals, lag.max = 20)
print(acf(clean_residuals))

# Forecast future values
forecast_values <- forecast(arima_model, h = 20)  # Adjust the horizon as needed
plot(forecast_values, main = "ARIMA Forecast")

# Example of Ljung-Box test for residuals
box_ljung_test <- Box.test(model_residuals, lag = 20, type = "Ljung-Box")

print(box_ljung_test)



# CRoss Validation
# 1. Split your data
# Assuming 'differenced_series' is your differenced time series
n <- length(differenced_series)
train_size <- floor(0.8 * n)
train_data <- head(differenced_series, train_size)
test_data <- tail(differenced_series, n - train_size)

# This process is repeated iteratively, often in a rolling or 
# expanding window fashion, to assess the model's robustness across different time periods.

# Fit the Model on Training Data
# Assuming 'arima_order' is your chosen order (e.g., c(2,1,2))
arima_model <- arima(train_data, order = c(2,1,4))

# Forecast on Test Data:
forecast_values <- forecast(arima_model, h = length(test_data))
#print(forecast_values)

# Evaluate the Forecast
actual_values <- as.numeric(test_data)
forecast_errors <- actual_values - forecast_values$mean

# Calculate evaluation metrics
mae <- mean(abs(forecast_errors))
mse <- mean(forecast_errors^2)
rmse <- sqrt(mse)

# Print or store the evaluation metrics
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

forecast_values_original <- cumsum(forecast_values$mean)



# Visualize Results:
plot(test_data_numeric, type = "l", col = "blue", ylim = range(c(test_data_numeric, forecast_values_numeric)))
lines(forecast_values_numeric, col = "red")

# actually wrong
# Visualize Results:
plot(test_data, type = "l", col = "blue", ylim = range(c(test_data, forecast_values$mean)))
lines(forecast_values$mean, col = "red")
class(test_data)
class(forecast_values$mean)


# Repeat for Cross-Validation
# Example of a rolling window cross-validation
for (i in seq_along(test_data)) {
  current_train_data <- head(differenced_series, train_size + i)
  current_test_data <- tail(differenced_series, n - train_size - i)
  
  # Fit model on current training data
  current_arima_model <- arima(current_train_data, order = c(2,1,2))
  
  # Forecast on current test data
  current_forecast <- forecast(current_arima_model, h = 1)
  
  # Evaluate the forecast and store results
  # (Similar to steps 4 and 5)
}
# Initialize vectors to store results
mae_values <- numeric(length(test_data))
mse_values <- numeric(length(test_data))
rmse_values <- numeric(length(test_data))

# Perform rolling window cross-validation
for (i in seq_along(test_data)) {
  current_train_data <- head(differenced_series, train_size + i)
  current_test_data <- tail(differenced_series, n - train_size - i)
  
  # Fit model on current training data
  current_arima_model <- arima(current_train_data, order = c(2, 1, 2))
  
  # Forecast on current test data
  current_forecast <- forecast(current_arima_model, h = 1)
  
  # Extract actual values for the current test period
  actual_values <- as.numeric(current_test_data[1])
  
  # Calculate forecast errors
  forecast_errors <- actual_values - current_forecast$mean
  
  # Calculate evaluation metrics
  mae_values[i] <- mean(abs(forecast_errors))
  mse_values[i] <- mean(forecast_errors^2)
  rmse_values[i] <- sqrt(mse_values[i])
}

# Print or store the evaluation metrics
cat("Mean Absolute Error (MAE):", mean(mae_values), "\n")
cat("Mean Squared Error (MSE):", mean(mse_values), "\n")
cat("Root Mean Squared Error (RMSE):", mean(rmse_values), "\n")

# Forecast on Test Data:
forecast_values <- forecast(arima_model, h = length(test_data))

#testing with non-differenced data
# Assuming 'differenced_series' is your differenced time series data
n <- length(differenced_series)
train_size <- floor(0.8 * n)

# Perform cross-validation using the actual time series
# Perform cross-validation using the actual time series
for (i in seq_along(test_data)) {
  current_train_data <- head(closing_prices, train_size + i)
  current_test_data <- tail(closing_prices, n - train_size - i)
  
  # Fit model on current training data
  current_arima_model <- arima(current_train_data, order = c(2,1,2))
  
  # Forecast on current test data
  current_forecast <- forecast(current_arima_model, h = 1)
  
  # Extract relevant parts for evaluation
  actual_values <- as.numeric(tail(current_test_data, 1))
  forecast_values <- as.numeric(current_forecast$mean)
  
  # Evaluate the forecast and store results
  forecast_errors <- actual_values - forecast_values
  
  # Calculate evaluation metrics
  mae <- mean(abs(forecast_errors))
  mse <- mean(forecast_errors^2)
  rmse <- sqrt(mse)
  
  # Print or store the evaluation metrics
  cat("MAE:", mae, "\n")
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  
  # Visualize Results
  plot(index(current_test_data), coredata(current_test_data), type = "l", col = "blue", ylim = range(c(coredata(current_test_data), forecast_values)))
  lines(index(forecast_values), forecast_values, col = "red")
  
  
}
# Set the desired number of iterations
num_iterations <- length(test_data)

# Loop through the test data
for (i in seq_along(test_data)) {
  current_train_data <- head(differenced_series, train_size + i)
  current_test_data <- tail(differenced_series, n - train_size - i)
  
  # Fit model on current training data
  current_arima_model <- arima(current_train_data, order = c(2,1,2))
  
  # Forecast on current test data
  current_forecast <- forecast(current_arima_model, h = 1)
  
  # Evaluate the forecast and store results
  actual_values <- as.numeric(current_test_data)
  forecast_values <- current_forecast$mean
  
  # Ensure the lengths match
  min_length <- min(length(actual_values), length(forecast_values))
  actual_values <- actual_values[1:min_length]
  forecast_values <- forecast_values[1:min_length]
  
  # Calculate evaluation metrics
  mae <- mean(abs(actual_values - forecast_values))
  mse <- mean((actual_values - forecast_values)^2)
  rmse <- sqrt(mse)
  
  # Print or store the evaluation metrics
  cat("MAE:", mae, "\n")
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  
  # Assuming 'current_test_data' is your test data and 'current_forecast' is your forecast
  plot(current_test_data, type = "l", col = "blue", ylim = range(c(current_test_data, current_forecast$mean)))
  lines(current_forecast$mean, col = "red")
  
  
  # Check if we reached the desired number of iterations
  if (i == num_iterations) {
    break
  }
}

# Assuming 'closing_prices' is your original time series data

# Specify the size of the training set (80% of the data)
n <- length(closing_prices)
train_size <- floor(0.8 * n)

# Perform rolling-window cross-validation
for (i in seq_along(test_data)) {
  # Extract the current training and test data
  current_train_data <- head(closing_prices, train_size + i)
  current_test_data <- tail(closing_prices, n - train_size - i)
  
  # Fit an ARIMA model on the current training data
  current_arima_model <- arima(current_train_data, order = c(2,1,2))
  
  # Forecast the next observation on the current test data
  current_forecast <- forecast(current_arima_model, h = 1)
  
  # Extract actual and forecast values
  actual_values <- coredata(tail(current_test_data, 1))
  forecast_values <- as.numeric(current_forecast$mean)
  
  # Evaluate the forecast and store results
  forecast_errors <- actual_values - forecast_values
  mae <- mean(abs(forecast_errors))
  mse <- mean(forecast_errors^2)
  rmse <- sqrt(mse)
  
  # Print or store the evaluation metrics
  cat("MAE:", mae, "\n")
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  
  # Visualize Results
  plot(index(current_test_data), coredata(current_test_data), type = "l", col = "blue", ylim = range(c(coredata(current_test_data), forecast_values)))
  lines(index(current_test_data) + length(current_train_data), forecast_values, col = "red")
}

# Assuming 'closing_prices' is your original time series data

# Specify the size of the training set (80% of the data)
n <- length(closing_prices)
train_size <- floor(0.8 * n)

# Perform rolling-window cross-validation
for (i in seq_along(test_data)) {
  # Extract the current training and test data
  current_train_data <- head(closing_prices, train_size + i)
  current_test_data <- tail(closing_prices, n - train_size - i)
  
  # Fit an ARIMA model on the current training data
  current_arima_model <- arima(current_train_data, order = c(2,1,2))
  
  # Forecast the next observation on the current test data
  current_forecast <- forecast(current_arima_model, h = 1)
  
  # Extract actual and forecast values
  actual_values <- coredata(tail(current_test_data, 1))
  forecast_values <- as.numeric(current_forecast$mean)
  
  # Evaluate the forecast and store results
  forecast_errors <- actual_values - forecast_values
  mae <- mean(abs(forecast_errors))
  mse <- mean(forecast_errors^2)
  rmse <- sqrt(mse)
  
  # Print or store the evaluation metrics
  cat("MAE:", mae, "\n")
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  
  # Visualize Results
  plot(index(current_test_data), coredata(current_test_data), type = "l", col = "blue", ylim = range(c(coredata(current_test_data), forecast_values)))
  lines(index(tail(current_test_data, 1)) + 1, forecast_values, col = "red")
}

# Assuming 'closing_prices' is your original time series data

# Specify the size of the training set (80% of the data)
n <- length(closing_prices)
train_size <- floor(0.8 * n)

# useless
# Perform rolling-window cross-validation
for (i in seq_along(test_data)) {
  # Extract the current training and test data
  current_train_data <- head(closing_prices, train_size + i)
  current_test_data <- tail(closing_prices, n - train_size - i)
  
  # Fit an ARIMA model on the current training data
  current_arima_model <- arima(current_train_data, order = c(2,1,2))
  
  # Forecast the next observation on the current test data
  current_forecast <- forecast(current_arima_model, h = 1)
  
  # Extract actual and forecast values
  actual_values <- tail(coredata(current_test_data), 1)
  forecast_values <- as.numeric(current_forecast$mean)
  
  # Evaluate the forecast and store results
  forecast_errors <- actual_values - forecast_values
  mae <- mean(abs(forecast_errors))
  mse <- mean(forecast_errors^2)
  rmse <- sqrt(mse)
  
  # Print or store the evaluation metrics
  cat("MAE:", mae, "\n")
  cat("MSE:", mse, "\n")
  cat("RMSE:", rmse, "\n")
  
  # Visualize Results
  plot(index(current_test_data), coredata(current_test_data), type = "l", col = "blue", ylim = range(c(coredata(current_test_data), forecast_values)))
  lines(index(tail(current_test_data, 1)) + 1, forecast_values, col = "red")
}


# Assuming 'closing_prices' is your original time series data

# Specify the size of the training set (80% of the data)
n <- length(closing_prices)
train_size <- floor(0.8 * n)

# Extract the training data
train_data <- head(closing_prices, train_size)

# Fit an ARIMA model on the training data
arima_model <- arima(train_data, order = c(2, 1, 2))

# Forecast the next two years
forecast_values <- forecast(arima_model, h = 2 * 365)  # Adjust the horizon as needed

# Extract the forecasted values for the entire period
forecast_values <- forecast(arima_model, h = length(test_data))

# Extract the forecasted values for the period from 2024 to 2026
forecast_values_2024_2026 <- window(forecast_values$mean, start = c(2024, 1), end = c(2026, 12))

# Plot only the forecasted values for 2024 to 2026
plot(forecast_values_2024_2026, type = "l", col = "blue", main = "WTI Crude Oil Price Forecast for 2024-2026", ylab = "Closing Price")
lines(forecast_values_2024_2026, col = "red")

