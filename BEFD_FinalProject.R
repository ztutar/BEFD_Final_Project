
## ----libraries------------------------------------------------------------------------------------------------------------------
# Load required libraries
library(readxl)     # For reading Excel files
library(lmtest)     # For statistical tests like Durbin-Watson (autocorrelation)
library(forecast)   # For time series analysis and forecasting
library(DIMORA)     # For innovation diffusion models (e.g., Bass, GBM)
library(naniar)     # For visualizing missing data
library(ggplot2)    # For creating plots
library(corrplot)   # For correlation matrix plots
library(car)        # For regression diagnostics
library(dplyr)      # For data manipulation
library(tidyr)      # For data tidying
library(prophet)    # For Facebook Prophet forecasting model
library(MLmetrics)  # For model performance metrics
library(knitr)      # For formatting output in reports

## ----import_monthly-------------------------------------------------------------------------------------------------------------
# Import the monthly data from the Excel file
ott_cinema_monthly  <- read_excel("ott_cinema_dataset.xlsx", sheet = "Monthly")

# Check the structure of the monthly dataset
str(ott_cinema_monthly)

## ----import_annual--------------------------------------------------------------------------------------------------------------
# Import the annual data from the Excel file
ott_cinema_annual <- read_excel("ott_cinema_dataset.xlsx", sheet = "Annual")

# Check the structure of the annual dataset
str(ott_cinema_annual)

## ----missing_visual-------------------------------------------------------------------------------------------------------------
# Find and display rows with missing values in the monthly dataset
ott_cinema_monthly[apply(is.na(ott_cinema_monthly), 1, any), ]  

# Visualize missing values in the monthly dataset
vis_miss(ott_cinema_monthly)

# Find and display rows with missing values in the annual dataset
ott_cinema_annual[apply(is.na(ott_cinema_annual), 1, any), ]  

# Visualize missing values in the annual dataset
vis_miss(ott_cinema_annual)

## ----create_ts------------------------------------------------------------------------------------------------------------------
# Convert the monthly data (excluding the date column) to a time series object
ott_cinema_monthly.ts <- ts(ott_cinema_monthly[,-1], start = c(2006, 1), 
                            end = c(2023, 12), frequency = 12)

# Convert the annual data (excluding the year column) to a time series object
ott_cinema_annual.ts <- ts(ott_cinema_annual[,-1], start = c(2006), 
                           end = c(2023), frequency = 1)

## ----plot_monthly_raw-----------------------------------------------------------------------------------------------------------
# Plot the raw monthly time series data
plot(ott_cinema_monthly.ts, main = "Monthly Cinema-OTT Dataset", cex.lab = 0.45, 
     type = "o", pch = 16, lty = 3, cex = 0.6)

## ----plot_annual_raw------------------------------------------------------------------------------------------------------------
# Plot the raw annual time series data
plot(ott_cinema_annual.ts, main = "Annual Cinema - OTT Dataset", cex.lab = 0.6, 
     type = "o", pch = 16, lty = 3, cex = 0.6, xlab = "")

## ----setup_missing--------------------------------------------------------------------------------------------------------------
# Define the total values for 2013 for key cinema-related variables (to be used for imputation)
total_2013 <- list(
  cinema_screenings = 3014642,             
  cinema_admissions = 106124433,         
  cinema_audience_spending = 732299026.82
)

# Specify the columns that need to be imputed
columns_to_impute <- c("cinema_screenings", "cinema_admissions", 
                       "cinema_audience_spending", "cinema_avg_spending_pp")

# Create a copy of the original time series to apply imputations
cinema_imputed <- ott_cinema_monthly.ts

# Select years around 2013 (excluding 2013) to calculate typical monthly patterns
selected_years <- (time(ott_cinema_monthly.ts) >= 2011 & 
                     time(ott_cinema_monthly.ts) < 2013) |
  (time(ott_cinema_monthly.ts) >= 2014 & 
     time(ott_cinema_monthly.ts) < 2016)

## ----impute_missing-------------------------------------------------------------------------------------------------------------
# Impute missing values for each target column
for (col in columns_to_impute) {
  # Extract column as numeric vector
  column_data <- as.numeric(ott_cinema_monthly.ts[, col])
  
  if (col == "cinema_avg_spending_pp") {
    # For average spending per person, calculate as spending / admissions
    imputed_2013 <- cinema_imputed[, "cinema_audience_spending"] / 
      cinema_imputed[, "cinema_admissions"]
    
    # Fill missing values for 2013 using calculated values
    column_data[time(ott_cinema_monthly.ts) >= 2013 & 
                  time(ott_cinema_monthly.ts) < 2014] <- 
      imputed_2013[time(ott_cinema_monthly.ts) >= 2013 & 
                     time(ott_cinema_monthly.ts) < 2014]
    
    # Fill remaining missing values for 2020 and 2021 (COVID impact) with yearly averages
    column_data[time(ott_cinema_monthly.ts) >= 2020 & 
                  time(ott_cinema_monthly.ts) < 2021 & 
                  is.na(column_data)] <- 6.35  # 2020 average
    column_data[time(ott_cinema_monthly.ts) >= 2021 & 
                  time(ott_cinema_monthly.ts) < 2022 & 
                  is.na(column_data)] <- 6.76  # 2021 average
  } 
  
  else {
    # For cumulative columns, calculate typical monthly proportions
    monthly_proportions <- colMeans(matrix(column_data[selected_years], ncol = 12, 
                                           byrow = TRUE), na.rm = TRUE)
    
    # Normalize to get proportions
    monthly_proportions <- monthly_proportions / sum(monthly_proportions)
    
    # Estimate 2013 monthly values by multiplying total by proportions
    imputed_2013 <- monthly_proportions * total_2013[[col]]
    
    # Replace 2013 missing values with imputed ones
    column_data[time(ott_cinema_monthly.ts) >= 2013 & 
                  time(ott_cinema_monthly.ts) < 2014] <- imputed_2013
  }
  
  # Save the updated column into the imputed dataset
  cinema_imputed[, col] <- column_data
}

## ----missing_vs_imputed---------------------------------------------------------------------------------------------------------
# Compare original and imputed values visually
# Set layout for 4 rows and 2 columns of plots
par(mfrow = c(4, 2), mar = c(2, 3, 1.5, 1))

# Loop over columns to impute
for (col in columns_to_impute) {
  # Plot original data
  plot(ott_cinema_monthly.ts[, col], type = "o", pch = 16, lty = 3, cex = 0.4,
       main = paste("Original", col), xlab = "", ylab = "")
  
  # Plot imputed data
  plot(cinema_imputed[, col], type = "o", pch = 16, lty = 3, cex = 0.4,
       main = paste("Imputed", col), xlab = "", ylab = "")
}

# Reset plot layout
par(mfrow = c(1, 1))

## ----monthly_imputed------------------------------------------------------------------------------------------------------------
# Replace original dataset with imputed version
ott_cinema_monthly.ts <- cinema_imputed
ott_cinema_monthly <- as.data.frame(cinema_imputed)

# Plot imputed dataset
plot(ott_cinema_monthly.ts, main = "Cinema-OTT Monthly Dataset (After Imputation)",
     cex.lab = 0.45, type = "o", pch = 16, lty = 3, cex = 0.6)

## ----corr_monthly---------------------------------------------------------------------------------------------------------------
# Compute and visualize correlation matrix for monthly data

# Convert time series object to data frame
ott_cinema_corr <- as.data.frame(ott_cinema_monthly.ts)

# Compute correlation matrix using complete observations
cor_matrix_monthly <- cor(ott_cinema_corr, use = "complete.obs")

# Plot the lower triangle of the correlation matrix with color coding and labels
corrplot(cor_matrix_monthly, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, tl.cex = 0.75)

## ----vif_monthly----------------------------------------------------------------------------------------------------------------
# Variance Inflation Factor (VIF) analysis to check multicollinearity

# Fit linear model to predict cinema_admissions using all other variables
vif_model.monthly <- lm(ott_cinema_monthly$cinema_admissions ~
                          . - ott_cinema_monthly$cinema_admissions, 
                        data = ott_cinema_monthly[,-1])

# Compute VIF values
VIF.values <- vif(vif_model.monthly)

# Display VIF table
kable(as.data.frame(VIF.values), caption = "VIF Table for Monthly Data")

## ----agg_google-----------------------------------------------------------------------------------------------------------------
# Create 'Year' column for grouping monthly data
ott_cinema_monthly$Year <- rep(2006:2023, each = 12)

# Aggregate monthly Google Trends OTT data to annual level
google_trends_annual <- ott_cinema_monthly %>%
  group_by(Year) %>%
  summarise(google_trend_ott = sum(gooogle_trend_ott, na.rm = TRUE))

# Merge aggregated Google Trends data with selected columns from annual dataset
merged_ott_data <- merge(ott_cinema_annual[, c(1:3)], google_trends_annual, 
                         by.x = "Year", by.y = "Year", all.x = TRUE)

## ----corr_merged_ott------------------------------------------------------------------------------------------------------------
# Compute correlation matrix for merged annual data
cor_matrix <- cor(merged_ott_data[,-1], use = "complete.obs")
cor_matrix


## ----minmax_merged_ott----------------------------------------------------------------------------------------------------------
# Define a Min-Max scaling function to normalize values between 0 and 1
min_max_scale <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Apply Min-Max scaling to key OTT variables
merged_ott_data$ott_subscriptions_scaled <-
  min_max_scale(merged_ott_data$ott_subscriptions_M)

merged_ott_data$ott_spending_scaled <- 
  min_max_scale(merged_ott_data$ott_spending_M_EUR)

merged_ott_data$google_trend_ott_scaled <- 
  min_max_scale(merged_ott_data$google_trend_ott)

## ----compare_ott_trends---------------------------------------------------------------------------------------------------------
# Plot normalized OTT subscriptions, spending, and Google Trends to compare trends
plot(merged_ott_data$Year, merged_ott_data$ott_subscriptions_scaled, type = "l", 
     col = "blue", lwd = 2, xlab = "Year", ylab = "Normalized Value",
     main = "OTT Subscriptions, OTT Spending, and Google Trends OTT")
lines(merged_ott_data$Year, merged_ott_data$ott_spending_scaled, 
      col = "red", lwd = 2)
lines(merged_ott_data$Year, merged_ott_data$google_trend_ott_scaled, 
      col = "green", lwd = 2) 
legend("topleft", 
       legend = c("OTT Subscriptions", "OTT Spending", "Google Trends OTT"), 
       col = c("blue", "red", "green"), lwd = 2, bty = "n")

## ----cinema_admissions----------------------------------------------------------------------------------------------------------
# Extract the 'cinema_admissions' time series for analysis
cinema_admissions <- ott_cinema_monthly.ts[, "cinema_admissions"]

# Display general plot, ACF, and PACF of the series
tsdisplay(cinema_admissions, lag.max = 50)

## ----seasonplot_admissions------------------------------------------------------------------------------------------------------
# Create a seasonal plot to visualize patterns across years
seasonplot(cinema_admissions, ylab = "admissions", xlab = "Year", 
           main = "Seasonal plot: Cinema Admissions", year.labels = TRUE, 
           year.labels.left = TRUE, col = 1:20, pch = 19)

## ----stl_decomp-----------------------------------------------------------------------------------------------------------------
# Apply STL decomposition to separate trend, seasonality, and residuals
admissions_decomposed <- stl(cinema_admissions, s.window = "periodic")

# Plot the decomposed components
plot(admissions_decomposed, main = "TS Decomposition of Cinema Admissions",
     cex.lab = 0.45, pch = 16, lty = 1, cex = 0.6)

## ----variables------------------------------------------------------------------------------------------------------------------
# Extract individual variables from the time series
cinema_screening <- ott_cinema_monthly.ts[, 2]
cinema_spending <- ott_cinema_monthly.ts[, 3]
avg_cinema_spending_pp <- ott_cinema_monthly.ts[, 4]
google_trend_ott <- ott_cinema_monthly.ts[, 5]
cpi_all <- ott_cinema_monthly.ts[, 6]
cpi_culture <- ott_cinema_monthly.ts[, 7]
covid_cases <- ott_cinema_monthly.ts[, 8]

# Define performance metrics and prepare an empty data frame to store model results
metrics <- c("Model", "AIC", "DW_Test", "Ljung_Box_pval", "R2", "MAE", "RMSE")
model_performance <- data.frame(matrix(ncol = length(metrics), nrow = 0))
colnames(model_performance) <- metrics

## ----full_tslm------------------------------------------------------------------------------------------------------------------
# Fit a full time series linear model with all predictors + trend + seasonality
full_tslm <- tslm(cinema_admissions ~ cinema_screening + cinema_spending + 
                    avg_cinema_spending_pp + google_trend_ott + cpi_all +
                    cpi_culture + covid_cases + trend + season)

# Show model summary
summary(full_tslm)

# Check residuals for autocorrelation and other issues
checkresiduals(full_tslm)

## ----full_tslm_metrics----------------------------------------------------------------------------------------------------------
# Collect and save performance metrics for the full TSLM model
model_name <- "TSLM - Full"
aic <- sprintf("%.3f", AIC(full_tslm))  # AIC value
dw <- sprintf("%.3f", as.numeric(dwtest(full_tslm)$statistic))  # Durbin-Watson stat
lj_box <- NA  # Ljung-Box test not applied here
r2 <- sprintf("%.3f", summary(full_tslm)$r.squared)  # R-squared
acc <- as.data.frame.array(forecast::accuracy(full_tslm))  # Accuracy metrics
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Add results to performance table
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----stepwise_tslm--------------------------------------------------------------------------------------------------------------
# Convert tslm model to a standard lm object for stepwise regression
full_lm <- lm(formula(full_tslm), data = as.data.frame(ott_cinema_monthly.ts))

# Perform stepwise selection (both directions)
stepwise_model <- step(full_lm, direction = "both", trace = TRUE)

# Show summary of selected model
summary(stepwise_model)

# Check residuals
checkresiduals(stepwise_model)

# Perform Durbin-Watson test for autocorrelation (result indicates positive autocorrelation)
dwtest(stepwise_model)

## ----stepwise_tslm_metrics------------------------------------------------------------------------------------------------------
# Collect and save performance metrics for the stepwise model
model_name <- "TSLM - Stepwise"
aic <- sprintf("%.3f", AIC(stepwise_model))
dw <- sprintf("%.3f", as.numeric(dwtest(stepwise_model)$statistic))
lj_box <- NA
r2 <- sprintf("%.3f", summary(stepwise_model)$r.squared)
acc <- as.data.frame.array(forecast::accuracy(stepwise_model))
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Add results to performance table
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----stepwise_regression--------------------------------------------------------------------------------------------------------
# Get fitted values from the stepwise model
fitted_stepwise <- fitted(stepwise_model)

# Convert fitted values to a time series
fitted_stepwise.ts <- ts(fitted_stepwise, start = start(cinema_admissions), 
                         frequency = frequency(cinema_admissions))

# Plot actual vs predicted values
plot(cinema_admissions, type = "o", pch = 16, cex = 0.6,
     main = "Actual vs. Stepwise Model Predictions")

# Add predicted values from stepwise model
lines(fitted_stepwise.ts, col = "orange2", lwd = 2, lty = 1)

# Add legend to the plot
legend("topright", legend = c("Observed", "Predicted (Stepwise)"),
       col = c("black", "orange2"), lwd = c(1, 2), lty = c(1, 1), bty = "n")

## ----diff_d1--------------------------------------------------------------------------------------------------------------------
# First difference to remove trend (d = 1)
diff1 <- diff(cinema_admissions)
tsdisplay(diff1, main = "cinema_admissions (d = 1)")

## ----diff_D1--------------------------------------------------------------------------------------------------------------------
# Seasonal difference to remove yearly seasonality (D = 1, s = 12)
diff2 <- diff(cinema_admissions, lag = 12)
tsdisplay(diff2, main = "cinema_admissions (D = 1, s = 12)")

## ----diff_d1_D1-----------------------------------------------------------------------------------------------------------------
# Apply both regular and seasonal differencing (d = 1, D = 1, s = 12)
diff3 <- diff(diff1, lag = 12)
tsdisplay(diff3, main = "cinema_admissions (d = 1, D = 1, s = 12)")

## ----arima1---------------------------------------------------------------------------------------------------------------------
# Manually defined ARIMA model: ARIMA(2,1,5)(0,1,1)[12]
arima1.cinema_admissions <- Arima(cinema_admissions, order = c(2,1,5), seasonal = c(0,1,1))
summary(arima1.cinema_admissions)

# Get fitted values from the model
fit.arima1.cinema_admissions <- fitted(arima1.cinema_admissions)

# Plot actual vs fitted values
plot(cinema_admissions, type = "o", pch = 16, cex = 0.6, ylab = "Admissions",
     main = "Observed vs. ARIMA(2,1,5)(0,1,1)[12] predictions")
lines(fit.arima1.cinema_admissions, col = "orange2", lwd = 2)

# Add legend
legend("topright", legend = c("Observed", "ARIMA"),
       col = c("black", "orange2"), lwd = c(1, 2), lty = c(1, 1), bty = "n")

# Display residuals
res.arima1.cinema_admissions <- residuals(arima1.cinema_admissions)
tsdisplay(res.arima1.cinema_admissions)

## ----arima1_metrics-------------------------------------------------------------------------------------------------------------
# Save performance metrics for ARIMA(2,1,5)(0,1,1)[12]
model_name <- "ARIMA(2,1,5)(0,1,1)[12]"
aic <- sprintf("%.3f", AIC(arima1.cinema_admissions))
dw <- NA  # DW test not applied here
lj_box <- sprintf("%.3f", checkresiduals(arima1.cinema_admissions)$p.value)  # Ljung-Box p-value
r2 <- sprintf("%.3f", R2_Score(fit.arima1.cinema_admissions, cinema_admissions))  # R-squared
acc <- as.data.frame.array(forecast::accuracy(arima1.cinema_admissions))  # Accuracy metrics
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Add results to model performance table
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----auto_arima-----------------------------------------------------------------------------------------------------------------
# Fit ARIMA model automatically using auto.arima()
arima.a.cinema_admissions <- auto.arima(cinema_admissions)
summary(arima.a.cinema_admissions)

# Get fitted values
fit.arima.a.cinema_admissions <- fitted(arima.a.cinema_admissions)

# Plot actual vs predicted values
plot(cinema_admissions, type = "o", pch = 16, cex = 0.6, ylab = "Admissions",
     main = "Observed vs. autoARIMA predictions")
lines(fit.arima.a.cinema_admissions, col = "purple2", lwd = 2)

# Add legend
legend("topright", legend = c("Observed", "autoARIMA"),
       col = c("black", "purple2"), lwd = c(1, 2), lty = c(1, 1), bty = "n")

# Show residual diagnostics
res.arima.a.cinema_admissions <- residuals(arima.a.cinema_admissions)
tsdisplay(res.arima.a.cinema_admissions)

## ----auto_arima_metrics---------------------------------------------------------------------------------------------------------
# Save performance metrics for auto ARIMA model
model_name <- "auto.ARIMA(1,0,2)(0,1,1)[12]"
aic <- sprintf("%.3f", AIC(arima.a.cinema_admissions))
dw <- NA
lj_box <- sprintf("%.3f", checkresiduals(arima.a.cinema_admissions)$p.value)
r2 <- sprintf("%.3f", R2_Score(fit.arima.a.cinema_admissions, cinema_admissions))
acc <- as.data.frame.array(forecast::accuracy(arima.a.cinema_admissions))
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Add results to model performance table
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)


## ----auto_sarimax1--------------------------------------------------------------------------------------------------------------
# Define external regressors for SARIMAX
xreg1 <- cbind(cinema_screening, cinema_spending, avg_cinema_spending_pp, cpi_all)

# Fit auto.ARIMA model with exogenous variables (SARIMAX)
sarimax_model1 <- auto.arima(cinema_admissions, xreg = xreg1)
summary(sarimax_model1)

# Get fitted values
fit.sarimax1 <- fitted(sarimax_model1)

# Plot actual vs predicted values
plot(cinema_admissions, type = "o", pch = 16, cex = 0.6)
lines(fit.sarimax1, col = "purple2", lwd = 2)

# Check residuals
res.sarimax_model1 <- residuals(sarimax_model1)
tsdisplay(res.sarimax_model1)

## ----auto_sarimax1_metrics------------------------------------------------------------------------------------------------------
# Save metrics for SARIMAX model (auto)
model_name <- "auto.ARIMAx(3,1,0)(2,0,1)[12]"
aic <- sprintf("%.3f", AIC(sarimax_model1))
dw <- NA
lj_box <- sprintf("%.3f", checkresiduals(sarimax_model1)$p.value)
r2 <- sprintf("%.3f", R2_Score(fit.sarimax1, cinema_admissions))
acc <- as.data.frame.array(forecast::accuracy(sarimax_model1))
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Store model performance
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----sarimax2-------------------------------------------------------------------------------------------------------------------
# Manually define SARIMAX model with chosen parameters
sarimax_model2 <- Arima(cinema_admissions, order = c(1,1,2), seasonal = c(0,1,1),
                        xreg = xreg1)
summary(sarimax_model2)

# Get fitted values
fit.sarimax2 <- fitted(sarimax_model2)

# Plot observed vs predicted
plot(cinema_admissions, type = "o", pch = 16, cex = 0.6)
lines(fit.sarimax2, col = "orange2", lwd = 2)
legend("topright", legend = c("Observed", "ARIMAx"),
       col = c("black", "orange2"), lwd = c(1, 2), lty = c(1, 1), bty = "n")

# Show residual diagnostics
res.sarimax_model2 <- residuals(sarimax_model2)
tsdisplay(res.sarimax_model2)

## ----sarimax2_metrics-----------------------------------------------------------------------------------------------------------
# Save metrics for SARIMAX model (manual)
model_name <- "ARIMAx(1,1,2)(0,1,1)[12]"
aic <- sprintf("%.3f", AIC(sarimax_model2))
dw <- NA
lj_box <- sprintf("%.3f", checkresiduals(sarimax_model2)$p.value)
r2 <- sprintf("%.3f", R2_Score(fit.sarimax2, cinema_admissions))
acc <- as.data.frame.array(forecast::accuracy(sarimax_model2))
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Store model performance
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----bass_model-----------------------------------------------------------------------------------------------------------------
# Fit Bass diffusion model to cinema admissions
BM.cinema_admissions <- BM(cinema_admissions)
summary(BM.cinema_admissions)

# Check residuals and plot the model
checkresiduals(BM.cinema_admissions)
plot.Dimora(BM.cinema_admissions)

## -------------------------------------------------------------------------------------------------------------------------------
# Generate time index for actual observations (monthly from 2006 to 2023)
observed_months <- seq(as.Date("2006-01-01"), as.Date("2023-12-01"), by = "month")

# Set forecast horizon (next 5 years = 60 months)
forecast_horizon <- 12 * 5  

# Create future time points starting after last observed month
predicted_months <- seq(from = max(observed_months) + 31,  # Next month
                        by = "month",                    
                        length.out = forecast_horizon)

## ----bass_predict---------------------------------------------------------------------------------------------------------------
# Make predictions using the Bass model
pred_BM.cinema_admissions <- predict(BM.cinema_admissions, 
                                     newx = 1:length(observed_months))

# Convert cumulative predictions to instantaneous values
inst_pred_BM.cinema_admissions <- make.instantaneous(pred_BM.cinema_admissions)

## ----bass_model_metrics---------------------------------------------------------------------------------------------------------
# Save metrics for the Bass model
model_name <- "Bass Model"
aic <- NA
dw <- NA
lj_box <- "< 2.2e-16"  # Based on model output
r2 <- sprintf("%.3f", as.numeric(BM.cinema_admissions$Rsquared))
mae <- format(mean(abs(cinema_admissions - inst_pred_BM.cinema_admissions)), 
              big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(sqrt(mean((cinema_admissions - inst_pred_BM.cinema_admissions)^2)), 
               big.mark = ",", nsmall = 2, scientific = FALSE)

# Store performance metrics
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----bass_forecast--------------------------------------------------------------------------------------------------------------
# Forecast cinema admissions using the Bass Model for the full period (observed + future)
forecast_BM.cinema_admissions <- predict(BM.cinema_admissions, 
                                         newx = 1:length(c(observed_months, 
                                                           predicted_months)))

# Convert cumulative Bass forecasts to monthly values
inst_forecast_BM.cinema_admissions <- make.instantaneous(forecast_BM.cinema_admissions)

## ----GBMe2----------------------------------------------------------------------------------------------------------------------
# Fit Generalized Bass Model (GBM) with exponential shocks (2 shocks specified)
GBM.e2 <- GBM(cinema_admissions, shock = "exp", nshock = 2,
              prelimestimates = c(1.963349e+09, 4.244444e-03, 
                                  9.822524e-03, 170, -0.1, -1,
                                  178, -0.7, -0.5)) 
summary(GBM.e2)

# Residual check and plot
checkresiduals(GBM.e2)
plot.Dimora(GBM.e2)

## ----gbm_pred-------------------------------------------------------------------------------------------------------------------
# Get in-sample predictions from GBM
pred_GBM.e2 <- predict(GBM.e2, newx = 1:length(observed_months)) 

# Convert cumulative predictions to monthly values
inst_pred_GBM.e2 <- make.instantaneous(pred_GBM.e2)

## ----gbm_metrics----------------------------------------------------------------------------------------------------------------
# Save performance metrics for GBM model
model_name <- "GBMe2"
aic <- NA
dw <- NA
lj_box <- "< 2.2e-16"
r2 <- sprintf("%.3f", GBM.e2$Rsquared)
mae <- format(mean(abs(cinema_admissions - inst_pred_GBM.e2)), 
              big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(sqrt(mean((cinema_admissions - inst_pred_GBM.e2)^2)), 
               big.mark = ",", nsmall = 2, scientific = FALSE)

model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----gbm_forecast---------------------------------------------------------------------------------------------------------------
# Forecast future cinema admissions using GBM
forecast_GBM.cinema_admissions <- predict(GBM.e2, 
                                          newx = 1:length(c(observed_months, 
                                                            predicted_months)))

# Convert cumulative forecast to monthly values
inst_forecast_GBM.cinema_admissions <- make.instantaneous(forecast_GBM.cinema_admissions)

## ----bm_gbm_predict-------------------------------------------------------------------------------------------------------------
# Plot actual vs. forecasted values from Bass and GBM models
plot(observed_months, cinema_admissions, type = "o", pch = 16, lty = 3, 
     cex = 0.6, xlab = "Time", ylab = "Monthly Cinema Admissions",
     xlim = range(c(observed_months, predicted_months)), 
     ylim = c(0, max(cinema_admissions, inst_forecast_GBM.cinema_admissions)))

# Add Bass Model forecast line
lines(c(observed_months, predicted_months), inst_forecast_BM.cinema_admissions, 
      lwd = 3, col = 2) 

# Add GBM forecast line
lines(c(observed_months, predicted_months), inst_forecast_GBM.cinema_admissions, 
      lwd = 3, col = 3)

# Add legend
legend("topright", c("Bass Model", "GBMe2"), col = c(2, 3),
       lty = c(1, 1), lwd = c(3, 3), merge = TRUE, cex = 0.8, bty = "n")

## ----gbm_sarmax-----------------------------------------------------------------------------------------------------------------
# Combine GBM fitted values with SARIMAX (ARIMA with external regressor = GBM fit)
fit_GBM.e2 <- fitted(GBM.e2)

# Fit SARIMAX using cumulative cinema_admissions and GBM predictions as external regressor
GBM.sarmax <- Arima(cumsum(cinema_admissions), order = c(2,1,0), 
                    seasonal = list(order = c(0,1,2), period = 12), 
                    xreg = fit_GBM.e2)

summary(GBM.sarmax)

# Residuals and diagnostics
res.GBM.sarmax <- residuals(GBM.sarmax)
tsdisplay(res.GBM.sarmax)

# Get fitted values and convert to monthly
fit.GBM.sarmax <- fitted(GBM.sarmax)
inst_fit.GBM.sarmax <- make.instantaneous(fit.GBM.sarmax)

## ----gbm_sarmax_metrics---------------------------------------------------------------------------------------------------------
# Save metrics for GBM + SARIMAX model
model_name <- "GBMe2 + ARIMAx(2,1,0)(0,1,2)12"
aic <- sprintf("%.3f", AIC(GBM.sarmax))
dw <- NA
lj_box <- sprintf("%.3f", checkresiduals(GBM.sarmax)$p.value)
r2 <- sprintf("%.3f", R2_Score(as.numeric(inst_fit.GBM.sarmax), cinema_admissions))
acc <- as.data.frame.array(forecast::accuracy(GBM.sarmax))
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----gbm_sarmax_plot------------------------------------------------------------------------------------------------------------
# Plot observed data and compare GBMe2 and GBMe2 + SARMAX predictions
plot(observed_months, cinema_admissions, type = "o", pch = 16, lty = 1, 
     cex = 0.6, lwd = 2,
     xlab = "Time", ylab = "Monthly Cinema Admissions",
     xlim = range(c(observed_months, predicted_months)), 
     ylim = c(0, max(cinema_admissions, inst_forecast_GBM.cinema_admissions)))

# Add GBMe2 forecast
lines(c(observed_months, predicted_months), inst_forecast_GBM.cinema_admissions, 
      lwd = 3, col = 3)

# Add GBMe2 + SARMAX fitted values
lines(observed_months, inst_fit.GBM.sarmax, lty = 1, lwd = 1, col = 2)

# Add legend
legend("topright", c("GBMe2", "GBMe2 + SARMAX"), col = c(3, 2),
       lty = c(1, 1), lwd = c(3, 1), merge = TRUE, cex = 0.8, bty = "n")

## ----gbm_sarmax_cumsum----------------------------------------------------------------------------------------------------------
# Plot cumulative admissions for GBMe2 and GBMe2 + SARMAX
plot(observed_months, cumsum(cinema_admissions), type = "b", pch = 16, lty = 3, 
     cex = 0.6, lwd = 1,
     xlab = "Time", ylab = "Cumulative Monthly Cinema Admissions",
     xlim = range(c(observed_months, predicted_months)))

# Add GBMe2 cumulative forecast
lines(c(observed_months, predicted_months), forecast_GBM.cinema_admissions, 
      lty = 4, lwd = 2, col = 3)

# Add GBMe2 + SARMAX cumulative fitted values
lines(observed_months, cumsum(inst_fit.GBM.sarmax), lty = 4, lwd = 2, col = 2)

# Add legend
legend("bottomright", c("GBMe2", "GBMe2 + SARMAX"), col = c(3, 2),
       lty = c(4, 4), lwd = c(2, 2), merge = TRUE, cex = 0.8, bty = "n")

## ----minmax_UCRCD---------------------------------------------------------------------------------------------------------------
# Apply Min-Max Normalization to annual cinema and OTT data
ott_subscriptions.norm <- min_max_scale(ott_cinema_annual$ott_subscriptions_M)
cinema_admissions.norm <- min_max_scale(ott_cinema_annual$cinema_admissions)

## ----UCRCD_normalized-----------------------------------------------------------------------------------------------------------
# Plot normalized series of cinema admissions and OTT subscriptions
plot(ott_cinema_annual$Year, cinema_admissions.norm, type = "o", pch = 16, lty = 3, col = "blue", 
     xlab = "Year", ylab = "Normalized Values",
     main = "Normalized OTT Subscriptions and Cinema Admissions", ylim = c(0, 1))
points(ott_cinema_annual$Year, ott_subscriptions.norm, type = "o", pch = 16, lty = 3, col = "red")

# Add legend
legend("left", legend = c("Cinema Admissions", "OTT Subscriptions"), 
       col = c("blue", "red"), lty = 3, pch = 16, cex = 0.7)

## ----initial_UCRCD_bass_norm----------------------------------------------------------------------------------------------------
# Fit Bass model to normalized cinema admissions data
BM.cinema_admissions.norm <- BM(cinema_admissions.norm, display = TRUE)
m.c <- BM.cinema_admissions.norm$Estimate[1, 1]  # Market potential
p.c <- BM.cinema_admissions.norm$Estimate[2, 1]  # Innovation coefficient
q.c <- BM.cinema_admissions.norm$Estimate[3, 1]  # Imitation coefficient

# Fit Bass model to normalized OTT subscriptions data
BM.ott_subscriptions.norm <- BM(ott_subscriptions.norm, display = TRUE)
m.o <- BM.ott_subscriptions.norm$Estimate[1, 1]
p.o <- BM.ott_subscriptions.norm$Estimate[2, 1]
q.o <- BM.ott_subscriptions.norm$Estimate[3, 1]

## ----GBM_cinema_annual_norm-----------------------------------------------------------------------------------------------------
# Fit Generalized Bass Model (GBM) with exponential shock for cinema admissions
# Define the index for when COVID-19 began (relative to start year)
covid_start <- 2020 - 2006 + 1

# Run GBM with predefined shock parameters and initial estimates from BM
GBM.cinema_admissions.norm <- GBM(cinema_admissions.norm, shock = "exp", nshock = 1,
                                  prelimestimates = c(m.c, p.c, q.c, covid_start, -0.5, -0.1)) 

# Show model summary and check residuals
summary(GBM.cinema_admissions.norm)
checkresiduals(GBM.cinema_admissions.norm)

# Reassign the BM coefficients (not necessary if unchanged, but ensures consistency)
m.c <- BM.cinema_admissions.norm$Estimate[1, 1]
p.c <- BM.cinema_admissions.norm$Estimate[2, 1]
q.c <- BM.cinema_admissions.norm$Estimate[3, 1]

## ----GGM_ott_annual_norm--------------------------------------------------------------------------------------------------------
# Fit Generalized Gompertz Model (GGM) to OTT subscriptions
GGM.ott_subscriptions.norm <- GGM(ott_subscriptions.norm, 
                                  prelimestimates = c(m.o, 0.01, 0.01, p.o, q.o), oos = 10)

# Show model summary and check residuals
summary(GGM.ott_subscriptions.norm)
checkresiduals(GGM.ott_subscriptions.norm)

## ----UCRCD_norm-----------------------------------------------------------------------------------------------------------------
# Estimate the UCRCD model (Unidirectional Coupled Replicator Constrained Diffusion)
UCRCD.adm.subs.norm <- UCRCD(
  series1 = cinema_admissions.norm,  # Dependent variable (e.g., cinema admissions)
  series2 = ott_subscriptions.norm,  # Independent variable (e.g., OTT subscriptions)
  m1 = m.c,                          # Market potential for cinema
  p1c = p.c,                         # Innovation coefficient for cinema
  q1c = q.c,                         # Imitation coefficient for cinema
  m2 = m.o,                          # Market potential for OTT
  p2 = p.o,                          # Innovation coefficient for OTT
  q2 = q.o                           # Imitation coefficient for OTT
)

# Show model summary
summary(UCRCD.adm.subs.norm)

## ----setup_prophet--------------------------------------------------------------------------------------------------------------
# Define special events (lockdowns, holidays) to include in Prophet

# COVID-19 lockdown periods
lockdowns <- data.frame(
  holiday = c("lockdown_1", "lockdown_2"),
  ds = as.Date(c("2020-03-01", "2020-11-01")),  
  lower_window = c(0, 0),  
  upper_window = c(110, 200)  
)

# Christmas/New Year holiday season (with extended effect)
holiday_season <- data.frame(
  holiday = "christmas_newyear",
  ds = as.Date(c("2006-12-01", "2007-12-01", "2008-12-01", "2009-12-01", 
                 "2010-12-01", "2011-12-01", "2012-12-01", "2013-12-01", 
                 "2014-12-01", "2015-12-01", "2016-12-01", "2017-12-01", 
                 "2018-12-01", "2019-12-01", "2020-12-01", "2021-12-01", 
                 "2022-12-01", "2023-12-01", "2024-12-01", "2025-12-01", 
                 "2026-12-01", "2027-12-01", "2028-12-01"),
               lower_window = 0, upper_window = 45)
)

# Ferragosto (Italian summer holiday)
ferragosto <- data.frame(
  holiday = "ferragosto",
  ds = as.Date(c("2006-08-01", "2007-08-01", "2008-08-01", "2009-08-01", 
                 "2010-08-01", "2011-08-01", "2012-08-01", "2013-08-01", 
                 "2014-08-01", "2015-08-01", "2016-08-01", "2017-08-01", 
                 "2018-08-01", "2019-08-01", "2020-08-01", "2021-08-01", 
                 "2022-08-01", "2023-08-01", "2024-08-01", "2025-08-01", 
                 "2026-08-01", "2027-08-01", "2028-08-01"),
               lower_window = 0, upper_window = 30)
)

# Combine all special dates into one dataframe
holidays <- bind_rows(lockdowns, holiday_season, ferragosto)

# Create a 'ds' (date) column for Prophet
ott_cinema_monthly$ds <- as.Date(paste(ott_cinema_monthly$Year, 
                                       rep(1:12, length.out = nrow(ott_cinema_monthly)), 
                                       "01", sep = "-"))

# Create Prophet-compatible dataframe with date and target variable
prophet_data <- data.frame(ds = ott_cinema_monthly$ds, 
                           y = ott_cinema_monthly$cinema_admissions)


## ----prophet--------------------------------------------------------------------------------------------------------------------
# Fit Prophet model including holiday effects and multiplicative seasonality
prophet_model <- prophet(
  holidays = holidays,
  yearly.seasonality = TRUE,
  seasonality.mode = "multiplicative",
  changepoint.prior.scale = 0.15,
  changepoint.range = 0.85
)

# Fit the model to the data
prophet_model <- fit.prophet(prophet_model, prophet_data)

## ----prophet_predict------------------------------------------------------------------------------------------------------------
# Create future dates for the next 5 years (60 months)
future <- make_future_dataframe(prophet_model, periods = 12 * 5, freq = "month", include_history = TRUE)

# Generate predictions using Prophet
prophet_predict <- predict(prophet_model, prophet_data)

# Add predicted values to the dataset
prophet_data$prophet_predict <- prophet_predict$yhat

## -------------------------------------------------------------------------------------------------------------------------------
# Interactive dygraph plot (if dygraphs package is available)
dyplot.prophet(prophet_model, prophet_predict)

## ----plot_prophet_predict-------------------------------------------------------------------------------------------------------
# Static plot of Prophet forecast including changepoints
plot(prophet_model, prophet_predict) +
  add_changepoints_to_plot(prophet_model)

## ----prophet_plot_components----------------------------------------------------------------------------------------------------
# Plot forecast components: trend, seasonality, and holidays
prophet_plot_components(prophet_model, prophet_predict)

## ----prophet_residuals----------------------------------------------------------------------------------------------------------
# Calculate residuals (actual - predicted)
prophet_residuals <- prophet_data$y - prophet_data$prophet_predict

# Add residuals to the dataset
prophet_data$prophet_residuals <- prophet_residuals

# Check residuals (ACF, PACF, etc.)
checkresiduals(prophet_residuals)

## ----prophet_metrics------------------------------------------------------------------------------------------------------------
# Save performance metrics for Prophet model
model_name <- "Prophet"
aic <- NA
dw <- NA
lj_box <- sprintf("%.3e", checkresiduals(prophet_residuals)$p.value)
r2 <- sprintf("%.3f", R2_Score(prophet_data$prophet_predict, prophet_data$y))
mae <- format(mean(abs(prophet_residuals)), big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(sqrt(mean((prophet_residuals)^2)), big.mark = ",", nsmall = 2, scientific = FALSE)

# Add results to model performance table
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----prophet_residuals_arima----------------------------------------------------------------------------------------------------
# Fit ARIMA model using Prophet predictions as external regressors (SARIMAX)
prop.arima <- auto.arima(cinema_admissions, xreg = prophet_data$prophet_predict,
                         seasonal = TRUE)
summary(prop.arima)

## ----prop_arima_fit-------------------------------------------------------------------------------------------------------------
# Get fitted values from Prophet + SARMAx model
fit.prop.arima <- fitted(prop.arima)

# Add combined predictions to the dataset
prophet_data$combined_predict <- fit.prop.arima

## ----plot_prophet_arima---------------------------------------------------------------------------------------------------------
# Plot observed vs Prophet and Prophet + SARMAx predictions
plot(ott_cinema_monthly$ds, ott_cinema_monthly$cinema_admissions, type = "o", 
     pch = 16, cex = 0.6, xlab = "", ylab = "Cinema Admissions")

# Add Prophet predictions
lines(ott_cinema_monthly$ds, prophet_data$prophet_predict, col = "orange2", lwd = 2)

# Add Prophet + SARMAx predictions
lines(ott_cinema_monthly$ds, as.numeric(fit.prop.arima), col = "purple2", lwd = 2)

# Add legend
legend("topright", c("Observed", "Prophet", "Prophet + SARMAx"),
       col = c("black", "orange2", "purple2"), lwd = c(1, 2, 2), lty = 1, bty = "n", cex = 0.85)

## ----prophet_arima_metrics------------------------------------------------------------------------------------------------------
# Save metrics for Prophet + SARMAx model
model_name <- "Prophet + SARMAx"
aic <- sprintf("%.3f", AIC(prop.arima))
dw <- NA
lj_box <- sprintf("%.3f", checkresiduals(prop.arima)$p.value)
r2 <- sprintf("%.3f", R2_Score(prophet_data$combined_predict, prophet_data$y))
acc <- as.data.frame.array(forecast::accuracy(prop.arima))
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Add results to performance table
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----hw_exponential-------------------------------------------------------------------------------------------------------------
# Fit Holt-Winters Exponential Smoothing model with additive seasonality
hw.cinema_admissions <- hw(cinema_admissions, seasonal = "additive")
summary(hw.cinema_admissions)

# Plot Holt-Winters fitted values and forecasts
autoplot(hw.cinema_admissions) +
  autolayer(fitted(hw.cinema_admissions), series = "Holt-Winters' additive") +
  ylab("Cinema Admissions") + xlab("Year")

## ----hw_exponential_metrics-----------------------------------------------------------------------------------------------------
# Save metrics for Holt-Winters model
model_name <- "Holt-Winters Exponential"
aic <- sprintf("%.3f", hw.cinema_admissions$model$aic)
dw <- NA
lj_box <- sprintf("%.3e", checkresiduals(hw.cinema_admissions)$p.value)
r2 <- sprintf("%.3f", R2_Score(hw.cinema_admissions$fitted, cinema_admissions))
acc <- as.data.frame.array(forecast::accuracy(hw.cinema_admissions))
mae <- format(acc$MAE, big.mark = ",", nsmall = 2, scientific = FALSE)
rmse <- format(acc$RMSE, big.mark = ",", nsmall = 2, scientific = FALSE)

# Add results to performance table
model_performance[nrow(model_performance) + 1, ] <- 
  c(model_name, aic, dw, lj_box, r2, mae, rmse)

## ----total_model_performances---------------------------------------------------------------------------------------------------
# Display all model performance metrics in a table
kable(as.data.frame(model_performance))

## ----prophet_forecast-----------------------------------------------------------------------------------------------------------
# Create future time points (next 5 years) for Prophet model forecasting
future <- make_future_dataframe(prophet_model, periods = forecast_horizon, freq = "month", include_history = FALSE)

# Forecast using Prophet model
prophet_forecast <- predict(prophet_model, future)

## ----ppres_arima_forecast-------------------------------------------------------------------------------------------------------
# Forecast Prophet + ARIMA (SARMAx) model using future Prophet predictions as external regressor
prop.arima_forecast <- forecast(prop.arima, h = forecast_horizon, xreg = prophet_forecast$yhat)

## ----prop_arima_forecast--------------------------------------------------------------------------------------------------------
# Plot forecast results for Prophet + SARMAx
plot(prop.arima_forecast, xlab = "", ylab = "Cinema Admissions", 
     main = "Cinema Admissions with Prophet + SARMAx Forecast")

# Add fitted values line
lines(fit.prop.arima, col = "purple2", lwd = 2, lty = 2)

# Add legend
legend("topright", 
       legend = c("Observed", "Prophet + SARMAx Fitted", "Prophet + SARMAx Forecast"),
       col = c("black", "purple2", "skyblue3"), cex = 0.8,
       lty = c(1, 2, 1), lwd = c(2, 2, 2), pch = c(NA, NA, NA), pt.cex = 0.5, bty = "n")

## ----xreg_future----------------------------------------------------------------------------------------------------------------
# Define forecast horizon
horizon <- 12 * 5

# Forecast future values of each external regressor using ARIMA models
f_screening <- forecast(Arima(cinema_screening, order = c(2,1,5), seasonal = c(0,1,1)), h = horizon)
f_spending <- forecast(Arima(cinema_spending, order = c(2,1,6), seasonal = c(0,1,1)), h = horizon)
f_avg_spending <- forecast(Arima(avg_cinema_spending_pp, order = c(2,1,3), seasonal = c(1,1,0)), h = horizon)
f_cpi <- forecast(Arima(cpi_all, order = c(3,1,3), seasonal = c(1,1,1), include.drift = TRUE), h = horizon)

# Combine future forecasts into a matrix for future xreg (for SARIMAX or hybrid models)
xreg1_future <- cbind(
  screening = as.numeric(f_screening$mean),
  spending = as.numeric(f_spending$mean),
  avg_spending = as.numeric(f_avg_spending$mean),
  cpi = as.numeric(f_cpi$mean)
)

## ----xreg_future_tsdisplay------------------------------------------------------------------------------------------------------
# Check residuals of ARIMA models for each regressor to validate forecast quality
tsdisplay(residuals(f_screening))
tsdisplay(residuals(f_spending))
tsdisplay(residuals(f_avg_spending))
tsdisplay(residuals(f_cpi))

## ----forecast_sarimax-----------------------------------------------------------------------------------------------------------
# Forecast future cinema admissions using the SARIMAX model with external regressors
sarimax_forecast <- forecast(sarimax_model2, h = horizon, xreg = xreg1_future)

# Extract forecasted values and 95% confidence intervals
sarimax_mean <- sarimax_forecast$mean                      # Forecasted values
sarimax_upper <- sarimax_forecast$upper[, 2]               # Upper bound (95%)
sarimax_lower <- sarimax_forecast$lower[, 2]               # Lower bound (95%)

# Create future monthly dates starting from Jan 2024
forecast_dates <- seq(from = as.Date("2024-01-01"), by = "month", length.out = horizon)

## ----plot_forecast_sarimax------------------------------------------------------------------------------------------------------
# Combine observed and forecast dates
full_dates <- c(ott_cinema_monthly$ds, forecast_dates)

# Set y-axis limits to include forecast and intervals
y_range <- range(c(cinema_admissions, sarimax_mean, sarimax_upper, sarimax_lower), na.rm = TRUE)

# Plot actual observed values
plot(ott_cinema_monthly$ds, ott_cinema_monthly$cinema_admissions, type = "o", pch = 16, cex = 0.6,
     xlab = "", ylab = "Cinema Admissions",
     main = "ARIMAx Fitted and Forecast",
     xlim = range(full_dates),
     ylim = y_range)

# Add SARIMAX fitted values line
lines(ott_cinema_monthly$ds, fit.sarimax2, col = "orange2", lwd = 2)

# Add shaded confidence interval
polygon(c(forecast_dates, rev(forecast_dates)),
        c(sarimax_upper, rev(sarimax_lower)),
        col = rgb(0, 0, 1, 0.2), border = NA)  

# Add SARIMAX forecast line
lines(forecast_dates, sarimax_mean, col = "blue", lwd = 2, lty = 2)

# Add legend
legend("topright",
       legend = c("SARIMAx Fitted", "SARIMAx Forecast"),
       col = c("orange2", "blue"), cex = 0.8,
       lty = c(1, 2), lwd = c(2, 2), pch = c(NA, NA), pt.cex = 2, bty = "n")


