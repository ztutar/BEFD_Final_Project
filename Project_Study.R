---
title: "Final Project Study"
author: "Zeynep TUTAR"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,       # Display R code in the output document
  warning = FALSE,   # Hide warnings
  message = FALSE,   # Hide package loading messages
  tidy.opts = list(width.cutoff = 72),  # Controls code formatting width
  tidy = TRUE,       # Ensures code is neatly formatted
  comment = '',      # Removes default comment markers (##) in output
  fig.path='plots/',  # Saves figures to the 'plots/' directory
  cache = TRUE,      # Enables caching to avoid redundant computations
  cache.path = '_cache/', # Defines cache storage location
  dev = "png"
)
```

```{r libraries}
library(readxl) #for reading excel
library(lmtest) #for the use of DW test, autocorrelation check
library(forecast) #for linear reg for time series
library(DIMORA) #for innovation dif models (GBM, Bass Model etc.)
#library(fpp2) #for datasets
library(naniar)
#library(GGally)
library(ggplot2)
library(corrplot)
library(car)
#library(tseries)
#library(rcompanion)
library(dplyr)
library(tidyr)
#library(splines)
library(prophet)
```

# 1) Data Exploration & Preprocessing

## Load and Inspect the Dataset

```{r import_monthly}
ott_cinema_monthly  <- read_excel("ott_cinema_dataset.xlsx", sheet = "Monthly")
str(ott_cinema_monthly)
```


```{r import_annual}
ott_cinema_annual <- read_excel("ott_cinema_dataset.xlsx", sheet = "Annual")
str(ott_cinema_annual)
```


```{r format_time}
#ott_cinema_monthly$Time=as.Date(paste0(ott_cinema_monthly$Time, "-01"), format = "%Y-%m-%d")
#ott_cinema_annual$Year <- as.Date(paste0(ott_cinema_annual$Year, "-01-01"), format="%Y-%m-%d")
```


```{r missing_visual}
# Check for missing values
ott_cinema_monthly[apply(is.na(ott_cinema_monthly), 1, any), ]  
vis_miss(ott_cinema_monthly)

ott_cinema_annual[apply(is.na(ott_cinema_annual), 1, any), ]  
vis_miss(ott_cinema_annual)
```



```{r create_ts}
ott_cinema_monthly.ts <- ts(ott_cinema_monthly[,-1], start=c(2006,1), 
                            end=c(2023,12), frequency = 12)
ott_cinema_annual.ts <- ts(ott_cinema_annual[,-1], start=c(2006), 
                           end=c(2023), frequency = 1)
```


```{r plot_monthly_raw}
plot(ott_cinema_monthly.ts, main="Monthly Cinema-OTT Dataset", cex.lab=0.45, 
     type="o", pch = 16, lty = 3, cex=0.6)
```

```{r plot_annual_raw}
plot(ott_cinema_annual.ts, main="Annual Cinema - OTT Dataset", cex.lab=0.45, 
     type="o", pch = 16, lty = 3, cex=0.6)
```

## Handle Missing Values

### Handle Monthly Dataset Missing Values

```{r setup_missing}
# Define the total annual values for 2013 for each column
total_2013 <- list(
  cinema_screenings = 3014642,             
  cinema_admissions = 106124433,         
  cinema_audience_spending = 732299026.82
)

# Define the columns to impute
columns_to_impute <- c("cinema_screenings", "cinema_admissions", 
                       "cinema_audience_spending", "cinema_avg_spending_pp")

# Create a copy of the time series object for imputation
cinema_imputed <- ott_cinema_monthly.ts

# Filter data to include only 2011, 2012, 2014, 2015 for monthly averages
selected_years <- (time(ott_cinema_monthly.ts) >= 2011 & 
                     time(ott_cinema_monthly.ts) < 2013) |
                  (time(ott_cinema_monthly.ts) >= 2014 & 
                     time(ott_cinema_monthly.ts) < 2016)
```


```{r impute_missing}
# Loop through each column to impute missing values
for (col in columns_to_impute) {
  # Extract the column data
  column_data <- as.numeric(ott_cinema_monthly.ts[, col])
  
  if (col == "cinema_avg_spending_pp") {
    # Calculate avg_spending_pp using the imputed audience_spending/audience_admissions
    imputed_2013 <- cinema_imputed[, "cinema_audience_spending"] / 
      cinema_imputed[, "cinema_admissions"]
    
    # Replace the missing 2013 data with the calculated values
    column_data[time(ott_cinema_monthly.ts) >= 2013 & 
                  time(ott_cinema_monthly.ts) < 2014] <- 
      imputed_2013[time(ott_cinema_monthly.ts) >= 2013 & 
                     time(ott_cinema_monthly.ts) < 2014]
    
    # Handle missing values in 2020 and 2021 (due to COVID-19 impact)
    column_data[time(ott_cinema_monthly.ts) >= 2020 & 
                  time(ott_cinema_monthly.ts) < 2021 & 
                  is.na(column_data)] <- 6.35  # Average for 2020
    column_data[time(ott_cinema_monthly.ts) >= 2021 & 
                  time(ott_cinema_monthly.ts) < 2022 & 
                  is.na(column_data)] <- 6.76  # Average for 2021
  } 
  
  else {
    # Handle cumulative features using selected years for proportions
    monthly_proportions <- colMeans(matrix(column_data[selected_years], ncol = 12, 
                                           byrow = TRUE), na.rm = TRUE)
    # Normalize to proportions
    monthly_proportions <- monthly_proportions / sum(monthly_proportions)  
    imputed_2013 <- monthly_proportions * total_2013[[col]]
    
    # Replace the missing 2013 data with the imputed values
    column_data[time(ott_cinema_monthly.ts) >= 2013 & 
                  time(ott_cinema_monthly.ts) < 2014] <- imputed_2013
  }
  
  # Update the imputed data in the time series
  cinema_imputed[, col] <- column_data
}
```


```{r missing_vs_imputed}
# Plot the original and imputed data for comparison
for (col in columns_to_impute) {
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
  
  # Plot the original data
  plot(ott_cinema_monthly.ts[, col], type = "o", pch = 16, lty = 3, cex = 0.4,
       main = paste("Original", col), xlab = "Time", ylab = col)
  
  # Plot the imputed data
  plot(cinema_imputed[, col], type = "o", pch = 16, lty = 3, cex = 0.4,
       main = paste("Imputed", col), xlab = "Time", ylab = col)
}

par(mfrow = c(1, 1))
```


```{r monthly_imputed}
ott_cinema_monthly.ts <- cinema_imputed
ott_cinema_monthly <- as.data.frame(cinema_imputed)

# Final plot of the imputed dataset
plot(ott_cinema_monthly.ts, main="Cinema-OTT Monthly Dataset (After Imputation)",
     cex.lab=0.45, type="o", pch = 16, lty = 3, cex=0.6)
```


### Handle Annual Dataset Missing Values

```{r}
# Extract the households_w_internet column
#households_ts <- ott_cinema_annual.ts[,9]

# Fit a Damped Holt's Exponential Smoothing Model
#holt_model <- holt(households_ts, damped = TRUE, h = 1)

# Forecast the missing value for 2023
#forecast_2023 <- forecast(holt_model, h = 1)$mean

# Fill the missing value in the dataset
#ott_cinema_annual$households_w_internet[which(is.na(ott_cinema_annual$households_w_internet))] <- forecast_2023

#ott_cinema_annual.ts <- ts(ott_cinema_annual[,-1], start=c(2006), end=c(2023), frequency = 1)

# The updated dataset
#tail(ott_cinema_annual[,c(1,10)])
```


```{r}
# Plot the fitted model and forecast
#autoplot(holt_model) +
#  autolayer(fitted(holt_model), series = "Fitted Values", color = "blue") +
#  labs(title = "Damped Holt's Exponential Smoothing for Households with Internet",
#       x = "Year", y = "% Households with Internet")
```

```{r}
#plot(ott_cinema_annual.ts, main="Annual Cinema-OTT Dataset (After Imputation)",
#     cex.lab=0.45, type="o", pch = 16, lty = 3, cex=0.6)
```



## Collinearity Check 

### Correlation Matrix for Monthly Data

```{r corr_monthly}
#Correlation Matrix for Monthly Data 
ott_cinema_corr <- as.data.frame(ott_cinema_monthly.ts)

# Compute correlation matrix
cor_matrix_monthly <- cor(ott_cinema_corr, use = "complete.obs")
```


```{r plot_corr_monthly}
#default_margins <- par("mar")
#par(mar = c(5, 5, 4, 2) + 0.1)

# Visualize correlation matrix
corrplot(cor_matrix_monthly, method = "color", type = "lower", tl.col = "black", 
         tl.srt = 45, addCoef.col = "black", number.cex = 0.8, tl.cex=0.75)
#title(main = "Correlation Matrix: Monthly Data", line = 2, cex.main = 1.2)
#par(mar = default_margins)
```

### VIF Analysis for Monthly Data

```{r vif_monthly}
# Run VIF analysis
# Regression model with all predictors
vif_model.monthly <- lm(ott_cinema_monthly$cinema_admissions ~
                          .-ott_cinema_monthly$cinema_admissions, 
                        data = ott_cinema_monthly[,-1]) 
# Compute VIF
vif(vif_model.monthly) 
```


```{r}
# Compute correlation matrix
#cor_matrix_annual <- cor(ott_cinema_annual[,-1], use = "complete.obs")

# Visualize correlation matrix
#corrplot(cor_matrix_annual, method = "color", type = "lower",
#         tl.col = "black", tl.srt = 45, addCoef.col = "black",
#         number.cex = 0.65, , tl.cex=0.55)
#title(main = "Correlation Matrix: Annual Data", line = 2, cex.main = 1.2)

```


### Check correlation & trends between OTT Data (Annual) and Google Trends OTT (Monthly)

```{r agg_google}
ott_cinema_monthly$Year <- rep(2006:2023, each = 12)  # Create a Year column

# Aggregate Google Trends OTT monthly data to annual (sum)
google_trends_annual <- ott_cinema_monthly %>%
  group_by(Year) %>%
  summarise(google_trend_ott = sum(gooogle_trend_ott, na.rm = TRUE))

# Merge with annual dataset
merged_ott_data <- merge(ott_cinema_annual[,c(1:3)], google_trends_annual, 
                         by.x = "Year", by.y = "Year", all.x = TRUE)
```


```{r corr_merged_ott}
# Compute Correlation Matrix
cor_matrix <- cor(merged_ott_data[,-1], use = "complete.obs")
cor_matrix
```


```{r}
# Visualize Correlation Matrix
#corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.8, tl.cex=0.65)
```


```{r minmax_merged_ott}
min_max_scale <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Apply Min-Max Scaling to the relevant columns
merged_ott_data$ott_subscriptions_scaled <-
  min_max_scale(merged_ott_data$ott_subscriptions_M)

merged_ott_data$ott_spending_scaled <- 
  min_max_scale(merged_ott_data$ott_spending_M_EUR)

merged_ott_data$google_trend_ott_scaled <- 
  min_max_scale(merged_ott_data$google_trend_ott)
```


```{r}
# Convert Year to Date format for better plotting
#merged_ott_data$Year <- as.Date(paste0(merged_ott_data$Year, "-01-01"))
```


```{r compare_ott_trends}
# Compare trends
plot(merged_ott_data$Year, merged_ott_data$ott_subscriptions_scaled, type = "l", 
     col = "blue", lwd = 2,xlab = "Year", ylab = "Normalized Value",
     main = "OTT Subscriptions, OTT Spending, and Google Trends OTT")
lines(merged_ott_data$Year, merged_ott_data$ott_spending_scaled, 
      col = "red", lwd = 2)
lines(merged_ott_data$Year, merged_ott_data$google_trend_ott_scaled, 
      col = "green", lwd = 2) 
legend("topleft", 
       legend = c("OTT Subscriptions", "OTT Spending", "Google Trends OTT"), 
       col = c("blue", "red", "green"), lwd = 2, bty = "n")
```


## Time Series Decomposition


```{r cinema_admissions}
# Extract Cinema Admissions time series
cinema_admissions <- ott_cinema_monthly.ts[, "cinema_admissions"]

#a general view of the data
tsdisplay(cinema_admissions, lag.max = 50)
```


```{r seasonplot_admissions}
seasonplot(cinema_admissions, ylab="admissions", xlab="Year", 
           main="Seasonal plot: Cinema Admissions", year.labels=T, 
           year.labels.left=T, col=1:20, pch=19)
```


```{r stl_decomp}
# STL decomposition (Seasonal + Trend + Residual)
admissions_decomposed <- stl(cinema_admissions, s.window = "periodic")

# Plot the decomposed components
plot(admissions_decomposed, main = "TS Decomposition of Cinema Admissions",
     cex.lab=0.45, pch = 16, lty = 1, cex=0.6)

```

# 2) Time Series Forecasting

## Linear Regression


```{r variables}
cinema_screening <- ott_cinema_monthly.ts[,2]
cinema_spending <- ott_cinema_monthly.ts[,3]
avg_cinema_spending_pp <- ott_cinema_monthly.ts[,4]
google_trend_ott <- ott_cinema_monthly.ts[,5]
cpi_all <- ott_cinema_monthly.ts[,6]
cpi_culture <- ott_cinema_monthly.ts[,7]
covid_cases <- ott_cinema_monthly.ts[,8]
```


### Multiple Linear Regression with Time Series

```{r stepwise_tslm}
# Fit Initial Full Model
full_tslm <- tslm(cinema_admissions ~ cinema_screening + cinema_spending + 
                     avg_cinema_spending_pp + google_trend_ott + cpi_all +
                     cpi_culture + covid_cases + trend + season) 
summary(full_tslm)

# Convert tslm object to lm object
full_lm <- lm(formula(full_tslm), data = as.data.frame(ott_cinema_monthly.ts))

# Stepwise Selection (Both Directions: Forward & Backward)
stepwise_model <- step(full_lm, direction = "both", trace = TRUE)


# Display Summary of Selected Model
summary(stepwise_model) 

AIC(full_tslm, stepwise_model)

```

### Stepwise Regression

```{r stepwise_regression}
# Extract fitted values from stepwise model
fitted_stepwise <- fitted(stepwise_model)

# Convert fitted values into a time series object
fitted_stepwise.ts <- ts(fitted_stepwise, start = start(cinema_admissions), 
                frequency = frequency(cinema_admissions))



plot(cinema_admissions, type = "l", col = "black", lwd = 2, 
     ylab = "Cinema Admissions",
     main = "Actual vs. Stepwise Model Predictions")

# Add Stepwise Regression Predictions
lines(fitted_stepwise.ts, col = "red", lwd = 2, lty = 2)

# Add Legend
legend("topright", legend = c("Actual Admissions", "Predicted (Stepwise)"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2), bty = "n")

```


```{r test_stepwise}

dwtest(stepwise_model) #indicates a problem of positive autocorrelation

#check the residuals
checkresiduals(stepwise_model)
```

## ARIMA Models


General indication (for differenced data): 
- If the ACF is exponentially decaying or sinusoidal and there is a significant spike at lag p in PACF and nothing else, it may be an ARIMA(p,d,0). 
- If the PACF is exponentially decaying or sinusoidal and there is a significant spike at lag q in ACF and nothing else, it may be an ARIMA(0,d,q). 


```{r diff_d1}
diff1<- diff(cinema_admissions)
tsdisplay(diff1, main = "cinema_admissions (d=1)")
```

```{r diff_D1}
diff2<- diff(cinema_admissions, lag=12) 
tsdisplay(diff2, main = "cinema_admissions (D=1, s=12)")
```

```{r diff_d1_D1}
diff3<- diff(diff1, lag=12) 
tsdisplay(diff3, main = "cinema_admissions (d=1, D=1, s=12)")
```


```{r arima1}
# first Arima model 
arima1.cinema_admissions<- Arima(cinema_admissions, order=c(2,1,2), seasonal=c(1,1,1))
summary(arima1.cinema_admissions)
fit.arima1.cinema_admissions<- fitted(arima1.cinema_admissions)

plot(cinema_admissions, type="o", pch = 16, cex=0.6)
lines(fit.arima1.cinema_admissions, col="green", lwd=2)

res.arima1.cinema_admissions<- residuals(arima1.cinema_admissions)
tsdisplay(res.arima1.cinema_admissions)

checkresiduals(arima1.cinema_admissions)
```


```{r auto_arima}
arima.a.cinema_admissions<- auto.arima(cinema_admissions)
summary(arima.a.cinema_admissions)
fit.arima.a.cinema_admissions<- fitted(arima.a.cinema_admissions)

plot(cinema_admissions, type="o", pch = 16, cex=0.6)
lines(fit.arima.a.cinema_admissions, col="green", lwd=2)

res.arima.a.cinema_admissions<- residuals(arima.a.cinema_admissions)
tsdisplay(res.arima.a.cinema_admissions)

checkresiduals(arima.a.cinema_admissions)
```


```{r auto_sarimax1}
xreg1 <- cbind(cinema_screening, cinema_spending, avg_cinema_spending_pp, cpi_all)
sarimax_model1 <- auto.arima(cinema_admissions, xreg = xreg1)
summary(sarimax_model1)

fit.sarimax1<- fitted(sarimax_model1)

plot(cinema_admissions, type="o", pch = 16, cex=0.6)
lines(fit.sarimax1, col="green", lwd=2)

res.sarimax_model1<- residuals(sarimax_model1)
tsdisplay(res.sarimax_model1)

checkresiduals(sarimax_model1)
```


```{r sarimax2}
sarimax_model2 <- Arima(cinema_admissions, order=c(1,0,2), seasonal=c(0,1,1),
                        xreg = xreg1)
summary(sarimax_model2)

fit.sarimax2<- fitted(sarimax_model2)

plot(cinema_admissions, type="o", pch = 16, cex=0.6)
lines(fit.sarimax2, col="green", lwd=2)

res.sarimax_model2<- residuals(sarimax_model2)
tsdisplay(res.sarimax_model2)

checkresiduals(sarimax_model2)
```


```{r arima_comparisons}
AIC(arima.a.cinema_admissions, 
    arima1.cinema_admissions, 
    sarimax_model1, 
    sarimax_model2)
```


## Diffusion Models

### Bass Model (BM)


```{r bass_model}
# Fit the Bass Model
BM.cinema_admissions <- BM(cinema_admissions)
summary(BM.cinema_admissions)

checkresiduals(BM.cinema_admissions)
plot.Dimora(BM.cinema_admissions)
```

Future Prediction

```{r}
# Generate time index for observed data (2006 to 2023, monthly)
observed_months <- seq(as.Date("2006-01-01"), as.Date("2023-12-01"), by = "month")
# Generate time index for predictions (2006 to 2033, monthly)
#predicted_months <- seq(as.Date("2006-01-01"), as.Date("2033-12-01"), by = "month")

# Set prediction horizon
forecast_horizon <- 60  

# Start predictions from the first month after observed data ends
predicted_months <- seq(from = max(observed_months) + 1,  # Start from next month
                         by = "month",                    
                         length.out = forecast_horizon)   

```


```{r bass_predict}
# Predict into the future
pred_BM.cinema_admissions<- predict(BM.cinema_admissions, 
                                    newx = 1:length(c(observed_months, 
                                                      predicted_months))) 
inst_pred_BM.cinema_admissions<- make.instantaneous(pred_BM.cinema_admissions)
```


### GBM

```{r GBMe2}
# GBM exponential shock
GBM.cinema_admissions<- GBM(cinema_admissions,shock = "exp",nshock = 2,
                                 prelimestimates = c(1.963349e+09, 4.244444e-03, 
                                                     9.822524e-03, 170,-0.1,-1,
                                                     178,-0.7,-0.5)) 
# m,p,q, a(starting point), b(memory of the shock), c(intensity) 
summary(GBM.cinema_admissions)

checkresiduals(GBM.cinema_admissions)
plot.Dimora(GBM.cinema_admissions)
```


Future Prediction


```{r gbm_pred}
# Predict into the future
pred_GBM.cinema_admissions<- predict(GBM.cinema_admissions, 
                                     newx = 1:length(c(observed_months, 
                                                       predicted_months))) 
inst_pred_GBM.cinema_admissions<- make.instantaneous(pred_GBM.cinema_admissions)
```


```{r bm_gbm_predict}
# Plot observed monthly cinema_admissions
plot(observed_months, cinema_admissions, type = "o", pch = 16, lty = 3, 
     cex = 0.6, xlab = "Time", ylab = "Monthly Cinema Admissions",
     xlim = range(c(observed_months, predicted_months)), 
     ylim = c(0, max(cinema_admissions, inst_pred_GBM.cinema_admissions)))

# Overlay predicted instantaneous cinema_admissions
lines(c(observed_months, predicted_months), inst_pred_BM.cinema_admissions, 
      lwd = 3, col = 2) 
lines(c(observed_months, predicted_months), inst_pred_GBM.cinema_admissions, 
      lwd = 3, col = 3)

legend("topright", c("Bass Model", "GBMe2"), col = c(2, 3),
       lty = c(1, 1), lwd = c(3, 3), merge = TRUE, cex=0.8)
```


### GBM with SARMAX refinement


```{r gbm_sarmax}
# SARMAX Refinement
fit_GBM.cinema_admissions <- fitted(GBM.cinema_admissions)
#inst_fit_GBM.cinema_admissions<- make.instantaneous(fit_GBM.cinema_admissions)

GBM.sarmax <- Arima(cumsum(cinema_admissions), order = c(2,1,0), 
                    seasonal = list(order = c(0,1,2), period = 12), 
                    xreg = fit_GBM.cinema_admissions)
summary(GBM.sarmax)
res.GBM.sarmax <- residuals(GBM.sarmax)
tsdisplay(res.GBM.sarmax)

checkresiduals(GBM.sarmax)

fit.GBM.sarmax <- fitted(GBM.sarmax)
inst_fit.GBM.sarmax <- make.instantaneous(fit.GBM.sarmax)
```


```{r gbm_sarmax_plot}
plot(observed_months, cinema_admissions, type = "o", pch = 16, lty = 1, 
     cex = 0.6, lwd = 2,
     xlab = "Time", ylab = "Monthly Cinema Admissions",
     xlim = range(c(observed_months, predicted_months)), 
     ylim = c(0, max(cinema_admissions, inst_pred_GBM.cinema_admissions)))
lines(c(observed_months, predicted_months), inst_pred_GBM.cinema_admissions, 
      lwd = 3, col = 3)
lines(observed_months, inst_fit.GBM.sarmax, lty=1, lwd=1, col=2)
legend("topright", c("GBMe2", "GBMe2 + SARMAX"), col = c(3, 2),
       lty = c(1, 1), lwd = c(3, 1), merge = TRUE, cex=0.8)
```

```{r gbm_sarmax_cumsum}

plot(observed_months, cumsum(cinema_admissions), type="b", pch = 16, lty = 3, 
     cex = 0.6, lwd = 1,
     xlab = "Time", ylab = "Cumulative Monthly Cinema Admissions",
     xlim = range(c(observed_months, predicted_months)))
lines(c(observed_months, predicted_months), pred_GBM.cinema_admissions, 
      lty=4, lwd = 3, col = 3)
lines(observed_months, cumsum(inst_fit.GBM.sarmax), lty=4, lwd=3, col=2)
legend("bottomright", c("GBMe2", "GBMe2 + SARMAX"), col = c(3, 2),
       lty = c(4, 4), lwd = c(3, 3), merge = TRUE, cex=0.8)

```


!!! TODO: how to do future predictions with sarmax refinement?

```{r gbm_sarmax_future}
# Create future xreg using GBM model predictions
future.xreg <- matrix(inst_pred_GBM.cinema_admissions, ncol = 1)

# Forecast SARMAX with xreg
GBM.sarmax_forecast <- forecast(GBM.sarmax, h = forecast_horizon, xreg = future.xreg)$mean
inst_GBM.sarmax_forecast <- make.instantaneous(GBM.sarmax_forecast)


plot(observed_months, cinema_admissions, type = "o", pch = 16, lty = 1, 
     cex = 0.6, lwd = 2,
     xlab = "Time", ylab = "Monthly Cinema Admissions",
     xlim = range(c(observed_months, predicted_months)), 
     ylim = c(0, max(cinema_admissions, inst_pred_GBM.cinema_admissions)))

lines(observed_months, inst_fit.GBM.sarmax, col = "red", lwd = 1)
lines(c(observed_months, predicted_months), inst_GBM.sarmax_forecast, 
      col = "blue", lwd = 2)

legend("topright", c("Observed", "GBM SARMAX Refinement","SARMAX Forecast"),
       col = c("black", "red", "blue"), lty = c(1,1,1), lwd = 2)

```



## Prophet Model

```{r setup_prophet}
# Define COVID-19 lockdown periods as holidays in Prophet
lockdowns <- data.frame(
  holiday = c("lockdown_1", "lockdown_2"),
  ds = as.Date(c("2020-03-01", "2020-11-01")),  # Start dates
  lower_window = c(0, 0),  # Start at the given date
  upper_window = c(110, 200)  # Duration of lockdown periods
)
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

holidays <- bind_rows(lockdowns, holiday_season, ferragosto)

# Create a Date column
ott_cinema_monthly$ds <- as.Date(paste(ott_cinema_monthly$Year, 
                                       rep(1:12, length.out = nrow(ott_cinema_monthly)), 
                                       "01", sep = "-"))

# create the Prophet dataframe
prophet_data <- data.frame(ds = ott_cinema_monthly$ds, 
                           y = ott_cinema_monthly$cinema_admissions)
```


```{r prophet}
# Fit Prophet model with lockdown effects
prophet_model <- prophet(holidays = holidays, 
                         yearly.seasonality = TRUE, seasonality.mode="multiplicative",
                         changepoint.prior.scale = 0.15,
                         changepoint.range = 0.85)
prophet_model <- fit.prophet(prophet_model, prophet_data)

#summary(prophet_model)
```



```{r prophet_predict}
# Generate future dates
future <- make_future_dataframe(prophet_model, periods = 12*5, freq = "month", include_history = TRUE)

# Predict future values
prophet_forecast <- predict(prophet_model, future)

```


```{r}
dyplot.prophet(prophet_model, prophet_forecast)
```




```{r prophet_predict}
# Plot the forecast with lockdown effects
plot(prophet_model, prophet_forecast) +
  add_changepoints_to_plot(prophet_model)

```


```{r}
prophet_plot_components(prophet_model, prophet_forecast)
```


```{r prophet_residuals}
# Generate residuals
prophet_residuals <- prophet_data$y - predict(prophet_model, prophet_data)$yhat

tsdisplay(prophet_residuals)
```


```{r prophet_residuals_arima}
prres.arima <- auto.arima(prophet_residuals)

summary(prres.arima)
fit.prres.arima<- fitted(prres.arima)

res.prres.arima<- residuals(prres.arima)
tsdisplay(res.prres.arima)
```


```{r prophet_residuals_arima}
prophet_fitted <- predict(prophet_model, prophet_data)

plot(ott_cinema_monthly$ds, ott_cinema_monthly$cinema_admissions, 
     type = "o", pch = 16, cex=0.6, 
     xlab = "Date", ylab = "Admissions")
lines(ott_cinema_monthly$ds, prophet_fitted$yhat, 
      col="orange2", lwd=2, )
lines(ott_cinema_monthly$ds, prophet_fitted$yhat + fit.prres.arima, 
      col="purple2", lwd=2)


```


```{r prophet_test}
# Create a residuals dataframe
residuals_df <- data.frame(ds = prophet_data$ds, residuals = prophet_residuals)

# Plot residuals over time
ggplot(residuals_df, aes(x = ds, y = residuals)) +
  geom_line() +
  ggtitle("Residuals Over Time") +
  xlab("Date") +
  ylab("Residuals") +
  theme_minimal()

# Check residual distribution
ggplot(residuals_df, aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  theme_minimal()

# ACF and PACF to check for autocorrelation
Acf(prophet_residuals, main = "ACF of Prophet Residuals")
Pacf(prophet_residuals, main = "PACF of Prophet Residuals")

```
!!! TODO: ACF LOOKS SIGNIFICANT IN THE FIRST 3 LAGS. HOW I CAN SOLVE THIS?





## Exponential Smoothing

###  Holt-Winters method (for Trend and Seasonality)

```{r hw_exponential}
hw.cinema_admissions<- hw(cinema_admissions, seasonal="additive")
# Replace negative lower bounds with zero
hw.cinema_admissions$lower[hw.cinema_admissions$lower < 0] <- 0

summary(hw.cinema_admissions)

autoplot(hw.cinema_admissions)+
  autolayer(fitted(hw.cinema_admissions), series="Holt-Winters' additive")+
  ylab("Cinema Admissions")+xlab("Year")
```

!!! TODO: I WANT TO COMPARE THE AIC'S OF ALL THE MODELS I TRIED TO COME UP WITH THE BEST MODEL. THEN I WANT TO USE SOME OF THE BEST MODELS TO FORECAST THE FUTURE OF CINEMA ADMISSIONS.



## Unbalanced Competition Regime Change Diachronic model (UCRCD)

Perform a competition study between OTT and Cinema

```{r minmax_UCRCD}
# Apply Min-Max Normalization
ott_subscriptions.norm <- min_max_scale(ott_cinema_annual$ott_subscriptions_M)
cinema_admissions.norm <- min_max_scale(ott_cinema_annual$cinema_admissions)
```



```{r UCRCD_normalized}
# Plot the normalized time series
plot(cinema_admissions.norm, type = "o", pch = 16, lty = 3, col = "blue", 
     xlab = "Year", ylab = "Normalized Values",
     main = "Normalized OTT Subscriptions and Cinema Admissions", ylim = c(0, 1))
points(ott_subscriptions.norm, type = "o", pch = 16, lty = 3, col = "red")
legend("left", legend = c("Cinema Admissions", "OTT Subscriptions"), 
       col = c("blue", "red"), lty = 3, pch = 16, cex=0.7)
```




Estimating initial parameters with Bass Model:

```{r initial_UCRCD_bass}
# Cinema Admissions
BM.cinema_admissions<-BM(ott_cinema_annual$cinema_admissions, display = T)
m.c = BM.cinema_admissions$Estimate[1, 1]     # Market potential for Cinema
p.c = BM.cinema_admissions$Estimate[2, 1]    # Innovation coefficient for Cinema
q.c = BM.cinema_admissions$Estimate[3, 1]    # Imitation coefficient for Cinema

# OTT Subscriptions
BM.ott_subscriptions<-BM(ott_cinema_annual$ott_subscriptions_M*1e6, display = T)
m.o = BM.ott_subscriptions$Estimate[1, 1]     # Market potential for OTT
p.o = BM.ott_subscriptions$Estimate[2, 1] # Innovation coefficient for OTT
q.o = BM.ott_subscriptions$Estimate[3, 1]  # Imitation coefficient for OTT

```

GBM for cinema admissions:
```{r GBM_cinema_annual}
# Cinema Admissions
# GBM exponential shock

# Define the time period when the COVID-19 shock begins
covid_start <- 2020 - 2006 + 1

# m,p,q, a(starting point), b(memory of the shock), c(intensity)
GBM.cinema_admissions<- GBM(ott_cinema_annual$cinema_admissions, shock = "exp", nshock = 1,
                     prelimestimates = c(m.c, p.c, q.c, covid_start, -0.5, -0.1)) 

summary(GBM.cinema_admissions)
checkresiduals(GBM.cinema_admissions)


m.c = BM.cinema_admissions$Estimate[1, 1]    # Market potential for Cinema
p.c = BM.cinema_admissions$Estimate[2, 1]    # Innovation coefficient for Cinema
q.c = BM.cinema_admissions$Estimate[3, 1]    # Imitation coefficient for Cinema
```

GGM for OTT subscriptions:
```{r GGM_ott_annual}
GGM.ott_subscriptions<- GGM(ott_cinema_annual$ott_subscriptions_M*1e6, 
                            prelimestimates=c(m.o, 0.01, 0.01, p.o, q.o), oos=10)
summary(GGM.ott_subscriptions)
checkresiduals(GGM.ott_subscriptions)
```



```{r UCRCD}
###estimate the UCRCD (with delta and gamma)
UCRCD.adm.subs<- UCRCD(
  series1 = ott_cinema_annual$cinema_admissions,  # Instantaneous cinema admissions
  series2 = ott_cinema_annual$ott_subscriptions_M*1e6,  # Instantaneous OTT subscriptions
  m1 = m.c,                    # Market potential for cinema (GBM result)
  p1c = p.c,                   # Innovation coefficient for cinema
  q1c = q.c,                   # Imitation coefficient for cinema
  m2 = m.o,                    # Market potential for OTT (BM result)
  p2 = p.o,                   # Innovation coefficient for OTT
  q2 = q.o                    # Imitation coefficient for OTT
)
summary(UCRCD.adm.subs)
```

!!! TODO: SINCE THERE IS A SCALE DIFFERENCE IN THE DATA, THE PLOT DOESNT EXPLAIN MUCH. CAN I WORK WITH NORMALIZED DATA WHEN MODELING UCRCD?



```{r initial_UCRCD_bass}
# Cinema Admissions
BM.cinema_admissions.norm<-BM(cinema_admissions.norm, display = T)
m.c = BM.cinema_admissions.norm$Estimate[1, 1]     # Market potential for Cinema
p.c = BM.cinema_admissions.norm$Estimate[2, 1]    # Innovation coefficient for Cinema
q.c = BM.cinema_admissions.norm$Estimate[3, 1]    # Imitation coefficient for Cinema

# OTT Subscriptions
BM.ott_subscriptions.norm<-BM(ott_subscriptions.norm, display = T)
m.o = BM.ott_subscriptions.norm$Estimate[1, 1]     # Market potential for OTT
p.o = BM.ott_subscriptions.norm$Estimate[2, 1] # Innovation coefficient for OTT
q.o = BM.ott_subscriptions.norm$Estimate[3, 1]  # Imitation coefficient for OTT

```

GBM for cinema admissions:
```{r GBM_cinema_annual}
# Cinema Admissions
# GBM exponential shock

# Define the time period when the COVID-19 shock begins
covid_start <- 2020 - 2006 + 1

# m,p,q, a(starting point), b(memory of the shock), c(intensity)
GBM.cinema_admissions.norm<- GBM(cinema_admissions.norm, shock = "exp", nshock = 1,
                     prelimestimates = c(m.c, p.c, q.c, covid_start, -0.5, -0.1)) 

summary(GBM.cinema_admissions.norm)
checkresiduals(GBM.cinema_admissions.norm)


m.c = BM.cinema_admissions.norm$Estimate[1, 1]    # Market potential for Cinema
p.c = BM.cinema_admissions.norm$Estimate[2, 1]    # Innovation coefficient for Cinema
q.c = BM.cinema_admissions.norm$Estimate[3, 1]    # Imitation coefficient for Cinema
```

GGM for OTT subscriptions:
```{r GGM_ott_annual}
GGM.ott_subscriptions.norm<- GGM(ott_subscriptions.norm, 
                            prelimestimates=c(m.o, 0.01, 0.01, p.o, q.o), oos=10)
summary(GGM.ott_subscriptions.norm)
checkresiduals(GGM.ott_subscriptions.norm)
```



```{r UCRCD}
###estimate the UCRCD (with delta and gamma)
UCRCD.adm.subs.norm<- UCRCD(
  series1 = cinema_admissions.norm,  # Instantaneous cinema admissions
  series2 = ott_subscriptions.norm,  # Instantaneous OTT subscriptions
  m1 = m.c,                    # Market potential for cinema (GBM result)
  p1c = p.c,                   # Innovation coefficient for cinema
  q1c = q.c,                   # Imitation coefficient for cinema
  m2 = m.o,                    # Market potential for OTT (BM result)
  p2 = p.o,                   # Innovation coefficient for OTT
  q2 = q.o                    # Imitation coefficient for OTT
)
summary(UCRCD.adm.subs.norm)
```


