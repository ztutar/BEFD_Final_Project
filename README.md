# TIME SERIES ANALYSIS & FORECASTING OF CINEMA ADMISSIONS IN ITALY — Business, Economic & Financial Data Final Project

Final project for the course **Business, Economic and Financial Data**.  
This repository is intended to showcase an end-to-end applied workflow in **R**: data preparation, diagnostics, time-series forecasting, and diffusion/competition modeling using cinema vs OTT-related indicators.

---

## Project goal

Analyze the evolution of cinema admissions together with OTT (streaming) indicators for Italy, and evaluate:

- which economic/market factors help explain cinema activity,
- how well different time-series models forecast the target series,
- whether diffusion/competition models can describe adoption dynamics and regime changes over time.

---

## Repository structure

- `BEFD_FinalProject.Rmd` — Main analysis in **R Markdown** (knit-ready).
- `BEFD_FinalProject.R` — Script version of the workflow.
- `ott_cinema_dataset.xlsx` — Source dataset (monthly + annual sheets).
- `Project_Report.pdf` — Final report.
- `BEFD_Final_Project.Rproj` — RStudio project file.

---

## Data

### Input file
The analysis reads:

- `ott_cinema_dataset.xlsx`

using two sheets:

- `Monthly`
- `Annual`

### Time indexing
- Monthly series: **2006-01 to 2023-12** (frequency = 12)
- Annual series: **2006 to 2023** (frequency = 1)

---

## Methods

### 1) Preprocessing & imputation
- Missingness visualization
- Imputation rules implemented in code, including:
  - imputing missing monthly cinema totals for 2013 using surrounding-year monthly proportions
  - filling some COVID-era missing values using constant averages (as implemented)

### 2) Diagnostics
- Correlation analysis and visualization
- Multicollinearity checks via VIF

### 3) Forecasting models
- Time-series linear regression + stepwise selection
- ARIMA
- ARIMAx (ARIMA with exogenous regressors)
- Holt–Winters exponential smoothing
- Prophet (with custom holiday/lockdown effects) and Prophet + ARIMA refinement

### 4) Diffusion & competition modeling (DIMORA)
- Bass Model (BM)
- Generalized Bass Model (GBM) with shocks
- UCRCD model (Unbalanced Competition Regime Change Diachronic) for cinema vs OTT competition

---

## How to run

### Option A — RStudio (recommended)
1. Open `BEFD_Final_Project.Rproj`
2. Open `BEFD_FinalProject.Rmd`
3. Knit to PDF (requires a LaTeX installation such as TinyTeX)

Install TinyTeX (if needed):
```r
install.packages("tinytex")
tinytex::install_tinytex()
```

### Option B — Run the script
Run:
- `BEFD_FinalProject.R`

---

## Requirements

Install packages used in the analysis:

```r
install.packages(c(
  "readxl","lmtest","forecast","DIMORA","naniar","ggplot2",
  "corrplot","car","dplyr","tidyr","prophet","MLmetrics","knitr"
))
```

---

## Author

**Zeynep TUTAR**
