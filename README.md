# Time Series Analysis of WTI Crude Oil Prices

This project presents a comprehensive time series analysis of **West Texas Intermediate (WTI) Crude Oil prices** from 2019 to early 2024. The analysis focuses on forecasting oil prices, modeling volatility, and understanding extreme price movements using various statistical and econometric models in R.

## 📌 Project Objectives

- Analyze and visualize the historical behavior of WTI crude oil prices
- Assess stationarity using ADF and KPSS tests
- Fit stationary and non-stationary models including ARIMA, ARCH, and GARCH
- Perform extreme value analysis using GEV distribution
- Forecast future prices and assess risk of extreme events

## 📊 Dataset

- **Source**: Yahoo Finance
- **Time Period**: January 2019 – January 2024
- **Ticker**: `CL=F` (WTI Crude Oil futures)

## 🛠️ Tools & Packages Used

- `quantmod` – for data acquisition
- `tseries` – for ADF, KPSS, and other stationarity tests
- `forecast` – for ARIMA modeling
- `fGarch` – for ARCH/GARCH modeling
- `extRemes` – for extreme value analysis
- `ggplot2` – for data visualization

## 📈 Models Implemented

- **Exploratory Data Analysis (EDA)**: Summary stats, histograms, boxplots
- **Stationarity Testing**: ADF, KPSS
- **ARIMA Modeling**: ARIMA(2,1,2) selected based on AIC and residual diagnostics
- **Volatility Modeling**:
  - ARCH(1)
  - GARCH(1,1) – Captured volatility clustering effectively
- **Extreme Value Analysis**:
  - Block Maxima Method
  - Fitted Generalized Extreme Value (GEV) distribution

## 🔍 Key Findings

- WTI crude oil prices are **non-stationary**, requiring differencing before modeling.
- The **ARIMA(2,1,2)** model produced minimal residual autocorrelation and was a good fit.
- **GARCH(1,1)** effectively modeled conditional variance and volatility clustering.
- **GEV distribution** allowed estimation of high-probability extreme values, useful for risk scenarios.

## 📁 Files in this Repository

- `wti_crude_analysis.R` – Main R script containing all code for data loading, EDA, modeling, and EVA
- `WTI_Crude_Oil_Report.pdf` – Final project report with visualizations, interpretations, and summaries
- `README.md` – This file

## 🚀 How to Run

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/wti-crude-analysis.git
