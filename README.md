# Macro Theory Project 3

This the R code for Macro Theory Project 3. This project examines fluctuations, economic crises, and firm responses to compare economic cycles across multiple countries.

## Requirements
The following packages need to be installed before running the script:
 - tseries
 - dplyr
 - forecast
 - lmtest
 - urca
 - dynlm
 - zoo
 - TTR
 - ggplot2

## Part 1
### Cross-Country Business Cycle Analysis

 1. Convert raw data into a time series data model starting in 1991.
 2. Extract data for each country into separate subsets.
 3. Calculate the difference of each data variable for each country and use the adf.test function to determine stationarity. (Assuming "intord fuctition" meant adf.test)
 4. Create data frames for each country individually, using the differenced (stationary) data, and run a correlation matrix on each country's dataset.

## Part 2
### Firm-Level Responses Using WRDS Compustat Data

 1. Create variables from the dataset.
 2. Define the Global Financial Crisis (GFC) shading period.
 3. Create a Capital Expenditure plot for all firms with GFC shading.
 4. Create a Revenue plot for all firms with GFC shading.
 5. Create a Debt-to-Equity Ratio plot for all firms with GFC shading.
