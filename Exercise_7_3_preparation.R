# set working directory to source file location
setwd(this.path::here())

# ChatGPT provided most of the commands in this file

library(tidyquant)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(xts)

sp500_data <- tq_index("SP500")
sp500_symbols <- sp500_data$symbol

# Define date range
start_date <- "2023-01-01"
end_date <- "2024-12-31"

# Download historical stock prices from Yahoo Finance
stock_prices <- tq_get(sp500_symbols, from = start_date, to = end_date, get = "stock.prices")

# Compute daily returns
stock_returns <- stock_prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(daily_return = log(adjusted / lag(adjusted))) %>%
  ungroup()

# Reshape data to wide format (each stock as a column)
returns_wide <- stock_returns %>%
  select(date, symbol, daily_return) %>%
  pivot_wider(names_from = symbol, values_from = daily_return)

# Convert to XTS object (remove date from columns and set it as index)
# and remove the first observation which is missing
returns_xts <- xts(returns_wide[,-1], order.by = returns_wide$date)[-1, ]

# Remove columns (stocks) with any missing values
returns <- returns_xts[, colSums(is.na(returns_xts)) == 0]

# Define the URL of the dataset
ff_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip"

# Define a temporary file path
temp_file <- tempfile()

# Download the ZIP file
download.file(ff_url, temp_file, mode = "wb")

# Unzip and find the CSV file
unzipped_files <- unzip(temp_file, list = TRUE)  # List contents of the ZIP
csv_file <- unzipped_files$Name[1]  # Extract the first file (usually the dataset)

# Unzip and read the CSV file
unzip(temp_file, files = csv_file, exdir = tempdir())  # Extract the CSV
ff_data <- read_csv(file.path(tempdir(), csv_file), skip = 3)  # Skip first 3 rows of metadata

# Rename columns
colnames(ff_data) <- c("date", "Mkt_RF", "SMB", "HML", "RMW", "CMA", "RF")

# Remove any blank or NA rows
ff_data <- ff_data %>%
  filter(!is.na(Mkt_RF))

# Convert the date column to Date format
ff_data <- ff_data %>%
  mutate(date = ymd(date))  # Convert YYYYMMDD to Date

# Convert to XTS
ff_xts <- xts(ff_data[,-1], order.by = ff_data$date)

# Consider only the dates before which we have returns, 
# and divide by 100 (as the original data is in percentages)
ff <- ff_xts[index(ff_xts) %in% index(returns), ] / 100

# Save the final dataset
save(returns, ff, file = "Data_Ex7_3.RData")
