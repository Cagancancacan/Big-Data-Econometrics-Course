# The zoo package allows for easy handling of time series
library(zoo)
# bootUR has a simple way to create appropriate differences
library(bootUR)

# Download and read in the data
FRED_MD <- read.csv('2026-10-MD.csv')
# Extract the transformation codes
trans_codes <- FRED_MD[1, -1]
# Extract the data (we delete the first row)
data_fred <- FRED_MD[-1, -1]
# Read in the dates and transform them to dates in zoo format
fred_date <- as.yearmon(FRED_MD[-1, 1], "%m/%d/%Y")
# Transform data into a time series 
fred_tseries <- zoo(data_fred, fred_date)
# Record the number of variables
N_fred <- ncol(fred_tseries)

# Although McCracken and Ng Suggest seconds differences of price series,
# most researchers actually use first differences. We are going to do the same.
# The first differences of log prices correspond to inflation,
# which makes things easier to interpret.
# However, that does mean that we need to make some adjustments to the transformation codes.
trans_codes[100:121] <- 5

# Take logs of the relevant series
fred_tseries[, trans_codes %in% 4:6] <- log(fred_tseries[, trans_codes %in% 4:6])
# Create a vector that says how often we need differ each series
n_diff <- 1 * (trans_codes %in% c(2, 5)) + 2 * (trans_codes %in% c(3, 6))

# Use the function in bootUR to create the appropriate differences
fred_diff <- diff_mult(fred_tseries, n_diff)

# For the sake of the exercise, we are only going to consider data from 1995 on
# Restrict data to January 1995 through January 2022
fred_final <- fred_diff[index(fred_diff) >= "jan 1995" & index(fred_diff) <= "jan 2022"]

# Let's check if there are still missing data
bootUR::plot_missing_values(fred_final)
# So there are still some missing data. 
# Let us delete the two series that have missing values in the middle,
where_missing <- bootUR::check_missing_insample_values(fred_final)
fred_final <- fred_final[, !where_missing]
# and remove the last months to get rid of the missing values at the end of the sample.
while (anyNA(fred_final)) {
  fred_final <- fred_final[-nrow(fred_final), ]
}
# We have now taken care of the missing values:
bootUR::plot_missing_values(fred_final)

# Now you can start your forecasting analysis!
