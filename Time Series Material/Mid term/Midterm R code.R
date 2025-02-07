install.packages("fredr")
# Load the fredr package
library(fredr)

# Set your FRED API key
fredr_set_key("YOUR_API_KEY")

# Fetch the data for each series

# 1. Consumer Price Index (CPI) - Monthly
cpi_data <- fredr(series_id = "CPIAUCSL",
                  observation_start = as.Date("2015-01-01"),  # Adjust to your time range
                  frequency = "m")  # Monthly

# 2. Mortgage Interest Rates - Monthly
mortgage_data <- fredr(series_id = "MORTGAGE30US",
                       observation_start = as.Date("2015-01-01"),  # Adjust as needed
                       frequency = "m")  # Monthly

# 3. Housing Price Index (HPI) - Monthly
hpi_data <- fredr(series_id = "CSUSHPINSA",
                  observation_start = as.Date("2015-01-01"),  # Adjust as needed
                  frequency = "m")  # Monthly

# 4. Gross Domestic Product (GDP) - Quarterly (interpolate later)
gdp_data <- fredr(series_id = "GDP",
                  observation_start = as.Date("2015-01-01"),  # Adjust as needed
                  frequency = "q")  # Quarterly data

# Convert quarterly GDP data to monthly using linear interpolation
gdp_monthly <- approx(gdp_data$date, gdp_data$value, 
                      xout = seq(min(gdp_data$date), max(gdp_data$date), by = "month"))  # Linear interpolation

# Convert interpolated GDP into a data frame
gdp_monthly_df <- data.frame(date = as.Date(gdp_monthly$x), gdp = gdp_monthly$y)

# View the data
head(cpi_data)
head(mortgage_data)
head(hpi_data)
head(gdp_monthly_df)

# Save data to CSV files if needed
write.csv(cpi_data, "cpi_data.csv", row.names = FALSE)
write.csv(mortgage_data, "mortgage_data.csv", row.names = FALSE)
write.csv(hpi_data, "hpi_data.csv", row.names = FALSE)
write.csv(gdp_monthly_df, "gdp_monthly.csv", row.names = FALSE)



Qn5.
install.packages("tseries")  # For ADF test
install.packages("forecast") # For visualization and tools
library(tseries)
library(forecast)
# Load necessary libraries for date handling and plotting
library(ggplot2)
library(scales)

# 1. Plot GDP (Monthly)
ggplot(gdp_monthly_df, aes(x = date, y = gdp)) +
  geom_line(color = "blue") +
  labs(title = "GDP Time Series (Monthly)", x = "Date", y = "GDP") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Plot CPI (Monthly)
ggplot(cpi_data, aes(x = date, y = value)) +
  geom_line(color = "green") +
  labs(title = "CPI Time Series (Monthly)", x = "Date", y = "CPI") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Plot Mortgage Interest Rates (Monthly)
ggplot(mortgage_data, aes(x = date, y = value)) +
  geom_line(color = "red") +
  labs(title = "Mortgage Interest Rates Time Series (Monthly)", x = "Date", y = "Mortgage Rates (%)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Plot Housing Price Index (HPI) (Monthly)
ggplot(hpi_data, aes(x = date, y = value)) +
  geom_line(color = "purple") +
  labs(title = "Housing Price Index (HPI) Time Series (Monthly)", x = "Date", y = "HPI") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 1. ADF Test for GDP
adf_gdp <- adf.test(gdp_monthly_df$gdp, alternative = "stationary")
print(adf_gdp)

# 2. ADF Test for CPI
adf_cpi <- adf.test(cpi_data$value, alternative = "stationary")
print(adf_cpi)

# 3. ADF Test for Mortgage Interest Rates
adf_mortgage <- adf.test(mortgage_data$value, alternative = "stationary")
print(adf_mortgage)

# 4. ADF Test for Housing Price Index (HPI)
adf_hpi <- adf.test(hpi_data$value, alternative = "stationary")
print(adf_hpi)



