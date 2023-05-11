Code for Final Project 

library(ggplot2)
library(ggfortify)
library(forecast)
library(quantmod)
library(tidyverse)
library(fredr)
library(vars)
library(magrittr)
library(dplyr)

fredr_set_key("a48e87969af3619e5b90795f1f47705d")
cpi_data <- fredr(series_id = "CPILFESL", observation_start = as.Date("2002-12-01"), observation_end = as.Date("2022-12-01"))
#2002 to 2022 CPI for Urban Consumers (FRED) less food and energy (Normal CPI)
ggplot(data = cpi_data, aes(x = date, y = value)) +
  geom_line() +
  labs(x = "Year", y = "CPI value", title = "Consumer Price Index for All Urban Consumers") +
  theme_minimal()
  
# Specify the variables of interest and date range
variables <- c("CPILFESL", "CPIAPPSL", "CPIENGSL", "CPIGAS", "CPISHEL", "CPITRANSPORT")
start_date <- "2002-01-01"
end_date <- "2022-12-31"

# Retrieve data from FRED and combine into a single data frame
cpi_data2 <- fredr_series_observations(variables, start_date, end_date, frequency = "M") %>%
  pivot_wider(names_from = series_id, values_from = value)
  
# Calculate average CPI and store in a new column called "avg_cpi"
# This is to use to compare original data to weighted
cpi_data <- cpi_data %>%
  mutate(avg_cpi = rowMeans(select(., CPILFESL:CPITRANSPORT)))

# Plot average CPI over time
ggplot(cpi_data, aes(x = date, y = avg_cpi)) +
  geom_line() +
  labs(title = "Consumer Price Index", y = "Index", color = "Variable") +
  theme_bw()


# Plot data showing weighted values
ggplot(cpi_data, aes(x = date)) +
  geom_line(aes(y = CPILFESL, color = "CPILFESL")) +
  geom_line(aes(y = CPIAPPSL, color = "CPIAPPSL")) +
  geom_line(aes(y = CPIENGSL, color = "CPIENGSL")) +
  geom_line(aes(y = CPIGAS, color = "CPIGAS")) +
  geom_line(aes(y = CPISHEL, color = "CPISHEL")) +
  geom_line(aes(y = CPITRANSPORT, color = "CPITRANSPORT")) +
  labs(title = "Consumer Price Index", y = "Index", color = "Variable") +
  scale_color_manual(values = c("CPILFESL" = "red", "CPIAPPSL" = "blue", "CPIENGSL" = "green", 
                                "CPIGAS" = "purple", "CPISHEL" = "orange", "CPITRANSPORT" = "pink")) +
  theme_bw()
  
  # Now observe both side by side
  # Create a line plot with two lines: one for the normal CPI and one for the weighted CPI
                 ggplot(cpi_data, aes(x = year)) +
                   geom_line(aes(y = cpi, color = "Normal CPI")) +
                   geom_line(aes(y = weighted_cpi, color = "Weighted CPI")) +
                   scale_color_manual(name = "CPI Type", values = c("Normal CPI" = "red", "Weighted CPI" = "blue")) +
                   xlab("Year") +
                   ylab("CPI Value") +
                   ggtitle("Normal CPI vs Weighted CPI (2002-2022)") +
                   theme(plot.title = element_text(hjust = 0.5))
                   
  #VAR Model
  # Set the weights for the VAR model
weights <- matrix(c(0.5, 0.2, 0.1, 0.1, 0.05, 0.05,
                    0.2, 0.5, 0.1, 0.1, 0.05, 0.05,
                    0.1, 0.1, 0.5, 0.1, 0.15, 0.05,
                    0.1, 0.1, 0.1, 0.5, 0.15, 0.05,
                    0.05, 0.05, 0.15, 0.15, 0.5, 0.1,
                    0.05, 0.05, 0.05, 0.05, 0.1, 0.5), nrow = 6, byrow = TRUE)

# Specify the variables for the VAR model
var_data <- cpi_data[, c("CPILFESL", "CPIAPPSL", "CPIENGSL", "CPIGAS", "CPISHEL", "CPITRANSPORT")]

# Fit the VAR model
var_model <- VAR(var_data, p = 1, type = "const", season = NULL, exogen = NULL, include = NULL, 
                 omit = NULL, output = FALSE, return.fitted = FALSE, 
                 include.mean = FALSE, method = "ols", 
                 estimation = c("ols", "ml"), 
                 LRinclude = NULL, dumvar = NULL, lag.max = NULL, 
                 ic = c("AIC", "HQ", "SC", "FPE", "ODP"), 
                 trend = NULL, selectlags = NULL, 
                 start.method = c("const", "R", "R2", "CI", "SC"),
                 y.name = NULL, date = NULL, ...)

# Get the summary of the VAR model
summary(var_model, equation = "CPILFESL")

tibble(summary(cpi_data))

