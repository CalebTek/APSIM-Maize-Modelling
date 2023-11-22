# Set seed for reproducibility
set.seed(123)

# Simulation parameters
years <- 2021:2030
historical_years <- 1990:2020  # Assuming historical data is available up to 2020
sigma_temp <- 2
sigma_rain <- 5

# Function to simulate meteorological data
simulate_meteorological_data <- function(historical_data, sigma) {
  simulated_data <- historical_data[1] + cumsum(rnorm(length(historical_data) - 1, mean = 0, sd = sigma))
  return(round(abs(simulated_data),2))  # Ensure non-negativity
}

# Simulate meteorological data for each variable
simulated_maxt <- simulate_meteorological_data(aggregated_nigeria$maxt, sigma_temp)
simulated_mint <- simulate_meteorological_data(aggregated_nigeria$mint, sigma_temp)
simulated_rain <- simulate_meteorological_data(aggregated_nigeria$rain, sigma_rain)

# Function to simulate maize yield
simulate_maize_yield <- function(radn, maxt, mint, rain, rh, windspeed) {
  yield <- radn * (maxt - mint) * (1 - exp(-rain)) * rh * abs(windspeed)  # Ensure non-negativity
  return(round(abs(yield)))  # Ensure non-negativity
}

# Simulate maize yield for each year
simulated_yield <- simulate_maize_yield(
  radn = simulate_meteorological_data(aggregated_nigeria$radn, 2),
  maxt = simulated_maxt,
  mint = simulated_mint,
  rain = simulated_rain,
  rh = simulate_meteorological_data(aggregated_nigeria$rh, 2),
  windspeed = simulate_meteorological_data(aggregated_nigeria$windspeed, 2)
)

# Combine simulated data into a new data frame
simulated_data <- data.frame(
  year = years,
  radn = simulate_meteorological_data(aggregated_nigeria$radn, 2),
  maxt = simulated_maxt,
  mint = simulated_mint,
  rain = simulated_rain,
  rh = simulate_meteorological_data(aggregated_nigeria$rh, 2),
  windspeed = simulate_meteorological_data(aggregated_nigeria$windspeed, 2),
  aggregated_yield_nigeria = simulated_yield
)

# Display the simulated data
print(simulated_data)









# Set seed for reproducibility
set.seed(123)

# Simulation parameters
years <- 2021:2030
historical_years <- 1990:2020  # Assuming historical data is available up to 2020
sigma_temp <- 2
sigma_rain <- 5

# Function to simulate meteorological data
simulate_meteorological_data <- function(historical_data, sigma) {
  simulated_data <- historical_data[1] + cumsum(rnorm(length(historical_data) - 1, mean = 0, sd = sigma))
  return(simulated_data)
}

# Simulate meteorological data for each variable
simulated_maxt <- simulate_meteorological_data(aggregated_nigeria$maxt, sigma_temp)
simulated_mint <- simulate_meteorological_data(aggregated_nigeria$mint, sigma_temp)
simulated_rain <- simulate_meteorological_data(aggregated_nigeria$rain, sigma_rain)

# Function to simulate maize yield
simulate_maize_yield <- function(radn, maxt, mint, rain, rh, windspeed) {
  yield <- radn * (maxt - mint) * (1 - exp(-rain)) * rh * windspeed
  return(yield)
}

# Simulate maize yield for each year
simulated_yield <- simulate_maize_yield(
  radn = simulate_meteorological_data(aggregated_nigeria$radn, 2),
  maxt = simulated_maxt,
  mint = simulated_mint,
  rain = simulated_rain,
  rh = simulate_meteorological_data(aggregated_nigeria$rh, 2),
  windspeed = simulate_meteorological_data(aggregated_nigeria$windspeed, 2)
)

# Combine simulated data into a new data frame
simulated_data <- data.frame(
  year = years,
  radn = simulate_meteorological_data(aggregated_nigeria$radn, 2),
  maxt = simulated_maxt,
  mint = simulated_mint,
  rain = simulated_rain,
  rh = simulate_meteorological_data(aggregated_nigeria$rh, 2),
  windspeed = simulate_meteorological_data(aggregated_nigeria$windspeed, 2),
  aggregated_yield_nigeria = simulated_yield
)

# Display the simulated data
print(simulated_data)



# # Function to simulate met data
# simulate_met_data <- function(historical_data) {
#   simulated_data <- historical_data  # Copy historical data
#   
#   # Simulate met data for each year from 2021 to 2030
#   for (year in 2021:2030) {
#     simulated_data <- rbind(simulated_data, mutate(simulated_data %>% filter(year == (year - 1)),
#                                                    year = year,
#                                                    day = day + 1,
#                                                    radn = mean(radn) + rnorm(1, mean = 0, sd = 2),
#                                                    maxt = mean(maxt) + rnorm(1, mean = 0, sd = 2),
#                                                    mint = mean(mint) + rnorm(1, mean = 0, sd = 2),
#                                                    rain = mean(rain) + rnorm(1, mean = 0, sd = 5),
#                                                    rh = mean(rh) + rnorm(1, mean = 0, sd = 5),
#                                                    windspeed = mean(windspeed) + rnorm(1, mean = 0, sd = 1)
#     ))
#   }
#   
#   return(simulated_data)
# }
# 
# # Simulate met data for the years 2021-2030
# simulated_met_data <- simulate_met_data(ames_power)
# 
# # Print the first few rows of simulated met data
# head(simulated_met_data)


# # Function to simulate met data
# simulate_met_data <- function(historical_data) {
#   # Copy historical data
#   simulated_data <- historical_data
#   
#   # Simulate met data for each year from 2021 to 2030
#   for (year in 2021:2030) {
#     # Filter data for the previous year
#     prev_year_data <- filter(simulated_data, year == (year - 1))
#     
#     # Simulate and append a new row
#     simulated_data <- rbind(simulated_data, mutate(prev_year_data,
#                                                    year = year,
#                                                    day = day + 1,
#                                                    radn = mean(radn) + rnorm(1, mean = 0, sd = 2),
#                                                    maxt = mean(maxt) + rnorm(1, mean = 0, sd = 2),
#                                                    mint = mean(mint) + rnorm(1, mean = 0, sd = 2),
#                                                    rain = mean(rain) + rnorm(1, mean = 0, sd = 5),
#                                                    rh = mean(rh) + rnorm(1, mean = 0, sd = 5),
#                                                    windspeed = mean(windspeed) + rnorm(1, mean = 0, sd = 1)
#     ))
#   }
#   
#   return(simulated_data)
# }
# 
# # Simulate met data for the years 2021-2030 using historical data (ames_power$noname.met)
# simulated_met_data <- simulate_met_data(ames_power)
# 
# # Print the first few rows of simulated met data
# head(simulated_met_data)



# # Function to simulate met data
# simulate_met_data <- function(historical_data) {
#   # Copy historical data
#   simulated_data <- historical_data
#   
#   # Get the last available data from 2020
#   last_data_2020 <- filter(simulated_data, year == 2020)
#   
#   # Simulate met data for each year from 2021 to 2030
#   for (year in 2021:2030) {
#     # Simulate and append a new row based on the last available data
#     simulated_data <- rbind(simulated_data, mutate(last_data_2020,
#                                                    year = year,
#                                                    day = day + 1,
#                                                    radn = mean(radn) + rnorm(1, mean = 0, sd = 2),
#                                                    maxt = mean(maxt) + rnorm(1, mean = 0, sd = 2),
#                                                    mint = mean(mint) + rnorm(1, mean = 0, sd = 2),
#                                                    rain = mean(rain) + rnorm(1, mean = 0, sd = 5),
#                                                    rh = mean(rh) + rnorm(1, mean = 0, sd = 5),
#                                                    windspeed = mean(windspeed) + rnorm(1, mean = 0, sd = 1)
#     ))
#   }
#   
#   return(simulated_data)
# }
# 
# # Simulate met data for the years 2021-2030 using historical data (ames_power)
# simulated_met_data <- simulate_met_data(ames_power)
# 
# # Print the first few rows of simulated met data
# head(simulated_met_data)



# # Function to simulate met data
# simulate_met_data <- function(historical_data) {
#   # Get the last available data from 2020
#   last_data_2020 <- filter(historical_data, year == 2020)
#   
#   # Create a new simulated_data data frame
#   simulated_data <- data.frame(
#     year = numeric(0),
#     day = numeric(0),
#     radn = numeric(0),
#     maxt = numeric(0),
#     mint = numeric(0),
#     rain = numeric(0),
#     rh = numeric(0),
#     windspeed = numeric(0)
#   )
#   
#   # Simulate met data for each year from 2021 to 2030
#   for (year in 2021:2030) {
#     # Simulate and append a new row based on the last available data
#     simulated_data <- rbind(simulated_data, mutate(last_data_2020,
#                                                    year = year,
#                                                    day = day + 1,
#                                                    radn = mean(radn) + rnorm(1, mean = 0, sd = 2),
#                                                    maxt = mean(maxt) + rnorm(1, mean = 0, sd = 2),
#                                                    mint = mean(mint) + rnorm(1, mean = 0, sd = 2),
#                                                    rain = mean(rain) + rnorm(1, mean = 0, sd = 5),
#                                                    rh = mean(rh) + rnorm(1, mean = 0, sd = 5),
#                                                    windspeed = mean(windspeed) + rnorm(1, mean = 0, sd = 1)
#     ))
#   }
#   
#   return(simulated_data)
# }
# 
# # Simulate met data for the years 2021-2030 using historical data (ames_power)
# simulated_data <- simulate_met_data(ames_power)
# 
# # Convert simulated_data to APSIM met datatype
# simulated_apsim_met <- as_apsim_met(simulated_data, filename = "simulated_met_data.met")
# 
# # Check the resulting met file using check_apsim_met
# check_apsim_met(simulated_apsim_met)







# Function to simulate met data
simulate_met_data <- function(historical_data) {
  # Get the last available data from 2020
  last_data_2020 <- filter(historical_data, year == 2020)
  
  # Create a new simulated_data data frame
  simulated_data <- data.frame(
    year = numeric(0),
    day = numeric(0),
    radn = numeric(0),
    maxt = numeric(0),
    mint = numeric(0),
    rain = numeric(0),
    rh = numeric(0),
    windspeed = numeric(0)
  )
  
  # Simulate met data for each year from 2021 to 2030
  for (year in 2021:2030) {
    # Simulate and append a new row based on the last available data
    simulated_data <- rbind(simulated_data, mutate(last_data_2020,
                                                   year = year,
                                                   day = day + 1,
                                                   radn = mean(radn) + rnorm(1, mean = 0, sd = 2),
                                                   maxt = mean(maxt) + rnorm(1, mean = 0, sd = 2),
                                                   mint = mean(mint) + rnorm(1, mean = 0, sd = 2),
                                                   rain = mean(rain) + rnorm(1, mean = 0, sd = 5),
                                                   rh = mean(rh) + rnorm(1, mean = 0, sd = 5),
                                                   windspeed = mean(windspeed) + rnorm(1, mean = 0, sd = 1)
    ))
  }
  
  return(simulated_data)
}

# Simulate met data for the years 2021-2030 using historical data (ames_power)
simulated_data <- simulate_met_data(ames_power)

# Convert simulated_data to APSIM met datatype with correct column names
simulated_apsim_met <- as_apsim_met(simulated_data, filename = "simulated_met_data.met",
                                    colnames = c("year", "day", "radn", "maxt", "mint", "rain"))

# Check the resulting met file using check_apsim_met
check_apsim_met(simulated_apsim_met)

