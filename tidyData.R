
# data cleaning
library(tidyverse)
# Combine all weather dataframes into one dataframe
combined_weather_data <- bind_rows(state_data_list, .id = "state_id")


# # Add a new factor column for state using state_id
# combined_weather_data <- combined_weather_data %>%
#   mutate(state = factor(state_id, levels = 1:37, labels = state_coordinates$State[state_id]))

# Add a new factor column for state using state_id
state_coordinates <- state_coordinates %>%
  mutate(state_id = factor(c(1:37), levels = 1:37))

# Convert state_id to numeric
#combined_weather_data$state_id <- as.numeric(combined_weather_data$state_id)

# Add a new factor column for state using state_id
# combined_weather_data <- combined_weather_data %>%
#   mutate(state = factor(state_id, levels = 1:37))

# Now, you can use `state` for the join
merged_data <- left_join(combined_weather_data, state_coordinates, by = c("state_id" = "state_id"))

# # Now, you can use `state` for the join
# merged_data <- left_join(combined_weather_data, state_coordinates, by = c("state" = "state_id"))

# # Now, you can use `state` for the join
# merged_data <- left_join(combined_weather_data, state_coordinates, by = "state")
# # Merge with state_coordinates to add ecological zone information
# merged_data <- left_join(combined_weather_data, state_coordinates, by = c("state_id" = "State"))

# 
# # Function to set attributes based on original met data
# set_met_attributes <- function(df, original_met_data) {
#   # Set attributes based on original_met_data
#   attributes(df) <- attributes(original_met_data)
#   
#   # Return the data frame with attributes
#   return(df)
# }







# library(dplyr)

# # Get the unique ecological zones
# unique_zones <- levels(factor(merged_data$Ecological_Zone))
# 
# # Specify columns to remove
# columns_to_remove <- c("state_id", "State", "Ecological_Zone", "Latitude", "Longitude")
# 
# # Create a list to store data frames for each ecological zone
# zone_data_list <- list()
# 
# # Loop through each ecological zone and filter data
# for (zone in unique_zones) {
#   zone_data <- merged_data %>% filter(Ecological_Zone == zone) %>%
#     select(-one_of(columns_to_remove))
#   zone_data_list[[zone]] <- zone_data
# }
# 
# # # Access the data frames for each ecological zone
# # zone_data_NC <- zone_data_list[["NC"]]
# # zone_data_NE <- zone_data_list[["NE"]]
# # zone_data_NW <- zone_data_list[["NW"]]
# # zone_data_SE <- zone_data_list[["SE"]]
# # zone_data_SS <- zone_data_list[["SS"]]
# # zone_data_SW <- zone_data_list[["SW"]]
# 
# 

# saveRDS(zone_data_list, "data/zone_data_list.rds")

zone_data_list <- readRDS("data/zone_data_list.rds")

# Aggregate by ecological zone
aggregated_by_zone <- merged_data %>%
  group_by(Ecological_Zone, year) %>%
  summarize(
    radn = round(mean(radn, na.rm = TRUE), 2),
    maxt = round(mean(maxt, na.rm = TRUE), 2),
    mint = round(mean(mint, na.rm = TRUE), 2),
    rain = round(mean(rain, na.rm = TRUE), 2),
    rh = round(mean(rh, na.rm = TRUE), 2),
    windspeed = round(mean(windspeed, na.rm = TRUE), 2)
  )
# # %>%
# #   # Set attributes
# #   set_met_attributes(state_data_list[[1]])

# Aggregate for the entire Nigeria
aggregated_nigeria <- combined_weather_data %>%
  group_by(year) %>%
  summarize(
    #day = 1,
    radn = round(mean(radn, na.rm = TRUE), 2),
    maxt = round(mean(maxt, na.rm = TRUE), 2),
    mint = round(mean(mint, na.rm = TRUE), 2),
    rain = round(mean(rain, na.rm = TRUE), 2),
    rh = round(mean(rh, na.rm = TRUE), 2),
    windspeed = round(mean(windspeed, na.rm = TRUE), 2)
  )

#   
# # Calculate tav as the mean of maxt and mint
# tav_value <- mean(aggregated_nigeria$maxt + aggregated_nigeria$mint, na.rm = TRUE)
# 
# # Calculate amp as the maximum value of maxt
# amp_value <- max(aggregated_nigeria$maxt, na.rm = TRUE)
# 
# # Convert aggregated_nigeria to class 'met' with calculated tav and amp
# met_aggregated_nigeria <- as_apsim_met(
#   x = aggregated_nigeria,
#   filename = "aggregated_nigeria.met",
#   site = "nigeria_site",  # You can customize the site name
#   latitude = 9.0820,  # Replace with the actual latitude of the site
#   longitude = 8.6753,  # Replace with the actual longitude of the site
#   tav = tav_value,
#   amp = amp_value,
#   colnames = c("year", "day", "radn", "maxt", "mint", "rain", "rh", "windspeed"),
#   units = c("()", "()", "(MJ/m2/day)", "(oC)", "(oC)", "(mm)"),
#   constants = NA,
#   comments = "Aggregated met data for Nigeria",
#   check = TRUE
# )



# %>%
#   # Set attributes
#   set_met_attributes(state_data_list[[1]])
