library(apsimx)
library(tidyverse)

# lat <- 7.250395934	
# lon <- 5.199982054
# 
# #get_gsod_apsim_met(lonlat = c(lon,lat), dates = c("1990-01-01", "2020-12-31"), fillin.radn = TRUE)
# 
# ames_power <- get_power_apsim_met(lonlat = c(lon,lat), dates = c("1990-01-01", "2020-12-31"))
# 
# summary(ames_power)
# 
# plot(ames_power, years = 1990:2000, cumulative = TRUE, climatology = TRUE, met.var = "rain")
# plot(ames_power, years = 2001:2010, cumulative = TRUE, climatology = TRUE, met.var = "rain")
# plot(ames_power, years = 2011:2020, cumulative = TRUE, climatology = TRUE, met.var = "rain")
# 
# 
# 
# # Create a dataframe with state, ecological zone, latitude, and longitude
# state_coordinates <- data.frame(
#   State = c("Benue", "Kogi", "Kwara", "Nassarawa", "Niger", "Plateau", "FCT", 
#             "Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe", 
#             "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara", 
#             "Abia", "Anambra", "Ebonyi", "Enugu", "Imo", 
#             "Akwa Ibom", "Cross River", "Bayelsa", "Delta", "Edo", "Rivers", 
#             "Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"),
#   Ecological_Zone = c("NC", "NC", "NC", "NC", "NC", "NC", "NC", 
#                       "NE", "NE", "NE", "NE", "NE", "NE", 
#                       "NW", "NW", "NW", "NW", "NW", "NW", "NW", 
#                       "SE", "SE", "SE", "SE", "SE", 
#                       "SS", "SS", "SS", "SS", "SS", "SS", 
#                       "SW", "SW", "SW", "SW", "SW", "SW"),
#   Latitude = c(7.4307, 7.8010, 8.4616, 8.4800, 9.0820, 9.2389, 9.0579, 
#                9.2094, 10.3159, 11.8402, 10.2897, 8.9806, 12.4593, 
#                11.7642, 10.5222, 11.8315, 12.9855, 12.4588, 13.0670, 12.1642, 
#                5.4309, 6.2579, 6.4351, 6.4483, 5.7990, 
#                4.9431, 5.0704, 4.8951, 5.6964, 6.4311, 4.8484, 
#                7.6713, 6.5244, 7.1604, 7.2633, 7.6298, 7.3775),
#   Longitude = c(8.8666, 6.7400, 4.5481, 8.5200, 6.0088, 9.7320, 7.4951, 
#                 12.4818, 9.8247, 13.1539, 11.1673, 10.1609, 11.4059, 
#                 9.3369, 7.4383, 8.4875, 7.6171, 4.2006, 5.2425, 6.6604, 
#                 7.5247, 7.2854, 8.0333, 7.5464, 7.0994, 
#                 7.8367, 8.0325, 6.2643, 5.9658, 5.8542, 7.0036, 
#                 5.3380, 3.3792, 3.3487, 5.1974, 4.1880, 3.9470)
# )

# Print the dataframe
#print(state_coordinates)

# Assuming state_coordinates is the dataframe with state and ecological zone information
#for (i in 1:nrow(state_coordinates)) {
#  state_data <- get_power_apsim_met(lonlat = c(state_coordinates$Longitude[i], state_coordinates$Latitude[i]),
#                                    dates = c("1990-01-01", "2020-12-31"))
  # Process or save the data for the current state/ecological zone
#}


# Create an empty list to store data frames
# state_data_list <- list()

# Assuming state_coordinates is the dataframe with state and ecological zone information
#for (i in 1:nrow(state_coordinates)) {
#  state_data <- get_power_apsim_met(lonlat = c(state_coordinates$Longitude[i], state_coordinates$Latitude[i]),
#                                    dates = c("1990-01-01", "2020-12-31"))
  # Store the data frame in the list
#  state_data_list[[i]] <- state_data
#}

# Now, state_data_list contains the data frames for each state or ecological zone

# Save the list of data frames to an RDS file
#saveRDS(state_data_list, "data/state_data_list.rds")


# Load the list of data frames from the RDS file
state_data_list <- readRDS("data/state_data_list.rds")

# saveRDS(state_coordinates, "data/state_coordinates.rds")

state_coordinates <- readRDS("data/state_coordinates.rds")


plot(state_data_list[[2]], years = 2010:2020, cumulative = TRUE, climatology = TRUE, met.var = "rain") + theme_minimal()

plot(state_data_list[[2]], years = 2010:2020, cumulative = TRUE, climatology = TRUE)

# saveRDS(Maize_data, "data/maize_data.rds")
maize_data <- readRDS("data/maize_data.rds")


# 
# # Create an empty list to store data frames
# state_soil_list <- list()
# 
# # Assuming state_coordinates is the dataframe with state and ecological zone information
# for (i in 1:nrow(state_coordinates)) {
#  state_soil <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[i], state_coordinates$Latitude[i]))
# #Store the data frame in the list
#  state_soil_list[[i]] <- state_soil
# }
# 
# 


# ams.sgrds <- get_isric_soil_profile(lonlat = c(7.8010
#                                                ,6.7400))
# 
# plot(ams.sgrds, property = "water")
# 
# 
# 
# state_soil_list <- list()
# 
# for (i in 1:nrow(state_coordinates)) {
#   tryCatch(
#     {
#       state_soil <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[i], state_coordinates$Latitude[i]))
#       state_soil_list[[i]] <- state_soil
#     },
#     error = function(e) {
#       cat("Error for coordinates:", state_coordinates$Longitude[i], state_coordinates$Latitude[i], "\n")
#       state_soil_list[[i]] <- NULL  # or any other action to handle the error
#     }
#   )
# }
# 
# 
# 
# state_soil_list <- list()
# 
# for (i in 1:nrow(state_coordinates)) {
#   tryCatch(
#     {
#       state_soil <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[i], state_coordinates$Latitude[i]))
#       state_soil_list[[i]] <- state_soil
#     },
#     error = function(e) {
#       cat("Error for coordinates:", state_coordinates$Longitude[i], state_coordinates$Latitude[i], "\n")
#       state_soil_list[[i]] <- NULL  # Set to NULL or handle the error accordingly
#     }
#   )
# }
# 
# 
# 
# # Create a list of coordinates
# coordinates_list <- split(state_coordinates[, c("Longitude", "Latitude")], seq(nrow(state_coordinates)))
# 
# # Use lapply to retrieve soil data for each set of coordinates
# state_soil_list <- lapply(coordinates_list, function(coord) {
#   get_isric_soil_profile(lonlat = coord)
# })
# 
# 
# 
# # Create an empty list to store data frames
# state_soil_list <- list()
# 
# # Retrieve soil data for each set of coordinates
# state_soil_list[[7]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[7], state_coordinates$Latitude[7]))
# # Create an empty list to store data frames
# state_soil_list <- vector("list", length = 37)
# 
# # Retrieve soil data for each set of coordinates for indices 7 to 37
# state_soil_list[[7]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[7], state_coordinates$Latitude[7]))
# state_soil_list[[8]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[8], state_coordinates$Latitude[8]))
# state_soil_list[[9]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[9], state_coordinates$Latitude[9]))
# state_soil_list[[10]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[10], state_coordinates$Latitude[10]))
# state_soil_list[[11]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[11], state_coordinates$Latitude[11]))
# state_soil_list[[12]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[12], state_coordinates$Latitude[12]))
# state_soil_list[[13]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[13], state_coordinates$Latitude[13]))
# state_soil_list[[14]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[14], state_coordinates$Latitude[14]))
# state_soil_list[[15]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[15], state_coordinates$Latitude[15]))
# state_soil_list[[16]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[16], state_coordinates$Latitude[16]))
# state_soil_list[[17]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[17], state_coordinates$Latitude[17]))
# state_soil_list[[18]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[18], state_coordinates$Latitude[18]))
# state_soil_list[[19]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[19], state_coordinates$Latitude[19]))
# state_soil_list[[20]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[20], state_coordinates$Latitude[20]))
# state_soil_list[[21]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[21], state_coordinates$Latitude[21]))
# state_soil_list[[22]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[22], state_coordinates$Latitude[22]))
# state_soil_list[[23]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[23], state_coordinates$Latitude[23]))
# state_soil_list[[24]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[24], state_coordinates$Latitude[24]))
# state_soil_list[[25]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[25], state_coordinates$Latitude[25]))
# state_soil_list[[26]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[26], state_coordinates$Latitude[26]))
# state_soil_list[[27]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[27], state_coordinates$Latitude[27]))
# state_soil_list[[28]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[28], state_coordinates$Latitude[28]))
# state_soil_list[[29]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[29], state_coordinates$Latitude[29]))
# state_soil_list[[30]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[30], state_coordinates$Latitude[30]))
# state_soil_list[[31]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[31], state_coordinates$Latitude[31]))
# state_soil_list[[32]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[32], state_coordinates$Latitude[32]))
# state_soil_list[[33]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[33], state_coordinates$Latitude[33]))
# state_soil_list[[34]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[34], state_coordinates$Latitude[34]))
# state_soil_list[[35]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[35], state_coordinates$Latitude[35]))
# state_soil_list[[36]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[36], state_coordinates$Latitude[36]))
# state_soil_list[[37]] <- get_isric_soil_profile(lonlat = c(state_coordinates$Longitude[37], state_coordinates$Latitude[37]))
# 
# state_soil_list[[37]] <- NULL

# saveRDS(state_soil_list, "data/state_soil_list.rds")
state_soil_list <- read_rds("data/state_soil_list.rds")

# Print any errors
# for (i in seq_along(state_soil_list)) {
#   if (inherits(state_soil_list[[i]], "try-error")) {
#     cat("Error for coordinates:", state_coordinates$Longitude[i], state_coordinates$Latitude[i], "\n")
#   }
# }



# library(purrr)
# 
# # Create an empty list to store data frames
# state_soil_list2 <- vector("list", length = 37)
# 
# # Retrieve soil data for each set of coordinates from 7 to 37 using map
# state_soil_list2[1:37] <- map(1:37, ~ {
#   coord <- c(state_coordinates$Longitude[.x], state_coordinates$Latitude[.x])
#   get_isric_soil_profile(lonlat = coord)
# })



