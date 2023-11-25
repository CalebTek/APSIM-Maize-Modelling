# Define state indices and corresponding zones
state_indices <- c(6, 10, 16, 24, 31, 33)
state_zones <- c("North Central", "North East", "North West", "South East", "South South", "South West")

# Function to create and save climatology plots
create_climatology_plots <- function(state_data, state_zone, years, met_var) {
  # Create the climatology plot
  climatology_plot <- plot(
    state_data,
    years = years,
    cumulative = TRUE,
    climatology = TRUE,
    met.var = met_var
  ) +
    theme_minimal() +
    labs(
      title = paste("Climatology Plot -", state_zone, "-", met_var),
      x = "Month",
      y = "Cumulative Value"
    )
  
  return(climatology_plot)
}

# List to store plots
all_plots <- list()

# Loop through state indices
for (i in seq_along(state_indices)) {
  state_index <- state_indices[i]
  state_zone <- state_zones[i]
  
  # Loop through the selected years and create plots for rainfall and temperature
  for (start_year in c(1991, 2001, 2011)) {
    end_year <- start_year + 9
    selected_years <- start_year:end_year
    
    # Plot Rainfall
    rainfall_plot <- create_climatology_plots(
      state_data_list[[state_index]],
      state_zone,
      selected_years,
      "rain"
    )
    
    # Plot max Temperature
    max_temp_plot <- create_climatology_plots(
      state_data_list[[state_index]],
      state_zone,
      selected_years,
      "maxt"
    )
    
    # Plot min Temperature
    min_temp_plot <- create_climatology_plots(
      state_data_list[[state_index]],
      state_zone,
      selected_years,
      "mint"
    )
    
    # Save the plots
    all_plots[[paste(state_zone, "_Rainfall_", start_year, "-", end_year)]] <- rainfall_plot
    all_plots[[paste(state_zone, "_Max_Temperature_", start_year, "-", end_year)]] <- max_temp_plot
    all_plots[[paste(state_zone, "_Min_Temperature_", start_year, "-", end_year)]] <- min_temp_plot
  }
}

# Save all plots
for (plot_name in names(all_plots)) {
  filename <- paste0("figures/climatology/decades/", tolower(gsub(" ", "_", plot_name)), ".jpg")
  ggsave(filename, all_plots[[plot_name]], width = 8, height = 6, units = "in")
}



# List to store plots
all_plots <- list()


# Loop through state indices
for (i in seq_along(state_indices)) {
  state_index <- state_indices[i]
  state_zone <- state_zones[i]
  
  # Plot Rainfall
  rainfall_plot <- create_climatology_plots(
    state_data_list[[state_index]],
    state_zone,
    c(1990:2020),
    "rain"
  )
  
  # Plot max Temperature
  max_temp_plot <- create_climatology_plots(
    state_data_list[[state_index]],
    state_zone,
    c(1990:2020),
    "maxt"
  )
  
  # Plot min Temperature
  min_temp_plot <- create_climatology_plots(
    state_data_list[[state_index]],
    state_zone,
    c(1990:2020),
    "mint"
  )
  
  # Save the plots
  all_plots[[paste(state_zone, "_Rainfall_1990-2020")]] <- rainfall_plot
  all_plots[[paste(state_zone, "_Max_Temperature_1990-2020")]] <- max_temp_plot
  all_plots[[paste(state_zone, "_Min_Temperature_1990-2020")]] <- min_temp_plot
}

# Save all plots
for (plot_name in names(all_plots)) {
  filename <- paste0("figures/climatology/non_decades/", tolower(gsub(" ", "_", plot_name)), ".jpg")
  ggsave(filename, all_plots[[plot_name]], width = 8, height = 6, units = "in")
}

