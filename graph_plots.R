# ## using summary
# p1 <- plot(zone_data_list$NC, climatology = T, cumulative = T, summary = T)
# p1 + ggtitle("North Central Nigeria Cumulative Temperature time series for 1990 - 2020")
# ggsave(filename = "NC cumulative Rain.png", units = "px", width = 2000, height = 1500)




library(ggplot2)

# Create a list to store the plots
zone_plots <- list()

# Loop through each ecological zone and create a plot
for (zone in unique_zones) {
  # Get the data for the current zone
  current_zone_data <- zone_data_list[[zone]]
  
  # Create the plot
  current_plot <- plot(current_zone_data, climatology = TRUE, cumulative = TRUE, summary = TRUE) +
    ggtitle(paste("Cumulative Temperature time series for", zone, "Nigeria (1990 - 2020)")) +
    theme_minimal()
  
  # Save the plot to the list
  zone_plots[[zone]] <- current_plot
  
  # Save the plot as an image
  ggsave(filename = paste0("figures/temperature/",zone, "_cumulative_temperature.jpg"), plot = current_plot, width = 10, height = 6, units = "in")
}




# # Access the plots for each ecological zone
# p_NC <- zone_plots[["NC"]]
# p_NE <- zone_plots[["NE"]]
# p_NW <- zone_plots[["NW"]]
# p_SE <- zone_plots[["SE"]]
# p_SS <- zone_plots[["SS"]]
# p_SW <- zone_plots[["SW"]]


# Create a list to store the plots
zone_plots_rainfall <- list()

# Loop through each ecological zone and create a plot for rainfall
for (zone in unique_zones) {
  # Get the data for the current zone
  current_zone_data <- zone_data_list[[zone]]
  
  # Create the plot for rainfall
  current_plot_rainfall <- plot(current_zone_data, climatology = TRUE, cumulative = TRUE, met.var = "rain", summary = TRUE) +
    ggtitle(paste("Cumulative Rainfall time series for", zone, "Nigeria (1990 - 2020)")) +
    theme_minimal()
  
  # Save the plot to the list
  zone_plots_rainfall[[zone]] <- current_plot_rainfall
  
  # Save the plot as an image
  ggsave(filename = paste0("figures/rainfall/",zone, "_cumulative_rainfall.jpg"), plot = current_plot_rainfall, width = 10, height = 6, units = "in")
}

# # Access the plots for cumulative rainfall for each ecological zone
# p_NC_rainfall <- zone_plots_rainfall[["NC"]]
# p_NE_rainfall <- zone_plots_rainfall[["NE"]]
# p_NW_rainfall <- zone_plots_rainfall[["NW"]]
# p_SE_rainfall <- zone_plots_rainfall[["SE"]]
# p_SS_rainfall <- zone_plots_rainfall[["SS"]]
# p_SW_rainfall <- zone_plots_rainfall[["SW"]]






# library(ggplot2)
# 
# # Assuming your maize data frame is named 'maize_data'
# # Adjust column names as needed
# maize_data$Year <- as.factor(maize_data$Year)
# 
# # Plot time series of Maize Yield (kg/ha)
# ggplot(maize_data, aes(x = Year, y = `Maize Yield (kg/ha)`)) +
#   geom_line() +
#   labs(
#     title = "Time Series of Maize Yield",
#     x = "Year",
#     y = "Maize Yield (kg/ha)"
#   ) +
#   theme_minimal()
# 
# # Save the plot to a file (optional)
# ggsave("maize_yield_time_series.png", width = 10, height = 6, units = "in")



library(ggplot2)

# Assuming your maize data frame is named 'maize_data'
# Convert 'Year' to numeric
maize_data$Year <- as.numeric(as.character(maize_data$Year))

# Plot time series of Maize Yield (kg/ha)
ggplot(maize_data, aes(x = Year, y = `Maize Yield (kg/ha)`)) +
  geom_line(color = "green") +
  geom_point(color = "darkgreen", fill = "darkgreen") +  # Set point color and fill
  labs(
    title = "Maize Yield Time Series for Nigeria (1990-2020)",
    x = "Year",
    y = "Maize Yield (kg/ha)"
  ) +
  theme_minimal()

# Save the plot to a file (optional)
ggsave(filename = paste0("figures/maize/","maize_yield_time_series.jpg"), width = 10, height = 6, units = "in")





library(ggplot2)

# Assuming your data frame is named 'aggregated_nigeria'
# Convert 'year' to factor for better grouping in the plot
# as.numeric(as.character
aggregated_nigeria$year <- as.numeric(as.character(aggregated_nigeria$year))

# # Plot time series of Max and Min Temperature and Rainfall
# ggplot(aggregated_nigeria, aes(x = year)) +
#   geom_line(aes(y = maxt, color = "Max Temperature"), size = 1.5) +
#   geom_line(aes(y = mint, color = "Min Temperature"), size = 1.5) +
#   geom_bar(aes(y = rain * 10, fill = "Rainfall"), stat = "identity", alpha = 0.7) +  # Scale for better visibility
#   labs(
#     title = "Temperature and Rainfall Time Series for Nigeria (1990-2020)",
#     x = "Year",
#     y = "Temperature (°C) / Rainfall (mm)"
#   ) +
#   scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue")) +  # Adjust color
#   scale_fill_manual(values = "green") +  # Adjust fill color
#   theme_minimal()
# 
# # Save the plot to a file (optional)
# ggsave(filename = paste0("figures/maize/","maize_yield_time_series.jpg"), width = 10, height = 6, units = "in")
# 



library(ggplot2)

# Assuming your data frame is named 'aggregated_nigeria'
# Convert 'year' to factor for better grouping in the plot
#aggregated_nigeria$year <- as.factor(aggregated_nigeria$year)

# # Plot time series of Max and Min Temperature
# ggplot(aggregated_nigeria, aes(x = year)) +
#   geom_line(aes(y = maxt, color = "Max Temperature"), size = 1.5) +
#   geom_line(aes(y = mint, color = "Min Temperature"), size = 1.5) +
#   labs(
#     title = "Temperature Time Series for Nigeria (1990-2020)",
#     x = "Year",
#     y = "Temperature (°C)"
#   ) +
#   scale_color_manual(values = c("Max Temperature" = "red", "Min Temperature" = "blue")) +  # Adjust color
#   theme_minimal()

# Plot time series of Rainfall
ggplot(aggregated_nigeria, aes(x = year)) +
  geom_bar(aes(y = rain * 10, fill = "Rainfall"), stat = "identity", alpha = 0.7) +  # Scale for better visibility
  labs(
    title = "Rainfall Time Series for Nigeria (1990-2020)",
    x = "Year",
    y = "Rainfall (mm)",
    fill = ""
  ) +
  scale_fill_manual(values = "green") +  # Adjust fill color
  theme_minimal()
ggsave(filename = paste0("figures/","rainfall_barplot_time_series.jpg"), width = 10, height = 6, units = "in")




# Reshape the data for a stacked bar plot
temperature_stacked <- aggregated_nigeria %>%
  pivot_longer(cols = c(maxt, mint), names_to = "Temperatures", values_to = "Temperature")

# Plot stacked bar for Temperature
ggplot(temperature_stacked, aes(x = year, y = Temperature, fill = Temperatures)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, alpha = 0.9) +
  labs(
    title = "Temperature Time Series for Nigeria (1990-2020)",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  scale_fill_manual(values = c("maxt" = "#FFA500", "mint" = "#3F7CAC")) +  # Adjust color
  theme_minimal()

ggsave(filename = paste0("figures/","temperature_barplot_time_series.jpg"), width = 10, height = 6, units = "in")







# Plot time series of Temperature
ggplot(temperature_stacked, aes(x = year, y = Temperature, color = Temperatures)) +
  geom_line(size=0.5) +
  geom_point() +
  geom_hline(yintercept = mean(aggregated_nigeria$maxt), linetype = "dashed") +
  geom_hline(yintercept = mean(aggregated_nigeria$mint), linetype = "dashed") +
  labs(
    title = "Temperature Time Series for Nigeria (1990-2020)",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  scale_color_manual(values = c("maxt" = "#FFA500", "mint" = "#3F7CAC")) +  # Adjust color
  theme_minimal()

ggsave(filename = paste0("figures/","temperature_time_series.jpg"), width = 10, height = 6, units = "in")


# Plot time series of Rainfall
ggplot(aggregated_nigeria, aes(x = year, y = rain * 10, color = "Rainfall")) +
  geom_line(size = 0.5) +
  geom_point(color = "darkgreen", fill = "darkgreen") +
  geom_hline(yintercept = mean(aggregated_nigeria$rain * 10), linetype = "dashed") +
  labs(
    title = "Rainfall Time Series for Nigeria (1990-2020)",
    x = "Year",
    y = "Rainfall (mm)",
    color = ""
  ) +
  scale_color_manual(values = "green") +  # Adjust color
  theme_minimal()
ggsave(filename = paste0("figures/","rainfall_time_series.jpg"), width = 10, height = 6, units = "in")





### regression and correlation

# Load required libraries
library(corrplot)

# Loop through each zone and variable
for (zone in zones) {
  for (variable in variables) {
    # Extract data for the current zone and variable
    zone_data <- regression_models[[zone]]$model
    x_data <- zone_data[[variable]]
    
    # Extract slope (m) and intercept (c) from the regression model
    m <- summary(regression_models[[zone]])$coefficients[variable, "Estimate"]
    c <- summary(regression_models[[zone]])$coefficients["(Intercept)", "Estimate"]
    
    # Create a scatter plot with a trend line and R-squared value
    p <- ggplot(zone_data, aes(x = .data[[variable]], y = aggregated_yield)) +
      geom_point(color = "steelblue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x) +
      labs(title = paste("Scatter Plot -", zone, "-", variable),
           x = variable,
           y = "Maize Yield") +
      theme_minimal() +
      geom_text(
        x = max(x_data),
        y = max(zone_data$aggregated_yield),
        label = paste("R-squared =", round(summary(regression_models[[zone]])$r.squared, 2),
                      "\ny =", round(m, 0), "x +", round(c, 0)),
        hjust = 1,
        vjust = 1,
        color = "darkred"
      )
    
    # Save the plot (optional)
    ggsave(paste("figures/ScatterPlot/","scatter_plot_", zone, "_", variable, ".jpg"), plot = p, width = 8, height = 6, units = "in")
    
    # Display the plot
    #print(p)
  }
}


# Extract data for the Nigeria regression model
nigeria_data <- regression_model_nigeria$model

# Extract slope (m) and intercept (c) from the regression model
m_nigeria <- summary(regression_model_nigeria)$coefficients[variable, "Estimate"]
c_nigeria <- summary(regression_model_nigeria)$coefficients["(Intercept)", "Estimate"]

# Create a scatter plot with a trend line and R-squared value for Nigeria
p_nigeria <- ggplot(nigeria_data, aes(x = .data[[variable]], y = aggregated_yield_nigeria)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x) +
  labs(title = paste("Scatter Plot - Nigeria -", variable),
       x = variable,
       y = "Maize Yield") +
  theme_minimal() +
  geom_text(
    x = max(nigeria_data[[variable]]),
    y = max(nigeria_data$aggregated_yield_nigeria),
    label = paste("R-squared =", round(summary(regression_model_nigeria)$r.squared, 2),
                  "\ny =", round(m_nigeria, 0), "x +", round(c_nigeria, 0)),
    hjust = 1,
    vjust = 1,
    color = "darkred"
  )

# Save the plot for Nigeria (optional)
ggsave(paste("figures/ScatterPlot/","scatter_plot_nigeria.jpg"), plot = p_nigeria, width = 8, height = 6, units = "in")

# Display the plot for Nigeria
#print(p_nigeria)



library(ggplot2)

# Assuming you have a data frame named 'df' with columns 'Year', 'Maize_Yield', 'Max_Temperature', 'Min_Temperature', 'Rainfall'

# Create a ggplot object
ggplot(aggregated_nigeria, aes(x = year)) +
  geom_line(aes(y = aggregated_yield_nigeria, color = "Maize Yield"), size = 1) +
  geom_line(aes(y = maxt, color = "Max Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = mint, color = "Min Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = rain, color = "Rainfall"), size = 1, linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Meteorological Variables")) +
  labs(title = "Maize Yield and Meteorological Variables Over Time",
       x = "Year",
       y = "Maize Yield",
       color = "Variables") +
  theme_minimal() +
  theme(legend.position = "top")

# Display the plot
# print(p)


library(ggplot2)

# Assuming you have a data frame named 'df' with columns 'Year', 'Maize_Yield', 'Max_Temperature', 'Min_Temperature', 'Rainfall'

# Create a ggplot object
ggplot(aggregated_nigeria, aes(x = year)) +
  geom_line(aes(y = aggregated_yield_nigeria, color = "Maize Yield"), size = 1) +
  geom_line(aes(y = maxt, color = "Max Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = mint, color = "Min Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = rain, color = "Rainfall"), size = 1, linetype = "dashed") +
  facet_grid(variable ~ ., scales = "free_y", switch = "y") +
  labs(title = "Maize Yield and Meteorological Variables Over Time",
       x = "Year",
       y = "Value",
       color = "Variables") +
  theme_minimal() +
  theme(legend.position = "top")

# Display the plot
# print(p)


library(ggplot2)

# Assuming 'aggregated_nigeria' is your dataframe
# Create a new variable 'variable' to indicate the type of variable
aggregated_nigeria$variable <- rep(c("Maize Yield", "Max Temperature", "Min Temperature", "Rainfall"), each = nrow(aggregated_nigeria))

# Plotting
ggplot(aggregated_nigeria, aes(x = year)) +
  geom_line(aes(y = aggregated_yield_nigeria, color = "Maize Yield"), size = 1) +
  geom_line(aes(y = maxt, color = "Max Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = mint, color = "Min Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = rain, color = "Rainfall"), size = 1, linetype = "dashed") +
  facet_grid(variable ~ ., scales = "free_y", switch = "y") +
  labs(title = "Maize Yield and Meteorological Variables Over Time",
       x = "Year",
       y = "Value",
       color = "Variables") +
  theme_minimal() +
  theme(legend.position = "top")



library(ggplot2)

# Assuming 'aggregated_nigeria' is your dataframe
# Create a new variable 'variable' to indicate the type of variable
aggregated_nigeria$variable <- rep(c("Maize Yield", "Max Temperature", "Min Temperature", "Rainfall"), times = nrow(aggregated_nigeria) / 4)

# Plotting
ggplot(aggregated_nigeria, aes(x = year)) +
  geom_line(aes(y = aggregated_yield_nigeria, color = "Maize Yield"), size = 1) +
  geom_line(aes(y = maxt, color = "Max Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = mint, color = "Min Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = rain, color = "Rainfall"), size = 1, linetype = "dashed") +
  facet_grid(variable ~ ., scales = "free_y", switch = "y") +
  labs(title = "Maize Yield and Meteorological Variables Over Time",
       x = "Year",
       y = "Value",
       color = "Variables") +
  theme_minimal() +
  theme(legend.position = "top")




library(ggplot2)

# Assuming 'aggregated_nigeria' is your dataframe
# Create a new variable 'variable' to indicate the type of variable
aggregated_nigeria$variable <- rep(c("Maize Yield", "Max Temperature", "Min Temperature", "Rainfall"), length.out = nrow(aggregated_nigeria))

# Plotting
ggplot(aggregated_nigeria, aes(x = year)) +
  geom_line(aes(y = aggregated_yield_nigeria, color = "Maize Yield"), size = 1) +
  geom_line(aes(y = maxt, color = "Max Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = mint, color = "Min Temperature"), size = 1, linetype = "dashed") +
  geom_line(aes(y = rain, color = "Rainfall"), size = 1, linetype = "dashed") +
  facet_grid(variable ~ ., scales = "free_y", switch = "y") +
  labs(title = "Maize Yield and Meteorological Variables Over Time",
       x = "Year",
       y = "Value",
       color = "Variables") +
  theme_minimal() +
  theme(legend.position = "top")




library(ggplot2)

# Assuming 'aggregated_nigeria' is your dataframe
# Create a new variable 'variable' to indicate the type of variable
aggregated_nigeria$variable <- rep(c("Maize Yield", "Max Temperature", "Min Temperature", "Rainfall"), length.out = nrow(aggregated_nigeria))

# Plotting
ggplot(aggregated_nigeria, aes(x = year)) +
  geom_line(aes(y = aggregated_yield_nigeria, color = "Maize Yield"), size = 1) +
  geom_line(aes(y = maxt, color = "Max Temperature"), size = 1, linetype = "dashed") +
  facet_grid(variable ~ ., scales = "free_y", switch = "y") +
  labs(title = "Maize Yield and Meteorological Variables Over Time",
       x = "Year",
       y = "Value",
       color = "Variables") +
  theme_minimal() +
  theme(legend.position = "top")



library(ggplot2)
library(patchwork)

# Assuming 'aggregated_nigeria' is your dataframe
# Create a new variable 'variable' to indicate the type of variable
aggregated_nigeria$variable <- rep(c("Maize Yield", "Max Temperature", "Min Temperature", "Rainfall"), length.out = nrow(aggregated_nigeria))

# Plotting Maize Yield
plot_maize_yield <- ggplot(aggregated_nigeria, aes(x = year, y = aggregated_yield_nigeria)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Maize Yield Over Time",
       x = "Year",
       y = "Maize Yield (kg/ha)") +
  theme_minimal()

# Plotting Max Temperature
plot_max_temp <- ggplot(aggregated_nigeria, aes(x = year, y = maxt)) +
  geom_line(color = "darkred", size = 1, linetype = "dashed") +
  labs(title = "Max Temperature Over Time",
       x = "Year",
       y = "Max Temperature") +
  theme_minimal()

# Plotting Min Temperature
plot_min_temp <- ggplot(aggregated_nigeria, aes(x = year, y = mint)) +
  geom_line(color = "green", size = 1, linetype = "dashed") +
  labs(title = "Min Temperature Over Time",
       x = "Year",
       y = "Min Temperature") +
  theme_minimal()

# Plotting Rainfall
plot_rainfall <- ggplot(aggregated_nigeria, aes(x = year, y = rain)) +
  geom_line(color = "orange", size = 1, linetype = "dashed") +
  labs(title = "Rainfall Over Time",
       x = "Year",
       y = "Rainfall") +
  theme_minimal()

# Combine the plots using patchwork
combined_plots_maxt <- plot_maize_yield / plot_max_temp
combined_plots_mint <- plot_maize_yield  / plot_min_temp
combined_plot_rain <- plot_maize_yield  /  plot_rainfall

# Display the combined plots
# Save each plot
ggsave("figures/CombinedPlots/maize_yield_max_temperature.png", plot = combined_plots_maxt, width = 8, height = 6, units = "in")
##ggsave("CombinedPlots/max_temperature.png", plot = plot_max_temp, width = 8, height = 6, units = "in")
ggsave("figures/CombinedPlots/maize_yield_min_temperature.png", plot = combined_plots_mint, width = 8, height = 6, units = "in")
ggsave("figures/CombinedPlots/maize_yield_rainfall.png", plot = combined_plot_rain, width = 8, height = 6, units = "in")










# Plotting Maize Yield with left y-axis
plot_maize_yield <- ggplot(aggregated_nigeria, aes(x = year, y = aggregated_yield_nigeria)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "Maize Yield and Max Temperature Over Time",
       x = "Year",
       y = "Maize Yield (kg/ha)") +
  theme_minimal()

# Plotting Max Temperature with right y-axis
plot_max_temp <- ggplot(aggregated_nigeria, aes(x = year, y = maxt)) +
  geom_line(color = "darkred", size = 1, linetype = "dashed") +
  labs(title = "",
       x = "",
       y = "Max Temperature") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "darkred"))

# Combine the plots using patchwork
combined_plots <- plot_maize_yield + plot_max_temp + plot_layout(guides = 'collect')

# Save the combined plot
ggsave("CombinedPlots/combined_maize_temp.png", plot = combined_plots, width = 8, height = 6, units = "in")

# Display the combined plot
combined_plots






# Plotting Maize Yield with left y-axis
plot_maize_temp <- ggplot(aggregated_nigeria, aes(x = year)) +
  geom_line(aes(y = aggregated_yield_nigeria, color = "Maize Yield"), size = 1) +
  labs(title = "Maize Yield and Max Temperature Over Time",
       x = "Year",
       y = "Maize Yield (kg/ha)",
       color = "Variable") +
  theme_minimal()

# Adding Max Temperature with right y-axis
plot_maize_temp <- plot_maize_temp +
  geom_line(aes(y = maxt, color = "Max Temperature"), size = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Max Temperature",
    sec.axis = sec_axis(~., name = "Maize Yield")
  ) +
  theme_minimal()

# Save the plot (optional)
ggsave("CombinedPlots/combined_maize_temp_overlay.png", plot = plot_maize_temp, width = 8, height = 6, units = "in")

# Display the plot
print(plot_maize_temp)




# # Set up some fake test data (replace this with your actual data)
# aggregated_nigeria <- data.frame(
#   year = seq(1990, 2020, 1),
#   aggregated_yield_nigeria = runif(31, 10000, 30000),
#   maxt = runif(31, 30, 34),
#   mint = runif(31, 20, 23),
#   rain = runif(31, 2, 5)
# )



# Save the plot
filename <- "figures/secondaries/maize_yield_max_temperature_plot.png"
png(
  filename,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the first plot
{
  plot(aggregated_nigeria$year, aggregated_nigeria$aggregated_yield_nigeria, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "black", 
       main = "Maize Yield and Max Temperature Over Nigeria")
  
  # Draw the left y-axis
  ##axis(2, pretty(range(aggregated_nigeria$aggregated_yield_nigeria)), col = "black", 
  ##     las = 1, padj = 0)
  mtext("Maize Yield", side = 2, line = 2.5)
  
  # Add a box around the first plot
  box()
  
  # Create the second plot on the same graph
  par(new = TRUE)
  
  # Plot the second set of data
  plot(aggregated_nigeria$year, aggregated_nigeria$maxt, pch = 15, xlab = "", ylab = "", type = "b", col = "darkred", axes=FALSE)
  
  # Draw the right y-axis
  axis(4, pretty(range(aggregated_nigeria$maxt)), col = "darkred", col.axis = "darkred", las = 1)
  
  # Add labels for the right y-axis
  mtext("Max Temperature", side = 4, col = "darkred", line = 2.5)
  
  # Draw the time axis
  axis(1, pretty(range(aggregated_nigeria$year), 10))
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add Legend
  legend("topleft", legend = c("Maize Yield", "Max Temperature"),
         text.col = c("black", "darkred"), pch = c(16, 15), col = c("black", "darkred"))
}

dev.off()


# Save the plot
filename_mint <- "figures/secondaries/maize_yield_mint_plot.png"
png(
  filename_mint,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the first plot
{
  plot(aggregated_nigeria$year, aggregated_nigeria$aggregated_yield_nigeria, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "black", 
       main = "Maize Yield and Min Temperature Over Nigeria")
  
  # Draw the left y-axis
  mtext("Maize Yield", side = 2, line = 2.5)
  
  # Add a box around the first plot
  box()
  
  # Create the second plot on the same graph
  par(new = TRUE)
  
  # Plot the second set of data
  plot(aggregated_nigeria$year, aggregated_nigeria$mint, pch = 15, xlab = "", ylab = "", type = "b", col = "darkorange", axes=FALSE)
  
  # Draw the right y-axis
  axis(4, pretty(range(aggregated_nigeria$mint)), col = "darkorange", col.axis = "darkorange", las = 1)
  
  # Add labels for the right y-axis
  mtext("Min Temperature", side = 4, col = "darkorange", line = 2.5)
  
  # Draw the time axis
  axis(1, pretty(range(aggregated_nigeria$year), 10))
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add Legend
  legend("topleft", legend = c("Maize Yield", "Min Temperature"),
         text.col = c("black", "darkorange"), pch = c(16, 15), col = c("black", "darkorange"))
}

dev.off()


# Save the plot
filename_rain <- "figures/secondaries/maize_yield_rain_plot.png"
png(
  filename_rain,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the first plot
{
  plot(aggregated_nigeria$year, aggregated_nigeria$aggregated_yield_nigeria, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "black", 
       main = "Maize Yield and Rainfall Over Nigeria")
  
  # Draw the left y-axis
  mtext("Maize Yield", side = 2, line = 2.5)
  
  # Add a box around the first plot
  box()
  
  # Create the second plot on the same graph
  par(new = TRUE)
  
  # Plot the second set of data
  plot(aggregated_nigeria$year, aggregated_nigeria$rain, pch = 15, xlab = "", ylab = "", type = "b", col = "darkgreen", axes=FALSE)
  
  # Draw the right y-axis
  axis(4, pretty(range(aggregated_nigeria$rain)), col = "darkgreen", col.axis = "darkgreen", las = 1)
  
  # Add labels for the right y-axis
  mtext("Rainfall", side = 4, col = "darkgreen", line = 2.5)
  
  # Draw the time axis
  axis(1, pretty(range(aggregated_nigeria$year), 10))
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add Legend
  legend("topleft", legend = c("Maize Yield", "Rainfall"),
         text.col = c("black", "darkgreen"), pch = c(16, 15), col = c("black", "darkgreen"))
}

dev.off()





# 
# # Set up the figure folder
# #dir.create("figures/secondaries", showWarnings = FALSE)
# 
# 
# # Create the first plot
# {
#   plot(aggregated_nigeria$year, aggregated_nigeria$aggregated_yield_nigeria, pch = 16, 
#      xlab = "", ylab = "", type = "b", col = "blue", 
#      main = "Maize Yield and Max Temperature Over Nigeria")
# 
#   mtext("Maize Yield", side = 2, line = 2.5)
#   
#   # Add a box around the first plot
#   box()
#   
#   # Create the second plot on the same graph
#   par(new = TRUE)
#   
#   # Plot the second set of data
#   plot(aggregated_nigeria$year, aggregated_nigeria$maxt, pch = 15, xlab = "", ylab = "", type = "b", col = "red", axes = FALSE)
#   
#   # Draw the right y-axis
#   axis(4, pretty(range(aggregated_nigeria$maxt)), col = "red", col.axis = "red", las = 1)
#   
#   # Add labels for the right y-axis
#   mtext("Max Temperature", side = 4, col = "red", line = 2.5)
#   
#   # Draw the time axis
#   axis(1, pretty(range(aggregated_nigeria$year), 10))
#   mtext("Year", side = 1, col = "black", line = 2.5)
#   
#   # Add Legend
#   legend("topleft", legend = c("Maize Yield", "Max Temperature"),
#          text.col = c("blue", "red"), pch = c(16, 15), col = c("blue", "red"))
# }
# 
# # Save the plot
# filename <- "figures/secondaries/maize_yield_max_temperature_plot.jpg"
# ##ggsave(filename, width = 8, height = 6, units = "in")
# 
# # png(filename, width = 8, height = 6, units = "in", res = 300)
# # dev.copy(png)
# # dev.off()
# 
# dev.copy(png, filename)
# dev.off()
# 
# 
# 
# # 
# # # Set up your base R plot
# # plot(aggregated_nigeria$year, aggregated_nigeria$aggregated_yield_nigeria, pch = 16, 
# #      xlab = "", ylab = "", type = "b", col = "black", 
# #      main = "Maize Yield and Max Temperature Over Nigeria")
# # 
# # # Add a box around the first plot
# # box()
# # 
# # # Create the second plot on the same graph
# # par(new = TRUE)
# # 
# # # Plot the second set of data
# # plot(aggregated_nigeria$year, aggregated_nigeria$maxt, pch = 15, xlab = "", ylab = "", type = "b", col = "red", axes=FALSE)
# # 
# # # Draw the right y-axis
# # axis(4, pretty(range(aggregated_nigeria$maxt)), col = "red", col.axis = "red", las = 1)
# # 
# # # Add labels for the right y-axis
# # mtext("Max Temperature", side = 4, col = "red", line = 2.5)
# # 
# # Draw the time axis
# axis(1, pretty(range(aggregated_nigeria$year), 10))
# mtext("Year", side = 1, col = "black", line = 2.5)
# 
# # Add Legend
# legend("topleft", legend = c("Maize Yield", "Max Temperature"),
#        text.col = c("black", "red"), pch = c(16, 15), col = c("black", "red"))
# 
# 
# filename <- "figures/secondaries/maize_yield_max_temperature_plot.png"
# dev.copy(png, filename)
# dev.off()
# 
# 
# 
# # 
# # library(ggplot2)
# # library(gridExtra)
# # 
# # # Create the first plot
# # plot1 <- ggplot(aggregated_nigeria, aes(x = year, y = aggregated_yield_nigeria)) +
#   geom_line(color = "black", size = 1) +
#   labs(title = "Maize Yield Over Time",
#        x = "Year",
#        y = "Maize Yield (kg/ha)") +
#   theme_minimal()
# 
# # Create the second plot
# plot2 <- ggplot(aggregated_nigeria, aes(x = year, y = maxt)) +
#   geom_line(color = "red", size = 1, linetype = "dashed") +
#   labs(title = "Max Temperature Over Time",
#        x = "Year",
#        y = "Max Temperature") +
#   theme_minimal()
# 
# # Arrange the plots side by side
# combined_plot <- grid.arrange(plot1, plot2, ncol = 2)
# 
# # Save the combined plot
# filename <- "figures/secondaries/maize_yield_max_temperature_plot.png"
# ggsave(filename, combined_plot, width = 8, height = 6, units = "in", dpi = 300)


# 
# # Extract data for the Nigeria regression model
# nigeria_data <- regression_models[["Nigeria"]]$model
# 
# # Extract slope (m) and intercept (c) from the regression model
# m_nigeria <- summary(regression_models)$coefficients[variable, "Estimate"]
# c_nigeria <- summary(regression_models[["Nigeria"]])$coefficients["(Intercept)", "Estimate"]
# 
# # Create a scatter plot with a trend line and R-squared value for Nigeria
# p_nigeria <- ggplot(nigeria_data, aes(x = .data[[variable]], y = aggregated_yield_nigeria)) +
#   geom_point(color = "steelblue", size = 3) +
#   geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x) +
#   labs(title = paste("Scatter Plot with Trend Line - Nigeria -", variable),
#        x = variable,
#        y = "Aggregated Yield") +
#   theme_minimal() +
#   geom_text(
#     x = max(nigeria_data[[variable]]),
#     y = max(nigeria_data$aggregated_yield_nigeria),
#     label = paste("R-squared =", round(summary(regression_models[["Nigeria"]])$r.squared, 4),
#                   "\nEquation: y =", round(m_nigeria, 4), "x +", round(c_nigeria, 4)),
#     hjust = 1,
#     vjust = 1,
#     color = "darkred"
#   )
# 
# # Save the plot for Nigeria (optional)
# ggsave("figures/ScatterPlot/","scatter_plot_nigeria.jpg", plot = p_nigeria, width = 8, height = 6, units = "in")
# 
# # Display the plot for Nigeria
# #print(p_nigeria)




# # Function to create a bar plot for regression coefficients
# plot_regression <- function(results, zone) {
#   ggplot(results[[zone]], aes(x = rownames(results[[zone]]), y = Estimate)) +
#     geom_bar(stat = "identity", fill = "steelblue") +
#     labs(title = paste("Regression Coefficients -", zone),
#          x = "Variables",
#          y = "Coefficient Estimate") +
#     theme_minimal()
# }
# 
# # Plot regression coefficients for each zone
# zones <- c("NC", "NE", "NW", "SE", "SS", "SW")
# regression_plots <- lapply(zones, function(zone) plot_regression(regression_results, zone))
# 
# # Plot correlation matrices
# correlation_plots <- lapply(zones, function(zone) {
#   corr_matrix <- correlation_results[[zone]]
#   corrplot(corr_matrix, method = "color", title = paste("Correlation Matrix -", zone))
# })
# 
# # Arrange and display the plots
# library(gridExtra)
# grid.arrange(grobs = c(regression_plots, correlation_plots), ncol = 2)





# 
# # Load required libraries
# library(ggplot2)
# library(corrplot)
# 
# # Function to extract regression coefficients from lm object
# get_regression_data <- function(model) {
#   coefficients <- summary(model)$coefficients[, 1:2]
#   colnames(coefficients) <- c("Estimate", "Std. Error")
#   return(data.frame(Variables = rownames(coefficients), coefficients))
# }
# 
# # Function to create a bar plot for regression coefficients
# plot_regression <- function(results, zone) {
#   regression_data <- get_regression_data(results[[zone]])
#   
#   ggplot(regression_data, aes(x = Variables, y = Estimate)) +
#     geom_bar(stat = "identity", fill = "steelblue") +
#     labs(title = paste("Regression Coefficients -", zone),
#          x = "Variables",
#          y = "Coefficient Estimate") +
#     theme_minimal()
# }
# 
# # Plot regression coefficients for each zone
# zones <- c("NC", "NE", "NW", "SE", "SS", "SW")
# regression_plots <- lapply(zones, function(zone) plot_regression(regression_results, zone))
# 
# # Plot correlation matrices
# correlation_plots <- lapply(zones, function(zone) {
#   corr_matrix <- correlation_results[[zone]]
#   corrplot(corr_matrix, method = "color", title = paste("Correlation Matrix -", zone))
# })
# 
# # Arrange and display the plots
# library(gridExtra)
# grid.arrange(grobs = c(regression_plots, correlation_plots), ncol = 2)
# 
# 

# 
# # Function to extract regression coefficients from lm object
# get_regression_data <- function(model) {
#   coefficients <- coef(model)
#   return(data.frame(
#     Variables = names(coefficients),
#     Estimate = coefficients,
#     Std.Error = summary(model)$coefficients[, "Std. Error"]
#   ))
# }
# 
# # Function to create a bar plot for regression coefficients
# plot_regression <- function(results, zone) {
#   regression_data <- get_regression_data(results[[zone]])
#   
#   ggplot(regression_data, aes(x = Variables, y = Estimate)) +
#     geom_bar(stat = "identity", fill = "steelblue") +
#     geom_errorbar(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.2, position = "identity", color = "black") +
#     labs(title = paste("Regression Coefficients -", zone),
#          x = "Variables",
#          y = "Coefficient Estimate") +
#     theme_minimal()
# }
# 
# # Plot regression coefficients for each zone
# zones <- c("NC", "NE", "NW", "SE", "SS", "SW")
# regression_plots <- lapply(zones, function(zone) plot_regression(regression_results, zone))
# 
# # Plot correlation matrices
# correlation_plots <- lapply(zones, function(zone) {
#   corr_matrix <- correlation_results[[zone]]
#   corrplot(corr_matrix, method = "color", title = paste("Correlation Matrix -", zone))
# })
# 
# # Arrange and display the plots
# library(gridExtra)
# grid.arrange(grobs = c(regression_plots, correlation_plots), ncol = 2)

# # Function to extract regression coefficients from lm object
# get_regression_data <- function(model) {
#   coefficients <- coef(model)
#   standard_errors <- sqrt(diag(vcov(model)))
#   
#   return(data.frame(
#     Variables = names(coefficients),
#     Estimate = coefficients,
#     Std.Error = standard_errors
#   ))
# }

# # Function to extract regression coefficients from lm object
# get_regression_data <- function(model) {
#   coefficients <- coef(model)
#   standard_errors <- sqrt(diag(vcov(model)))
#   
#   # Check if the lengths match
#   if (length(coefficients) != length(standard_errors)) {
#     stop("Lengths of coefficients and standard errors do not match.")
#   }
#   
#   return(data.frame(
#     Variables = names(coefficients),
#     Estimate = coefficients,
#     Std.Error = standard_errors
#   ))
# }
# 
# 
# 
# # # Function to extract regression coefficients from lm object
# # get_regression_data <- function(model) {
# #   coefficients <- coef(model)
# #   standard_errors <- summary(model)$coefficients[, "Std. Error"]
# #   
# #   return(data.frame(
# #     Variables = names(coefficients),
# #     Estimate = coefficients,
# #     Std.Error = standard_errors
# #   ))
# # }
# 
# # Function to create a bar plot for regression coefficients
# plot_regression <- function(results, zone) {
#   regression_data <- get_regression_data(results[[zone]])
#   
#   ggplot(regression_data, aes(x = Variables, y = Estimate)) +
#     geom_bar(stat = "identity", fill = "steelblue") +
#     geom_errorbar(aes(ymin = Estimate - Std.Error, ymax = Estimate + Std.Error), width = 0.2, position = "identity", color = "black") +
#     labs(title = paste("Regression Coefficients -", zone),
#          x = "Variables",
#          y = "Coefficient Estimate") +
#     theme_minimal()
# }
# 
# # Plot regression coefficients for each zone
# zones <- c("NC", "NE", "NW", "SE", "SS", "SW")
# regression_plots <- lapply(zones, function(zone) plot_regression(regression_results, zone))
# 
# # Plot correlation matrices
# correlation_plots <- lapply(zones, function(zone) {
#   corr_matrix <- correlation_results[[zone]]
#   corrplot(corr_matrix, method = "color", title = paste("Correlation Matrix -", zone))
# })
# 
# # Arrange and display the plots
# library(gridExtra)
# grid.arrange(grobs = c(regression_plots, correlation_plots), ncol = 2)



# # Function to create a bar plot for regression coefficients
# plot_regression <- function(models, zone) {
#   model <- models[[zone]]
#   coefficients <- coef(model)
#   standard_errors <- sqrt(diag(vcov(model)))
#   
#   # Check if the lengths match
#   if (length(coefficients) != length(standard_errors)) {
#     stop("Lengths of coefficients and standard errors do not match.")
#   }
#   
#   data <- data.frame(
#     Variables = names(coefficients),
#     Estimate = coefficients,
#     Std.Error = standard_errors
#   )
#   
#   ggplot(data, aes(x = Variables, y = Estimate)) +
#     geom_bar(stat = "identity", fill = "steelblue") +
#     labs(title = paste("Regression Coefficients -", zone),
#          x = "Variables",
#          y = "Coefficient Estimate") +
#     theme_minimal()
# }
# 
# # Plot regression coefficients for each zone
# zones <- c("NC", "NE", "NW", "SE", "SS", "SW")
# #regression_plots <- 
# lapply(zones, function(zone) plot_regression(regression_models, zone))
# 
# 
# 
# 
# # Function to create a scatter plot with trend line for regression coefficients
# plot_regression <- function(models, zone) {
#   model <- models[[zone]]
#   
#   # Extract coefficients and their names
#   coefficients <- coef(model)
#   coefficient_names <- names(coefficients)
#   
#   # Prepare data for plotting
#   plot_data <- data.frame(
#     Variables = coefficient_names,
#     Estimate = coefficients
#   )
#   
#   # Create scatter plot
#   p <- ggplot(plot_data, aes(x = Variables, y = Estimate)) +
#     geom_point(color = "steelblue", size = 3) +
#     
#     # Add trend line
#     geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x) +
#     
#     labs(title = paste("Regression Coefficients -", zone),
#          x = "Variables",
#          y = "Coefficient Estimate") +
#     
#     theme_minimal()
#   
#   # Add R-squared value to the plot
#   p <- p + geom_text(
#     x = max(plot_data$Variables),
#     y = max(plot_data$Estimate),
#     label = paste("R-squared =", round(summary(model)$r.squared, 4)),
#     hjust = 1,
#     vjust = 1,
#     color = "darkred"
#   )
#   
#   return(p)
# }
# 
# # Plot regression coefficients for each zone
# zones <- c("NC", "NE", "NW", "SE", "SS", "SW")
# regression_plots <- lapply(zones, function(zone) plot_regression(regression_models, zone))
# 
# 
# 
# 
# 
# # List of zones
# zones <- c("NC", "NE", "NW", "SE", "SS", "SW")
# 
# # List of variables
# variables <- c("maxt", "mint", "rain")
# 
# # Loop through each zone and variable
# for (zone in zones) {
#   for (variable in variables) {
#     # Extract data for the current zone and variable
#     zone_data <- regression_models[[zone]]$model
#     x_data <- zone_data[[variable]]
#     
#     # Create a scatter plot with a trend line and R-squared value
#     p <- ggplot(zone_data, aes(x = .data[[variable]], y = aggregated_yield)) +
#       geom_point(color = "steelblue", size = 3) +
#       geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x_data) +
#       labs(title = paste("Scatter Plot with Trend Line -", zone, "-", variable),
#            x = variable,
#            y = "Aggregated Yield") +
#       theme_minimal() +
#       geom_text(
#         x = max(x_data),
#         y = max(zone_data$aggregated_yield),
#         label = paste("R-squared =", round(summary(regression_models[[zone]])$r.squared, 4)),
#         hjust = 1,
#         vjust = 1,
#         color = "darkred"
#       )
#     
#     # Save the plot (optional)
#     ggsave(paste("scatter_plot_", zone, "_", variable, ".png"), plot = p, width = 8, height = 6, units = "in")
#     
#     # Display the plot
#     print(p)
#   }
# }
# 
# 
# # 
# # # Loop through each zone and variable
# # for (zone in zones) {
# #   for (variable in variables) {
# #     # Extract data for the current zone and variable
# #     zone_data <- regression_models[[zone]]$model
# #     x_data <- zone_data[[variable]]
# #     
# #     # Create a scatter plot with a trend line and R-squared value
# #     p <- ggplot(zone_data, aes(x = .data[[variable]], y = aggregated_yield)) +
# #       geom_point(color = "steelblue", size = 3) +
# #       geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x_data) +
# #       labs(title = paste("Scatter Plot with Trend Line -", zone, "-", variable),
# #            x = variable,
# #            y = "Aggregated Yield") +
# #       abline(regression_models[[zone]]) +
# #       theme_minimal() +
# #       geom_text(
# #         x = max(x_data),
# #         y = max(zone_data$aggregated_yield),
# #         label = paste("R-squared =", round(summary(regression_models[[zone]])$r.squared, 4)),
# #         hjust = 1,
# #         vjust = 1,
# #         color = "darkred"
# #       )
# #     
# #     # Save the plot (optional)
# #     ggsave(paste("scatter_plot_", zone, "_", variable, ".png"), plot = p, width = 8, height = 6, units = "in")
# #     
# #     # Display the plot
# #     print(p)
# #   }
# # }
# 
# 
# 
# # Loop through each zone and variable
# for (zone in zones) {
#   for (variable in variables) {
#     # Extract data for the current zone and variable
#     zone_data <- regression_models[[zone]]$model
#     x_data <- zone_data[[variable]]
#     
#     # Create a scatter plot with a trend line and R-squared value
#     p <- ggplot(zone_data, aes(x = .data[[variable]], y = aggregated_yield)) +
#       geom_point(color = "steelblue", size = 3) +
#       geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x_data) +
#       labs(title = paste("Scatter Plot with Trend Line -", zone, "-", variable),
#            x = variable,
#            y = "Aggregated Yield") +
#       geom_abline(intercept = coef(regression_models[[zone]])[1], slope = coef(regression_models[[zone]])[2], color = "blue") +
#       theme_minimal() +
#       geom_text(
#         x = max(x_data),
#         y = max(zone_data$aggregated_yield),
#         label = paste("R-squared =", round(summary(regression_models[[zone]])$r.squared, 4)),
#         hjust = 1,
#         vjust = 1,
#         color = "darkred"
#       )
#     
#     # Save the plot (optional)
#     ggsave(paste("scatter_plot_", zone, "_", variable, ".png"), plot = p, width = 8, height = 6, units = "in")
#     
#     # Display the plot
#     print(p)
#   }
# }
# 
# 
# 
# # Loop through each zone and variable
# for (zone in zones) {
#   for (variable in variables) {
#     # Extract data for the current zone and variable
#     zone_data <- regression_models[[zone]]$model
#     x_data <- zone_data[[variable]]
#     
#     # Create a scatter plot with a trend line and R-squared value
#     p <- ggplot(zone_data, aes(x = .data[[variable]], y = aggregated_yield)) +
#       geom_point(color = "steelblue", size = 3) +
#       geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x_data) +
#       labs(title = paste("Scatter Plot with Trend Line -", zone, "-", variable),
#            x = variable,
#            y = "Aggregated Yield") +
#       theme_minimal() +
#       geom_text(
#         x = max(x_data),
#         y = max(zone_data$aggregated_yield),
#         label = paste("R-squared =", round(summary(regression_models[[zone]])$r.squared, 4)),
#         hjust = 1,
#         vjust = 1,
#         color = "darkred"
#       )
#     
#     # Save the plot (optional)
#     ggsave(paste("scatter_plot_", zone, "_", variable, ".png"), plot = p, width = 8, height = 6, units = "in")
#     
#     # Display the plot
#     print(p)
#   }
# }
# 
# 
# 
# 
# # Loop through each zone and variable
# for (zone in zones) {
#   for (variable in variables) {
#     # Extract data for the current zone and variable
#     zone_data <- regression_models[[zone]]$model
#     x_data <- zone_data[[variable]]
#     
#     # Create a scatter plot with a trend line and R-squared value
#     p <- ggplot(zone_data, aes(x = .data[[variable]], y = aggregated_yield)) +
#       geom_point(color = "steelblue", size = 3) +
#       geom_smooth(method = "lm", se = FALSE, color = "darkred", formula = y ~ x) +
#       labs(title = paste("Scatter Plot with Trend Line -", zone, "-", variable),
#            x = variable,
#            y = "Aggregated Yield") +
#       theme_minimal() +
#       geom_text(
#         x = max(x_data),
#         y = max(zone_data$aggregated_yield),
#         label = paste("R-squared =", round(summary(regression_models[[zone]])$r.squared, 4)),
#         hjust = 1,
#         vjust = 1,
#         color = "darkred"
#       )
#     
#     # Save the plot (optional)
#     #ggsave(paste("scatter_plot_", zone, "_", variable, ".png"), plot = p, width = 8, height = 6, units = "in")
#     
#     # Display the plot
#     print(p)
#   }
# }
