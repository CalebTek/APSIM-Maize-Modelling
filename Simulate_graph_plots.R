# Save the plot
filename <- "figures/simulation/maize_yield_time_series.png"
png(
  filename,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the first plot
{
  plot(simulated_data$aggregated_yield_nigeria, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "darkgreen", 
       main = "Simulated Maize Yield Time Series for Nigeria (2021-2030)", axes = F)
  
  # Draw the left y-axis
  axis(2, pretty(range(simulated_data$aggregated_yield_nigeria)), col = "black",
      las = 3)
  mtext("Maize Yield", side = 2, line = 2.5)
  
  # Draw the time axis
  axis(1, at = 1:length(simulated_data$year), labels = simulated_data$year, col = "black", tick = TRUE)
  
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add a box around the first plot
  box()
  
}

dev.off()


# Save the plot
filename_maxt <- "figures/simulation/maxt_time_series.png"
png(
  filename_maxt,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the plot for Max Temperature
{
  plot(simulated_data$maxt, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "darkred", 
       main = "Simulated Max Temperature Time Series for Nigeria (2021-2030)", axes = F)
  
  # Draw the left y-axis
  axis(2, pretty(range(simulated_data$maxt)), col = "black",
       las = 3)
  mtext("Max Temperature", side = 2, line = 2.5)
  
  # Draw the time axis
  axis(1, at = 1:length(simulated_data$year), labels = simulated_data$year, col = "black", tick = TRUE)
  
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add a box around the plot
  box()
}

dev.off()


# Save the plot
filename_mint <- "figures/simulation/mint_time_series.png"
png(
  filename_mint,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the plot for Min Temperature
{
  plot(simulated_data$mint, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "darkorange", 
       main = "Simulated Min Temperature Time Series for Nigeria (2021-2030)", axes = F)
  
  # Draw the left y-axis
  axis(2, pretty(range(simulated_data$mint)), col = "black",
       las = 3)
  mtext("Min Temperature", side = 2, line = 2.5)
  
  # Draw the time axis
  axis(1, at = 1:length(simulated_data$year), labels = simulated_data$year, col = "black", tick = TRUE)
  
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add a box around the plot
  box()
}

dev.off()



# Save the plot
filename_rain <- "figures/simulation/rain_time_series.png"
png(
  filename_rain,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the plot for Rainfall
{
  plot(simulated_data$rain, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "darkgreen", 
       main = "Simulated Rainfall Time Series for Nigeria (2021-2030)", axes = F)
  
  # Draw the left y-axis
  axis(2, pretty(range(simulated_data$rain)), col = "black",
       las = 3)
  mtext("Rainfall", side = 2, line = 2.5)
  
  # Draw the time axis
  axis(1, at = 1:length(simulated_data$year), labels = simulated_data$year, col = "black", tick = TRUE)
  
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add a box around the plot
  box()
}

dev.off()



# Draw the right y-axis
axis(4, pretty(range(aggregated_nigeria$mint)), col = "darkorange", col.axis = "darkorange", las = 1)

# Add labels for the right y-axis
mtext("Min Temperature", side = 4, col = "darkorange", line = 2.5)


# Save the plot
filename <- "figures/simulation/secondaries/maize_yield_max_temperature_plot.png"
png(
  filename,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the first plot
{
  plot(simulated_data$aggregated_yield_nigeria, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "black", 
       main = "Simulated Maize Yield and Max Temperature for Nigeria (2021-2030)", axes = F)
  
  # Draw the left y-axis
  axis(2, pretty(range(simulated_data$aggregated_yield_nigeria)), col = "black",
       las = 3)
  mtext("Maize Yield", side = 2, line = 2.5)
  
  # Draw the time axis
  axis(1, at = 1:length(simulated_data$year), labels = simulated_data$year, col = "black", tick = TRUE)
  
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add a box around the first plot
  box()
  
  # Create the second plot on the same graph
  par(new = TRUE)
  
  
  plot(simulated_data$maxt, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "darkred", axes = F)
  
  # Draw the left y-axis
  axis(4, pretty(range(simulated_data$maxt)), col = "darkred",
       las = 1,col.axis = "darkred")
  mtext("Max Temperature", side = 4, line = 2.5, col = "darkred")
  
  # Add Legend
  legend("topleft", legend = c("Maize Yield", "Max Temperature"),
         text.col = c("black", "darkred"), pch = c(16, 15), col = c("black", "darkred"))
  
}

dev.off()



# Save the plot
filename_mint <- "figures/simulation/secondaries/maize_yield_min_temperature_plot.png"
png(
  filename_mint,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the first plot
{
  plot(simulated_data$aggregated_yield_nigeria, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "black", 
       main = "Simulated Maize Yield and Min Temperature for Nigeria (2021-2030)", axes = F)
  
  # Draw the left y-axis
  axis(2, pretty(range(simulated_data$aggregated_yield_nigeria)), col = "black",
       las = 3)
  mtext("Maize Yield", side = 2, line = 2.5)
  
  # Draw the time axis
  axis(1, at = 1:length(simulated_data$year), labels = simulated_data$year, col = "black", tick = TRUE)
  
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add a box around the first plot
  box()
  
  # Create the second plot on the same graph
  par(new = TRUE)
  
  plot(simulated_data$mint, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "darkorange", axes = F)
  
  # Draw the left y-axis
  axis(4, pretty(range(simulated_data$mint)), col = "darkorange",
       las = 1, col.axis = "darkorange")
  mtext("Min Temperature", side = 4, line = 2.5, col = "darkorange")
  
  # Add Legend
  legend("topleft", legend = c("Maize Yield", "Min Temperature"),
         text.col = c("black", "darkorange"), pch = c(16, 15), col = c("black", "darkorange"))
  
}

dev.off()



# Save the plot
filename_rain <- "figures/simulation/secondaries/maize_yield_rain_temperature_plot.png"
png(
  filename_rain,
  width     = 8,
  height    = 6,
  units     = "in",
  res       = 300
)
# Create the first plot
{
  plot(simulated_data$aggregated_yield_nigeria, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "black", 
       main = "Simulated Maize Yield and Rainfall for Nigeria (2021-2030)", axes = F)
  
  # Draw the left y-axis
  axis(2, pretty(range(simulated_data$aggregated_yield_nigeria)), col = "black",
       las = 3)
  mtext("Maize Yield", side = 2, line = 2.5)
  
  # Draw the time axis
  axis(1, at = 1:length(simulated_data$year), labels = simulated_data$year, col = "black", tick = TRUE)
  
  mtext("Year", side = 1, col = "black", line = 2.5)
  
  # Add a box around the first plot
  box()
  
  # Create the second plot on the same graph
  par(new = TRUE)
  
  plot(simulated_data$rain, pch = 16, 
       xlab = "", ylab = "", type = "b", col = "darkgreen", axes = F)
  
  # Draw the left y-axis
  axis(4, pretty(range(simulated_data$rain)), col = "darkgreen",
       las = 1, col.axis = "darkgreen")
  mtext("Rainfall", side = 4, line = 2.5, col = "darkgreen")
  
  # Add Legend
  legend("topleft", legend = c("Maize Yield", "Rainfall"),
         text.col = c("black", "darkgreen"), pch = c(16, 15), col = c("black", "darkgreen"))
  
}

dev.off()




















library(ggplot2)

# Assuming your maize data frame is named 'maize_data'
# Convert 'Year' to numeric
simulated_data$year <- as.numeric(as.character(simulated_data$year))

# Plot time series of Maize Yield (kg/ha)
ggplot(simulated_data, aes(x = year, y = aggregated_yield_nigeria)) +
  geom_line(color = "green") +
  geom_point(color = "darkgreen", fill = "darkgreen") +  # Set point color and fill
  labs(
    title = "Simulated Maize Yield Time Series for Nigeria (2021-2030)",
    x = "Year",
    y = "Maize Yield (kg/ha)"
  ) +
  theme_minimal()

# Save the plot to a file (optional)
ggsave(filename = paste0("figures/simulation/","maize_yield_time_series.jpg"), width = 10, height = 6, units = "in")





# Plot time series of Rainfall
ggplot(simulated_data, aes(x = year)) +
  geom_bar(aes(y = rain * 10, fill = "Rainfall"), stat = "identity", alpha = 0.7) +  # Scale for better visibility
  labs(
    title = "Simulated Rainfall Time Series for Nigeria (2021-2030)",
    x = "Year",
    y = "Rainfall (mm)",
    fill = ""
  ) +
  scale_fill_manual(values = "green") +  # Adjust fill color
  theme_minimal()
ggsave(filename = paste0("figures/simulation/","rainfall_barplot_time_series.jpg"), width = 10, height = 6, units = "in")




# Reshape the data for a stacked bar plot
temperature_stacked <- simulated_data %>%
  pivot_longer(cols = c(maxt, mint), names_to = "Temperatures", values_to = "Temperature")

# Plot stacked bar for Temperature
ggplot(temperature_stacked, aes(x = year, y = Temperature, fill = Temperatures)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, alpha = 0.9) +
  labs(
    title = "Simulated Temperature Time Series for Nigeria (2021-2030)",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  scale_fill_manual(values = c("maxt" = "#FFA500", "mint" = "#3F7CAC")) +  # Adjust color
  theme_minimal()

ggsave(filename = paste0("figures/simulation/","temperature_barplot_time_series.jpg"), width = 10, height = 6, units = "in")







# Plot time series of Temperature
ggplot(temperature_stacked, aes(x = year, y = Temperature, color = Temperatures)) +
  geom_line(size=0.5) +
  geom_point() +
  geom_hline(yintercept = mean(aggregated_nigeria$maxt), linetype = "dashed") +
  geom_hline(yintercept = mean(aggregated_nigeria$mint), linetype = "dashed") +
  labs(
    title = "Simulated Temperature Time Series for Nigeria (2021-2030)",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  scale_color_manual(values = c("maxt" = "#FFA500", "mint" = "#3F7CAC")) +  # Adjust color
  theme_minimal()

ggsave(filename = paste0("figures/simulation/","temperature_time_series.jpg"), width = 10, height = 6, units = "in")


# Plot time series of Rainfall
ggplot(simulated_data, aes(x = year, y = rain * 10, color = "Rainfall")) +
  geom_line(size = 0.5) +
  geom_point(color = "darkgreen", fill = "darkgreen") +
  geom_hline(yintercept = mean(aggregated_nigeria$rain * 10), linetype = "dashed") +
  labs(
    title = "Simulated Rainfall Time Series for Nigeria (2021-2030)",
    x = "Year",
    y = "Rainfall (mm)",
    color = ""
  ) +
  scale_color_manual(values = "green") +  # Adjust color
  theme_minimal()
ggsave(filename = paste0("figures/simulation/","rainfall_time_series.jpg"), width = 10, height = 6, units = "in")









##### 

# Plot time series of Maize Yield (kg/ha) with smoothing
ggplot(simulated_data, aes(x = year, y = aggregated_yield_nigeria)) +
  geom_line(color = "green") +
  geom_point(color = "darkgreen", fill = "darkgreen") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add smoothing
  labs(
    title = "Simulated Maize Yield Time Series for Nigeria (2021-2030)",
    x = "Year",
    y = "Maize Yield (kg/ha)"
  ) +
  theme_minimal()



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




