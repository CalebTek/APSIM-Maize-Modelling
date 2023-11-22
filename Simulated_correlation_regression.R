# Correlation Analysis
correlation_matrix_simulated <- cor(simulated_data[, c("maxt", "mint", "rain", "aggregated_yield_nigeria")])

# Regression Analysis
regression_model_simulated <- lm(aggregated_yield_nigeria ~ maxt + mint + rain, data = simulated_data)
regression_mode_simulated_summary <- summary(regression_model_simulated)

# Access correlation and regression results for the entire country as needed
# Example: Access correlation matrix for the entire country
correlation_matrix_simulated

# Example: Access regression summary for the entire country
regression_mode_simulated_summary
