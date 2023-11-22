# Load necessary libraries
library(dplyr)

# Assuming 'met_data' is your dataset
# Replace 'met_data' with the actual name of your dataset

# List of ecological zones
ecological_zones <- unique(aggregated_by_zone$Ecological_Zone)

# Create an empty list to store results
correlation_results <- list()
regression_results <- list()
regression_models <- list()

# Loop through each ecological zone
for (zone in ecological_zones) {
  # Subset data for the current zone
  zone_data <- aggregated_by_zone %>% filter(Ecological_Zone == zone)
  
  # Aggregate maize yield for the zone (assuming 'maize_yield' is the column name)
  #aggregated_yield <- sum(zone_data$maize_yield, na.rm = TRUE)
  
  # Add the aggregated yield to the data for the zone
  zone_data <- mutate(zone_data, aggregated_yield = maize_data$`Maize Yield (kg/ha)`)
  
  # Correlation Analysis
  correlation_matrix <- cor(zone_data[, c("maxt", "mint", "rain", "aggregated_yield")])
  correlation_results[[zone]] <- correlation_matrix
  
  # Regression Analysis
  regression_model <- lm(aggregated_yield ~ maxt + mint + rain, data = zone_data)
  regression_models[[zone]] <- regression_model
  regression_results[[zone]] <- summary(regression_model)
}

# # Access correlation and regression results for each zone as needed
# # Example: Access correlation matrix for 'NC' zone
# correlation_results_NC <- correlation_results[["NC"]]
# 
# # Example: Access regression summary for 'NC' zone
# regression_summary_NC <- regression_results[["NC"]]



# Assuming 'met_data' is your dataset
# Replace 'met_data' with the actual name of your dataset

# Aggregate maize yield for the entire country (assuming 'maize_yield' is the column name)
# aggregated_yield_nigeria <- sum(met_data$maize_yield, na.rm = TRUE)

# Add the aggregated yield to the data for the entire country
aggregated_nigeria <- mutate(aggregated_nigeria, aggregated_yield_nigeria = maize_data$`Maize Yield (kg/ha)`)

# Correlation Analysis
correlation_matrix_nigeria <- cor(aggregated_nigeria[, c("maxt", "mint", "rain", "aggregated_yield_nigeria")])

# Regression Analysis
regression_model_nigeria <- lm(aggregated_yield_nigeria ~ maxt + mint + rain, data = aggregated_nigeria)
regression_summary_nigeria <- summary(regression_model_nigeria)

# Access correlation and regression results for the entire country as needed
# Example: Access correlation matrix for the entire country
correlation_matrix_nigeria

# Example: Access regression summary for the entire country
regression_summary_nigeria






# 
# 
# 
# # Create a function to extract correlation coefficients from the results
# extract_correlation_coefficients <- function(correlation_matrix) {
#   coefficients <- as.data.frame(as.table(correlation_matrix))
#   colnames(coefficients) <- c("Variable_1", "Variable_2", "Correlation_Coefficient")
#   return(coefficients)
# }
# 
# # Create a data frame for each zone and Nigeria
# correlation_dataframes <- lapply(names(correlation_results), function(zone) {
#   correlation_matrix <- correlation_results[[zone]]
#   data.frame(Ecological_Zone = zone, extract_correlation_coefficients(correlation_matrix))
# })
# 
# # Combine the data frames into a single data frame
# all_correlation_data <- do.call(rbind, correlation_dataframes)
# 
# # Save the combined correlation data as a CSV file
# write.csv(all_correlation_data, file = "result/tables/correlation_results.csv", row.names = FALSE)
# 
# 
