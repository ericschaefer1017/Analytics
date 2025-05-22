
# ALY6015 Module 1 Assignment
# Eric Schaefer
# This script performs regression diagnostics and model evaluation on the AmesHousing dataset.

# Step 1: Load required libraries
# These libraries help with data cleaning, visualization, diagnostics, and modeling
library(tidyverse)     # Core data manipulation and visualization tools
library(corrplot)      # For plotting correlation matrices
library(car)           # For Variance Inflation Factor (VIF)
library(MASS)          # For stepwise regression using stepAIC
library(ggplot2)       # For making plots

# Step 2: Load the dataset
# Read the dataset into memory; change the path as needed to where your CSV file is located
df <- read.csv("AmesHousing-1.csv")

# Step 3: Clean the dataset
# Remove houses with more than 4000 sq ft of living area, as recommended, to eliminate extreme outliers
df <- df %>% filter(Gr.Liv.Area < 4000)

# Step 4: Impute missing numeric values
# Replace missing values in numeric columns with the column mean to allow regression modeling
df_clean <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Step 5: Create a correlation matrix of numeric features
# Helps identify variables most and least correlated with SalePrice
numeric_vars <- df_clean %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Step 6: Save and interpret the correlation matrix plot
# This shows which variables are strongly or weakly correlated with SalePrice
png("correlation_matrix.png", width = 800, height = 600)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
dev.off()

# Step 7: Identify variables with high, low, and moderate correlation with SalePrice
cor_saleprice <- sort(cor_matrix[,"SalePrice"])
cor_target <- cor_saleprice[abs(cor_saleprice - 0.5) == min(abs(cor_saleprice - 0.5))]

# Store variable names for plotting
high_corr_var <- names(tail(cor_saleprice, 2))[1]   # Highest correlation
low_corr_var <- names(head(cor_saleprice, 1))       # Lowest correlation
mid_corr_var <- names(cor_target)[1]                # Closest to 0.5

# Step 8: Create scatter plots to visually examine relationships
# These plots help understand the linearity and strength of relationship with SalePrice

# High correlation
ggsave("scatter_high_corr.png", ggplot(df_clean, aes_string(x = high_corr_var, y = "SalePrice")) +
         geom_point(alpha = 0.6) + ggtitle(paste("High Correlation with", high_corr_var)))

# Low correlation
ggsave("scatter_low_corr.png", ggplot(df_clean, aes_string(x = low_corr_var, y = "SalePrice")) +
         geom_point(alpha = 0.6) + ggtitle(paste("Low Correlation with", low_corr_var)))

# Mid (~0.5) correlation
ggsave("scatter_mid_corr.png", ggplot(df_clean, aes_string(x = mid_corr_var, y = "SalePrice")) +
         geom_point(alpha = 0.6) + ggtitle(paste("Mid Correlation (~0.5) with", mid_corr_var)))

# Step 9: Fit a regression model using three continuous predictors
# These predictors were selected based on correlation strength with SalePrice
model1 <- lm(SalePrice ~ Gr.Liv.Area + Garage.Area + Total.Bsmt.SF, data = df_clean)
summary(model1)  # Display coefficients, R-squared, and p-values

# Step 10: Plot regression diagnostics
# Helps identify violations of regression assumptions
png("diagnostics_model1.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(model1)
dev.off()

# Step 11: Check for multicollinearity using VIF
# High VIF (>5 or 10) suggests multicollinearity issues
vif(model1)

# Step 12: Identify and visualize outliers using Cook's Distance
# Cook's distance shows which data points overly influence the regression model
cooksd <- cooks.distance(model1)
png("cooks_distance.png", width = 800, height = 600)
plot(cooksd, ylab = "Cook's Distance", main = "Cook's Distance Plot")
abline(h = 4/(nrow(df_clean)-length(model1$coefficients)-2), col = "red")
dev.off()

# Step 13: Remove influential outliers if any are above threshold
influential <- as.numeric(names(cooksd)[(cooksd > 4/(nrow(df_clean)))])
df_refined <- df_clean[-influential, ]

# Step 14: Refit the model using the cleaned dataset
model2 <- lm(SalePrice ~ Gr.Liv.Area + Garage.Area + Total.Bsmt.SF, data = df_refined)
summary(model2)

# Step 15: Perform stepwise regression to find best model
# Uses AIC to add/remove predictors to improve model fit
full_model <- lm(SalePrice ~ ., data = df_clean %>% select(where(is.numeric)))
best_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(best_model)

# Final model summaries used for comparison in written report
summary(model1)
summary(model2)
summary(best_model)

