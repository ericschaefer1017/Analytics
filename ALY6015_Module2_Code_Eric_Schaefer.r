# -----------------------------------------------
# ALY6015 Module 2 Assignment R Script
# Author: Eric Schaefer
# Datasets used in this analysis:
#   1. baseball.csv - Used for Task 9 (Chi-Square test of wins by decade)
#   2. crop_data.csv - Used for Task 10 (Two-way ANOVA on crop yield)
#   3. Inline data - Tasks 1 through 8 use hardcoded data values in the script
# -----------------------------------------------


# ALY6015 Module 2 Assignment - Eric Schaefer
# Hypothesis testing using Chi-square and ANOVA in R
# Required libraries
# This step is necessary for preparing or running the statistical test below
library(dplyr)       # For data manipulation
# This step is necessary for preparing or running the statistical test below
library(tidyr)       # For data reshaping
# We visualize the group differences to better understand variance visually
library(ggplot2)     # For data visualization
# This step is necessary for preparing or running the statistical test below
library(readr)       # For reading external .csv files
# This step is necessary for preparing or running the statistical test below
library(car)         # For advanced ANOVA summaries

# ---------------------------
# Task 1: Blood Types
# ---------------------------
# Step 1: Define observed frequencies from sample
# This step is necessary for preparing or running the statistical test below
observed <- c(A=12, B=8, O=24, AB=6)

# Step 2: Define expected probabilities from the population
# This step is necessary for preparing or running the statistical test below
expected_probs <- c(0.20, 0.28, 0.36, 0.16)

# Step 3: Calculate expected counts based on sample size (n=50)
# This step is necessary for preparing or running the statistical test below
expected_counts <- sum(observed) * expected_probs

# Step 4: Run chi-square goodness-of-fit test
# We run a Chi-square test to assess if the observed frequencies differ from expected
chi1 <- chisq.test(x = observed, p = expected_probs)

# Step 5: Visualize observed vs expected
# We visualize the group differences to better understand variance visually
png("task1_bloodtypes_barplot.png")
# We visualize the group differences to better understand variance visually
barplot(rbind(observed, round(expected_counts)), beside=TRUE, col=c("skyblue", "orange"),
# This step is necessary for preparing or running the statistical test below
        legend = c("Observed", "Expected"), main="Blood Type Distribution")
# This step is necessary for preparing or running the statistical test below
dev.off()

# ---------------------------
# Task 2: Airline Delays
# ---------------------------
# Step 1: Input observed frequencies from airline company
# This step is necessary for preparing or running the statistical test below
observed2 <- c(OnTime=125, NAS=10, Late=25, Other=40)

# Step 2: Input expected government proportions
# This step is necessary for preparing or running the statistical test below
expected2 <- c(70.8, 8.2, 9.0, 12.0) / 100  # Convert % to proportion

# Step 3: Run chi-square goodness-of-fit test
# We run a Chi-square test to assess if the observed frequencies differ from expected
chi2 <- chisq.test(x = observed2, p = expected2)

# ---------------------------
# Task 3: Movie Admissions and Ethnicity
# ---------------------------
# Step 1: Create contingency table for admissions across ethnicities over 2 years
# This step is necessary for preparing or running the statistical test below
observed3 <- matrix(c(724,335,174,107, 370,292,152,140), nrow=2, byrow=TRUE)
# This step is necessary for preparing or running the statistical test below
dimnames(observed3) <- list(Year = c("2013", "2014"),
# This step is necessary for preparing or running the statistical test below
                            Ethnicity = c("Caucasian", "Hispanic", "African American", "Other"))

# Step 2: Run chi-square test for independence
# We run a Chi-square test to assess if the observed frequencies differ from expected
chi3 <- chisq.test(observed3)

# ---------------------------
# Task 4: Women in the Military
# ---------------------------
# Step 1: Build contingency table of ranks and branches
# This step is necessary for preparing or running the statistical test below
observed4 <- matrix(c(10791,62491, 7816,42750, 932,9525, 11819,54344), nrow=4, byrow=TRUE)
# This step is necessary for preparing or running the statistical test below
colnames(observed4) <- c("Officers", "Enlisted")
# This step is necessary for preparing or running the statistical test below
rownames(observed4) <- c("Army", "Navy", "Marine Corps", "Air Force")

# Step 2: Run chi-square test for independence
# We run a Chi-square test to assess if the observed frequencies differ from expected
chi4 <- chisq.test(observed4)

# ---------------------------
# Task 5: Sodium Contents of Foods
# ---------------------------
# Step 1: Create dataset of sodium values grouped by food category
# This step is necessary for preparing or running the statistical test below
sodium <- data.frame(
# This step is necessary for preparing or running the statistical test below
  amount = c(270,130,230,180,80,70,200, 260,220,290,290,200,320,140, 100,180,250,250,300,360,300,160),
# This step is necessary for preparing or running the statistical test below
  category = factor(rep(c("Condiments", "Cereals", "Desserts"), times=c(7,7,8)))
# This step is necessary for preparing or running the statistical test below
)

# Step 2: Run one-way ANOVA
# We fit an ANOVA model to test for differences in means across groups
anova5 <- aov(amount ~ category, data = sodium)

# Step 3: Visualize distribution
# We visualize the group differences to better understand variance visually
png("task5_sodium_boxplot.png")
# We visualize the group differences to better understand variance visually
ggplot(sodium, aes(x = category, y = amount, fill = category)) + geom_boxplot() + theme_minimal() + labs(title="Sodium Content by Food Category")
# This step is necessary for preparing or running the statistical test below
dev.off()

# ---------------------------
# Task 6: Sales for Leading Companies
# ---------------------------
# Step 1: Create dataset of product sales
# This step is necessary for preparing or running the statistical test below
sales <- data.frame(
# This step is necessary for preparing or running the statistical test below
  value = c(578,320,264,249,237, 311,106,109,125,173, 261,185,302,689,250),  # Added missing value for Coffee
# This step is necessary for preparing or running the statistical test below
  type = factor(rep(c("Cereal", "Chocolate", "Coffee"), each = 5))
# This step is necessary for preparing or running the statistical test below
)

# Step 2: Run one-way ANOVA
# We fit an ANOVA model to test for differences in means across groups
anova6 <- aov(value ~ type, data = sales)

# Step 3: Visualize sales distribution
# We visualize the group differences to better understand variance visually
png("task6_sales_boxplot.png")
# We visualize the group differences to better understand variance visually
ggplot(sales, aes(x = type, y = value, fill = type)) + geom_boxplot() + theme_minimal() + labs(title="Sales Comparison by Product Type")
# This step is necessary for preparing or running the statistical test below
dev.off()

# ---------------------------
# Task 7: Per-Pupil Expenditures
# ---------------------------
# Step 1: Create dataset of per-pupil expenditures by region
# This step is necessary for preparing or running the statistical test below
exp <- data.frame(
# This step is necessary for preparing or running the statistical test below
  amount = c(4946,5953,6202,7243,6113, 6149,7451,6000,6479, 5282,8605,6528,6911),
# This step is necessary for preparing or running the statistical test below
  region = factor(rep(c("Eastern", "Middle", "Western"), times = c(5,4,4)))
# This step is necessary for preparing or running the statistical test below
)

# Step 2: Run one-way ANOVA
# We fit an ANOVA model to test for differences in means across groups
anova7 <- aov(amount ~ region, data = exp)

# ---------------------------
# Task 8: Increasing Plant Growth (Two-way ANOVA)
# ---------------------------
# Step 1: Input growth data
# This step is necessary for preparing or running the statistical test below
growth <- data.frame(
# This step is necessary for preparing or running the statistical test below
  Growth = c(9.2,9.4,8.9,8.5,9.2,8.9, 7.1,7.2,8.5,5.5,5.8,7.6),
# This step is necessary for preparing or running the statistical test below
  Light = factor(rep(c("Light1", "Light2"), each = 6)),
# This step is necessary for preparing or running the statistical test below
  Food = factor(rep(c("A", "B"), each = 3, times = 2))
# This step is necessary for preparing or running the statistical test below
)

# Step 2: Run two-way ANOVA with interaction
# We fit an ANOVA model to test for differences in means across groups
anova8 <- aov(Growth ~ Light * Food, data = growth)

# Step 3: Create interaction plot
# This step is necessary for preparing or running the statistical test below
png("task8_growth_interaction.png")
# This step is necessary for preparing or running the statistical test below
interaction.plot(growth$Light, growth$Food, growth$Growth, col=c("blue", "darkgreen"), lty=1, type="b", main="Interaction Between Light and Food on Growth")
# This step is necessary for preparing or running the statistical test below
dev.off()

# ---------------------------
# Task 9: Baseball Dataset Analysis
# ---------------------------
# Step 1: Read baseball dataset and create Decade column
# This step is necessary for preparing or running the statistical test below
bb <- read_csv("baseball.csv")
# We create a new variable for decade to test if wins differ by decade
bb$Decade <- bb$Year - (bb$Year %% 10)

# Step 2: Summarize wins by decade
# We create a new variable for decade to test if wins differ by decade
wins <- bb %>% group_by(Decade) %>% summarise(Wins = sum(W))

# Step 3: Run chi-square test on wins distribution
# We run a Chi-square test to assess if the observed frequencies differ from expected
chi9 <- chisq.test(wins$Wins)

# ---------------------------
# Task 10: Crop Data Analysis (Two-way ANOVA with Blocking)
# ---------------------------
# Step 1: Load crop data
# This step is necessary for preparing or running the statistical test below
crop <- read_csv("crop_data-1.csv")

# Step 2: Convert variables to factors for ANOVA
# This step is necessary for preparing or running the statistical test below
crop <- crop %>%
# This step is necessary for preparing or running the statistical test below
  mutate(density = as.factor(density),
# This step is necessary for preparing or running the statistical test below
         fertilizer = as.factor(fertilizer),
# This step is necessary for preparing or running the statistical test below
         block = as.factor(block))

# Step 3: Run two-way ANOVA with blocking
# We fit an ANOVA model to test for differences in means across groups
anova10 <- aov(yield ~ density * fertilizer + Error(block), data = crop)

# Step 4: Boxplot of yield by factor interaction
# We visualize the group differences to better understand variance visually
png("task10_crop_boxplot.png")
# We visualize the group differences to better understand variance visually
ggplot(crop, aes(x = interaction(density, fertilizer), y = yield, fill = fertilizer)) +
# We visualize the group differences to better understand variance visually
  geom_boxplot() + theme_minimal() +
# This step is necessary for preparing or running the statistical test below
  labs(title="Crop Yield by Density and Fertilizer")
# This step is necessary for preparing or running the statistical test below
dev.off()

