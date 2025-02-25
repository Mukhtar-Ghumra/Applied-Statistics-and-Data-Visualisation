# Install and load necessary packages
install.packages("tidyverse")
install.packages("corrplot")
install.packages("car")
install.packages("rcompanion")
install.packages("lmtest")
install.packages("randomForest")
install.packages("gbm")
install.packages("readxl")

# Load the necessary libraries
library(tidyverse)
library(corrplot)
library(car)
library(rcompanion)
library(lmtest)
library(randomForest)
library(gbm)
library(readxl)

# Load the dataset
concrete_data <- read_excel("concrete compressive strength.xlsx", sheet = 1)

# Explore the dataset
# View the structure and summary of the dataset
head(concrete_data)  # Preview the first few rows of the dataset
str(concrete_data)   # Understand the structure of the dataset
summary(concrete_data)  # View summary statistics of the dataset
names(concrete_data)  # Display column names
sum(is.na(concrete_data))  # Check for missing values

# Rename column names to make them more readable
names(concrete_data) <- c("Cement",
                          "BlastFurnaceSlag",
                          "FlyAsh",
                          "Water",
                          "Superplasticizer",
                          "CoarseAggregate",
                          "FineAggregate",
                          "Age",
                          "ConcreteCategory",
                          "ContainsFlyAsh",
                          "CompressiveStrength")

# Exploratory Data Analysis (EDA)
# Histograms to check the distribution of key variables
hist(concrete_data$Cement, main="Histogram of Cement", xlab="Cement (kg/m^3)")
hist(concrete_data$FlyAsh, main="Histogram of Fly Ash", xlab="Fly Ash (kg/m^3)")
hist(concrete_data$Water, main="Histogram of Water", xlab="Water (kg/m^3)")
hist(concrete_data$Age, main="Histogram of Age", xlab="Age (days)", col="blue", border="black")

# Box plots to check for outliers in key variables
boxplot(concrete_data$Cement, main="Box Plot of Cement", ylab="Cement (kg/m^3)")
boxplot(concrete_data$CompressiveStrength, main="Box Plot of Compressive Strength", ylab="Compressive Strength (MPa)")

# Density plots to understand the distribution of continuous variables
ggplot(concrete_data, aes(x = CompressiveStrength)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  ggtitle("Density Plot of Compressive Strength") +
  xlab("Compressive Strength (MPa)") +
  ylab("Density") +
  theme_minimal()

ggplot(concrete_data, aes(x = Cement)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  ggtitle("Density Plot of Cement") +
  xlab("Cement (kg/m^3)") +
  ylab("Density") +
  theme_minimal()

# Bar plots for categorical variables
barplot(table(concrete_data$ConcreteCategory), main="Bar Plot of Concrete Categories", xlab="Category", ylab="Frequency")
barplot(table(concrete_data$ContainsFlyAsh), main="Bar Plot of Fly Ash Presence", xlab="Contains Fly Ash", ylab="Frequency")

# Normality check using Q-Q plots and Shapiro-Wilk test
# List of columns to check for normality
columns_to_check <- c("Cement", "BlastFurnaceSlag", "FlyAsh", "Water", "CompressiveStrength")

# Loop through each column and create Q-Q plots
for (colname in columns_to_check) {
  qqnorm(concrete_data[[colname]], main = paste("Q-Q Plot for", colname))
  qqline(concrete_data[[colname]], col = "steelblue")
}

# Shapiro-Wilk normality test for selected variables
shapiro_test_fine_aggregate <- shapiro.test(concrete_data$FineAggregate)
print(paste("Shapiro-Wilk test for FineAggregate: p-value =", shapiro_test_fine_aggregate$p.value))

shapiro_test_age <- shapiro.test(concrete_data$Age)
print(paste("Shapiro-Wilk test for Age: p-value =", shapiro_test_age$p.value))

shapiro_test_coarse_aggregate <- shapiro.test(concrete_data$CoarseAggregate)
print(paste("Shapiro-Wilk test for CoarseAggregate: p-value =", shapiro_test_coarse_aggregate$p.value))

shapiro_test_superplasticizer <- shapiro.test(concrete_data$Superplasticizer)
print(paste("Shapiro-Wilk test for Superplasticizer: p-value =", shapiro_test_superplasticizer$p.value))

# Correlation Analysis
# Calculate the correlation matrix for numeric columns
cor_matrix <- round(cor(concrete_data[, sapply(concrete_data, is.numeric)], method = "spearman"), digits = 2)
print(cor_matrix)

# Visualize the correlation matrix using a correlation plot
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45,  # Text label color and rotation
         addCoef.col = "black")  # Color of the correlation coefficients

# Scatter plots to explore relationships between key variables
# Scatter plot for Cement vs. CompressiveStrength
ggplot(concrete_data, aes(x = Cement, y = CompressiveStrength)) +
  geom_point(aes(color = Cement), alpha = 0.6) +
  ggtitle("Cement vs. Compressive Strength") +
  xlab("Cement (kg/m^3)") +
  ylab("Compressive Strength (MPa)") +
  theme_minimal()

# Scatter plot for Age vs. Compressive Strength
ggplot(concrete_data, aes(x = Age, y = CompressiveStrength)) +
  geom_point(aes(color = Age), alpha = 0.6) +
  ggtitle("Age vs. Compressive Strength") +
  xlab("Age (days)") +
  ylab("Compressive Strength (MPa)") +
  theme_minimal()

# Scatter plot for Water vs. Compressive Strength
ggplot(concrete_data, aes(x = Water, y = CompressiveStrength)) +
  geom_point(aes(color = Water), alpha = 0.6) +
  ggtitle("Water vs. Compressive Strength") +
  xlab("Water (kg/m^3)") +
  ylab("Compressive Strength (MPa)") +
  theme_minimal()

# Cube Root Transformation for Variables
transformed_data <- concrete_data
transformed_data$Cement <- concrete_data$Cement^(1/3)
transformed_data$BlastFurnaceSlag <- concrete_data$BlastFurnaceSlag^(1/3)
transformed_data$FlyAsh <- concrete_data$FlyAsh^(1/3)
transformed_data$Water <- concrete_data$Water^(1/3)
transformed_data$Superplasticizer <- concrete_data$Superplasticizer^(1/3)
transformed_data$CoarseAggregate <- concrete_data$CoarseAggregate^(1/3)
transformed_data$FineAggregate <- concrete_data$FineAggregate^(1/3)
transformed_data$Age <- concrete_data$Age^(1/3)
transformed_data$CompressiveStrength <- concrete_data$CompressiveStrength^(1/3)

# Fit the Multiple Linear Regression Model
Multiple_linear_model <- lm(CompressiveStrength ~ Cement + BlastFurnaceSlag + FlyAsh + Water + Superplasticizer + Age, data = transformed_data)

# Summary of the Multiple Linear Model
model_summary <- summary(Multiple_linear_model)
print(model_summary)

# Assumption Checks for Multiple Linear Regression
# Q-Q plot of residuals
residuals_model <- residuals(Multiple_linear_model)
qqnorm(residuals_model, main = "Q-Q Plot of Residuals")
qqline(residuals_model, col = "red")

# Shapiro-Wilk test on the residuals
shapiro_test <- shapiro.test(residuals_model)
print(paste("Shapiro-Wilk test p-value:", shapiro_test$p.value))

# Residuals vs Fitted Values Plot
plot(Multiple_linear_model$fitted.values, residuals_model,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red")

# Histogram and density plot of residuals
hist(residuals_model, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black")
ggplot(data.frame(residuals_model), aes(x = residuals_model)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  ggtitle("Density Plot of Residuals") +
  xlab("Residuals") +
  ylab("Density") +
  theme_minimal()

# Multicollinearity Check using Variance Inflation Factor (VIF)
vif_values <- vif(Multiple_linear_model)
print("VIF values for each predictor:")
print(vif_values)

# Durbin-Watson Test for Independence of Residuals
dw_test <- dwtest(Multiple_linear_model)
print(dw_test)
if (dw_test$p.value < 0.05) {
  print("Reject the null hypothesis: There is evidence of autocorrelation among residuals.")
} else {
  print("Fail to reject the null hypothesis: There is no evidence of autocorrelation among residuals (independence holds).")
}

# Random Forest Regression
rf_model <- randomForest(CompressiveStrength ~ Cement + BlastFurnaceSlag + FlyAsh + Water + Superplasticizer + Age, data = transformed_data, ntree = 500)
print(rf_model)

# Predictions
rf_predictions <- predict(rf_model)

# Residuals Calculation
rf_residuals <- transformed_data$CompressiveStrength - rf_predictions

# Assumption Checks for Random Forest Regression

# Linearity Check
plot(rf_predictions, rf_residuals,
     main = "Residuals vs Fitted Values for Random Forest Regression",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red")

# Normality of Residuals
qqnorm(rf_residuals, main = "Q-Q Plot of Residuals for Random Forest Regression")
qqline(rf_residuals, col = "red")
shapiro_test_rf <- shapiro.test(rf_residuals)
print(paste("Shapiro-Wilk test p-value for Random Forest Regression:", shapiro_test_rf$p.value))

# Homoscedasticity Check
plot(rf_predictions, abs(rf_residuals),
     main = "Scale-Location Plot for Random Forest Regression",
     xlab = "Fitted Values",
     ylab = "|Residuals|",
     pch = 20)
abline(h = 0, col = "red")

# Gradient Boosting Regression
gb_model <- gbm(CompressiveStrength ~ Cement + BlastFurnaceSlag + FlyAsh + Water + Superplasticizer + Age + CoarseAggregate + FineAggregate, 
                data = transformed_data, 
                distribution = "gaussian", 
                n.trees = 5000, 
                interaction.depth = 4)
print(summary(gb_model))

# Assumption Checks for Gradient Boosting Regression

# Predictions
gb_predictions <- predict(gb_model, n.trees = 5000)

# Residuals Calculation
gb_residuals <- transformed_data$CompressiveStrength - gb_predictions

# Linearity Check
plot(gb_predictions, gb_residuals,
     main = "Residuals vs Fitted Values for Gradient Boosting Regression",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20)
abline(h = 0, col = "red")

# Normality of Residuals
qqnorm(gb_residuals, main = "Q-Q Plot of Residuals for Gradient Boosting Regression")
qqline(gb_residuals, col = "red")
shapiro_test_gb <- shapiro.test(gb_residuals)
print(paste("Shapiro-Wilk test p-value for Gradient Boosting Regression:", shapiro_test_gb$p.value))

# Homoscedasticity Check
plot(gb_predictions, abs(gb_residuals),
     main = "Scale-Location Plot for Gradient Boosting Regression",
     xlab = "Fitted Values",
     ylab = "|Residuals|",
     pch = 20)
abline(h = 0, col = "red")

# t-test for Fly Ash Presence
t_test_result <- t.test(CompressiveStrength ~ ContainsFlyAsh, data = concrete_data)
print(t_test_result)

# One-Way ANOVA for Concrete Category
anova_result <- aov(CompressiveStrength ~ ConcreteCategory, data = concrete_data)
summary(anova_result)

# Paired t-test for Water and Superplasticizer Effects
paired_t_test <- t.test(transformed_data$Water, transformed_data$Superplasticizer, paired = TRUE)
print("Paired t-test for Water and Superplasticizer Effects:")
print(paired_t_test)

# Chi-Square Test for Fly Ash and Concrete Category
fly_ash_category_table <- table(transformed_data$ContainsFlyAsh, transformed_data$ConcreteCategory)
chi_square_test <- chisq.test(fly_ash_category_table)
print("Chi-Square Test for Fly Ash and Concrete Category:")
print(chi_square_test)
