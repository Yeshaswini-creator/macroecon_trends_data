

# Inspect the data
summary(eco_prj)
str(eco_prj)

# Remove rows with NA values
dataset_clean <- na.omit(eco_prj)

# Replace `dataset` with your actual dataset name
dataset_clean <- eco_prj[!apply(eco_prj == 0, 1, any), ]


install.packages("dplyr")

library(dplyr)


# Convert Year and Quarter into a Date format (if applicable)
eco_prj$Date <- as.Date(paste0(eco_prj$YEAR, "-Q", eco_prj$QTR), format="%Y-Q%q")
# Create a Month column based on the quarter (QTR)
eco_prj$Month <- case_when(
  eco_prj$QTR == 1 ~ 1,   # Q1 starts in January
  eco_prj$QTR == 2 ~ 4,   # Q2 starts in April
  eco_prj$QTR == 3 ~ 7,   # Q3 starts in July
  eco_prj$QTR == 4 ~ 10   # Q4 starts in October
)

# Combine YEAR and Month to form a valid date
eco_prj$Date <- as.Date(paste(eco_prj$YEAR, eco_prj$Month, "01", sep = "-"), format = "%Y-%m-%d")

# Remove the temporary Month column if not needed
eco_prj$Month <- NULL

head(eco_prj$Date)
summary(eco_prj$Date)


# Step 2: Exploratory Data Analysis (EDA)

# Descriptive Statistics
summary(eco_prj)

# Time Series Plot for Real GDP using base R
plot(eco_prj$Date, eco_prj$REALGDP, 
     type = "l",  # "l" for line plot
     main = "REALGDP Over Time", 
     xlab = "Date", 
     ylab = "REALGDP ($bil)")


# Scatter plot for Real GDP vs Unemployment using base R
plot(eco_prj$UNEMP, eco_prj$REALGDP,
     main = "Real GDP vs Unemployment Rate",
     xlab = "Unemployment Rate",
     ylab = "Real GDP ($bil)",
     pch = 19,    # Sets the point style to solid circles
     col = "blue" # Color of the points
)

# Add a linear regression line
model <- lm(REALGDP ~ UNEMP, data = eco_prj)  # Fit a linear model
abline(model, col = "red", lwd = 2)  # Add the regression line in red with increased line width

# Optionally, add a legend
legend("topright", legend = c("Data points", "Regression line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))


# Correlation Analysis
correlations <- eco_prj %>% select(REALGDP, REALCONS, REALINVS, REALGOVT, CPI_U, TBILRATE, UNEMP, POP, INFL, REALINT)
cor_matrix <- cor(correlations, use = "complete.obs")
print(cor_matrix)


# Step 3: Statistical Testing

# Correlation analysis for Real GDP and other variables
correlations <- eco_prj %>% select(REALGDP, REALCONS, REALINVS, REALGOVT, CPI_U, TBILRATE, UNEMP,INFL)
cor_matrix <- cor(correlations, use = "complete.obs")
print(cor_matrix)


#Research Questions & Hypothesis Formulation
# Load necessary libraries
library(dplyr)
# OLS regression model for Research Question 1
model1 <- lm(REALGDP ~ REALCONS + REALINVS + REALGOVT, data = eco_prj)

# Display the summary of the model
summary(model1)

# Load the car package if not already installed
if (!require(car)) install.packages("car")
library(car)
# Calculate VIF for the model(Checking multicollinearity)
vif_values <- vif(model1)
print(vif_values)

#Principal Component Analysis (PCA)
# Perform PCA on the predictor variables
pca_result <- prcomp(eco_prj[, c("REALCONS", "REALINVS", "REALGOVT")], scale. = TRUE)
# Summary of PCA to check variance explained by each component
summary(pca_result)

# Use the principal components in a new regression model
pca_scores <- as.data.frame(pca_result$x)
model_pca <- lm(REALGDP ~ pca_scores$PC1 + pca_scores$PC2 + pca_scores$PC3, data = eco_prj)
summary(model_pca)

# OLS regression model for Research Question 2
model2 <- lm(REALGDP ~ CPI_U + TBILRATE, data = eco_prj)
# Display the summary of the model
summary(model2)
# Load the car package if not already installed
if (!require(car)) install.packages("car")
library(car)

# Calculate VIF for the model(checking multicollinerarity)
vif_values <- vif(model2)
print(vif_values)


# OLS regression model for Research Question 3
model3 <- lm(REALGDP ~ UNEMP, data = eco_prj)
# Display the summary of the model
summary(model3)

# Fit a multiple regression model with all predictors
model_full <- lm(REALGDP ~ REALCONS + REALINVS + REALGOVT + CPI_U + TBILRATE + UNEMP + INFL, data = eco_prj)
# Display the summary of the model
summary(model_full)

# Check for multicollinearity
if (!require(car)) install.packages("car")
library(car)
vif_values <- vif(model_full)
print(vif_values)

#multicollinearity
# Fit a reduced model with selected predictors to reduce multicollinearity
model_reduced <- lm(REALGDP ~ REALCONS + CPI_U + TBILRATE + UNEMP + INFL, data = eco_prj)
# Display the summary of the reduced model
summary(model_reduced)
# Check VIF for the reduced model
vif_reduced <- vif(model_reduced)
print(vif_reduced)

# Fit a further reduced model without CPI_U
model_further_reduced <- lm(REALGDP ~ REALCONS + TBILRATE + UNEMP + INFL, data = eco_prj)
# Display the summary of the further reduced model
summary(model_further_reduced)

# Check VIF for the further reduced model(checking multicollinerarity)
vif_further_reduced <- vif(model_further_reduced)
print(vif_further_reduced)


# Step 5: Model Diagnostics
# Plot residuals for homoscedasticity and normality checks
par(mfrow = c(2, 2))
plot(model)

#Step 6:
#Explanation of Fixed and Random Effects Models
if (!require(plm)) install.packages("plm")
library(plm)

eco_prj_panel <- pdata.frame(eco_prj, index = c("YEAR"))

fixed_model <- plm(REALGDP ~ REALCONS + REALINVS + REALGOVT + CPI_U + TBILRATE + UNEMP + INFL, 
                   data = eco_prj_panel, 
                   model = "within")
summary(fixed_model)


random_model <- plm(REALGDP ~ REALCONS + REALINVS + REALGOVT + CPI_U + TBILRATE + UNEMP + INFL, 
                    data = eco_prj_panel, 
                    model = "random")
summary(random_model)

#Hausman test
phtest(fixed_model, random_model)
