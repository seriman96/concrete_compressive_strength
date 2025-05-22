setwd("D:/Data Science/1st_Semester/Regression_model/Proposal")

library(ggplot2)
library(car)
library(corrplot)
library(infotheo)
library(olsrr)
library(lmtest)

# step 1
data <-read.csv("./Concrete Compressive Strength.csv")
head(data)
names(data) # name of present feature in the dataset
class(data$Water) # class of the given feature
str(data) # it like info() in python
summary(data) #like describe() in python

# Step 2: Clean column names (if needed)
colnames(data) <- gsub("\\s+", "_", colnames(data))

# Step 3: Correlation analysis
correlation_matrix <- cor(data)
print(correlation_matrix)
corrplot::corrplot(correlation_matrix, method = "number") # 'arg' should be one of “circle”, “square”,
# “ellipse”, “number”, “shade”, “color”, “pie”

# Step 4: Visualize relationships
pairs(data[, c("Cement", "Blast.Furnace.Slag", "Fly.Ash", 
               "Water", "Superplasticizer", "Coarse.Aggregate", "Fine.Aggregate",
               "Age..day.", "Concrete.compressive.strength")], main = "Scatterplot Matrix")

# Define a function for mutual information scores:
mutual_info_scores <- function(data, response_var, top_n = 3) {
  # Ensure the response variable is a factor for discretization
  data[[response_var]] <- as.factor(data[[response_var]])
  
  # Convert data to a discretized version (required for mutual information)
  disc_data <- discretize(data, disc = "equalfreq")
  
  # Calculate mutual information for all predictors against the response variable
  mi_scores <- sapply(colnames(disc_data), function(var) {
    if (var != response_var) {
      mutinformation(disc_data[[var]], disc_data[[response_var]]) # Calculates the mutual information score between two variables.
    } else {
      NA  # Skip the response variable itself
    }
  })
  
  # Format the results into a data frame
  mi_df <- data.frame(Variable = colnames(data), MI_Score = mi_scores)
  mi_df <- mi_df[!is.na(mi_df$MI_Score), ]  # Remove NA values
  print(mi_df)
  
  # Plot the mutual information scores
  p <- ggplot(mi_df, aes(x = reorder(Variable, MI_Score), y = MI_Score)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Mutual Information Scores", x = "Predictor Variables", y = "Mutual Information") +
    theme_minimal()
  print(p)
  
}

# Apply the function to your dataset:
# Example: Assuming the response variable is "Concrete compressive strength"
mi_results <- mutual_info_scores(data, response_var = "Concrete.compressive.strength")

## Get the top predictors
# t <- reorder(mi_df$Variable, mi_df$MI_Score)
# print(t)
# top_predictors <- head(mi_df$Variable, top_n)
top_predictors <- c("Age..day.", "Cement", "Water")
print(top_predictors)
response_var = "Concrete.compressive.strength"
# Plot relationships for the top predictors
for (var in top_predictors) {
  h <- ggplot(data, aes_string(x = var, y = response_var)) +
    geom_point(alpha = 0.6, color = "blue") +
    labs(title = paste("Relationship between", var, "and", response_var),
         x = var, y = response_var) +
    theme_minimal()
  print(h)
}

# Step 5: OLS Regression Analysis

# Fit the OLS model
model <- lm(Concrete.compressive.strength ~ ., data = data)
ols_regress(model) #sig. in output table represent p-value

# Define the alpha level and degrees of freedom
alpha <- 0.05
df1 <- 1    # Numerator degrees of freedom (commonly 1 for a simple test)
df2 <- 1021 # Denominator degrees of freedom

# Compute the F-value at alpha = 0.05
f_value <- qf(1 - alpha, df1, df2)

# Print the F-value
print(f_value)

# from anova table we've:
f0 <- 204.269
cat("F0 = ", f0, ">" , "F = ", f_value, "\n")



# Question 1: Is there a relationship between the predictors (age and ingredients) and the response variable (compressive strength)?
  
  # Null hypothesis: coefficients for each predictor is zero.

# F-statistic = 204.3 >> 1 (suggests at least one of the predictors is related to compressive strength)

# F0 = 3.850583 << F-statistic (associated to the probability that the null hypothesis is true)

# Therefore, there is a relationship between the predictors and the response variable.

# Question 2: How strong is the relationship?
  
  # R-squared = 0.616 (61.6% of variance is explained by the model)

# Question 3: Which predictors contribute to compressive strength?
  
  # Look at the p-values for each t-statistic for each predictor where p-values are the probability of 
# t-statistic given the null hypothesis is true. A probability less than 1/20 (0.05) is considered sufficient to reject the null hypothesis.

# All are less than 0.05 except coarse and fine aggregates. Therefore, the aggregates do not contribute to compressive strength in this model.


# Question 4: How large is the effect of each predictor on compressive strength?
# The only predictor confidence interval to include zero is coarse aggregate. The rest are considered to be statistically significant. 
# To test whether collinearity is the reason why the confidence interval is so wide for coarse aggregate, the VIF scores are calculated.

## VIF scores for each feature
vif(model)
# If the VIF values are large (typically above 10), it indicates multicollinearity among predictors.

# The VIF scores exceeding 5 to 10 indicate collinearity (where 1 is the minimum). 
# Fine.Aggregate, Blast.Furnace.Slag, water, Fly.Ash, and cement exhibit multicollinearity. 
# Coarse.Aggregate is within the 5 to 10 range, so if being conservative then Coarse.Aggregate 
# also demonstrates multicollinearity. Therefore, we cannot say that cement and water are 
# or are not statistically significant, because the wideness of the confidence interval 
# could be due to multicollinearity.


# To assess association of each predictor, seperate OLS for each predictor is performed:

model1 <- lm(Concrete.compressive.strength ~ Cement, data = data)
model2 <- lm(Concrete.compressive.strength ~ Blast.Furnace.Slag, data = data)
model3 <- lm(Concrete.compressive.strength ~ Fly.Ash, data = data)
model4 <- lm(Concrete.compressive.strength ~ Water, data = data)
model5 <- lm(Concrete.compressive.strength ~ Superplasticizer, data = data)
model6 <- lm(Concrete.compressive.strength ~ Coarse.Aggregate, data = data)
model7 <- lm(Concrete.compressive.strength ~ Fine.Aggregate, data = data)
model8 <- lm(Concrete.compressive.strength ~ Age..day., data = data)

# model1 <- ols_regress(Concrete.compressive.strength ~ Cement, data = data)
# kurtosis_value <- model1$diagnostics$residual_kurtosis
ols_regress(model1)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")
# print(kurtosis_value)

ols_regress(model2)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")

ols_regress(model3)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")


ols_regress(model4)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")

ols_regress(model5)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")

ols_regress(model6)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")

ols_regress(model7)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")

ols_regress(model8)
print("Standard Errors assume that the covariance matrix of the errors is correctly specified.")

print("Looking at the p-value of the t-statistic: all variables have a strong association with 
      compressive strength where fly ash has the weakest by 0.001.")

# Question 5: How accurately can this model predict compressive strength?
  
# The accuracy depends on what type of prediction:
# Individual response (Y = f(X) + ep), the prediction interval is used
# Average response (f(X)), the confidence interval is used
# Prediction intervals are wider than confidence intervals because the account for 
# the uncertainity associated with the irreducible error (ep).

# Question 6: Is the relationship linear?

# Non-linearity can be determined from residual vs. predicted 
# value plot for each variable (top right plots below). 
# When linearity exists, there should be no clear pattern. 
# The residual plot with the most non-linear form is for age where for ages 0 to 20, 
# there are negative residuals then the residuals increase from 20 to 100 before 
# decreasing again. Water and fine aggregate have slight non-linear patterns. 
# Transformations of the predictors (e.g., sqrt(X), X^2) could accomodate the nonlinearities.

ols_plot_resid_fit(model1) # Cement

ols_plot_resid_fit(model2) # Blast.Furnace.Slag

ols_plot_resid_fit(model3) # Fly.Ash

ols_plot_resid_fit(model4) # Water

ols_plot_resid_fit(model5) # Superplasticizer

ols_plot_resid_fit(model6) # Coarse.Aggregate

ols_plot_resid_fit(model7) # Fine.Aggregate

ols_plot_resid_fit(model8) # Age..day.

# Let's conduct Breusch-Pagan test to clear out the constance variance assumption

bptest(model1, varformula = ~ Cement, data = data, studentize = FALSE) # Cement

bptest(model2, varformula = ~ Blast.Furnace.Slag, data = data, studentize = FALSE) # Blast.Furnace.Slag

bptest(model3, varformula = ~ Fly.Ash, data = data, studentize = FALSE) # Fly.Ash

bptest(model4, varformula = ~ Water, data = data, studentize = FALSE) # Water

bptest(model5, varformula = ~ Superplasticizer, data = data, studentize = FALSE) # Superplasticizer

bptest(model6, varformula = ~ Coarse.Aggregate, data = data, studentize = FALSE) # Coarse.Aggregate

bptest(model7, varformula = ~ Fine.Aggregate, data = data, studentize = FALSE) # Fine.Aggregate

bptest(model8, varformula = ~ Age..day., data = data, studentize = FALSE) # Age..day.

# Calculate the critical value of the Chi-square statistic
alpha <- 0.05  
df <- 1   
chi_square_critical <- qchisq(1 - alpha, df)
chi_square_critical

# Under Breusch-Pagan test, non constance variance is valid for all all features except Coarse.Aggregate and Age..day.

# Step 6: Feature Engineering with OLS

# Question 7: Is there synergy among the predictors?

# To answer this, an interaction term needs to be created that accomodates non-additive 
# relationships and the R-squared value should increase with this inclusion. Below I have 
# created an interaction term for water and cement (water : cement ratio) and ran an OLS 
# analysis again. With this interaction term, the R-squared term increased from 0.616 to 0.618. 
# Since adding variables increases R-squared automatically and an increase of 0.002 is not large, 
# it's better to look at the adj. R-squared which penalizes additional predictors. This value 
# went from 0.613 to 0.615 indicating synergy exists between these predictors. Similarly, AIC and 
# BIC metrics penalize models with more predictors, but AIC decreased with the addition of 
# water : cement ratio (from 7756 to 7750) and SBC remained the same (7807) further demonstrating 
# that the addition of the predictor is justified. The best method for judgement of the inclusion of 
# this variable is through estimating the test set results through cross-validation, but given the 
# nonlinearity of compressive strength to the predictors, linear regression isn't worth wasting 
# further time on as other non-linear models will produce better predictive results. For inference 
# purposes, adj.-R-squared, SBC, and AIC suffice.

# Other interaction terms I tried are cement : fineaggregate, cement : coarseaggregate, 
# cement : fineaggregate : coarseaggregate, and superplasticizer : cement. None of these 
# increased adj. R-squared and their t-statistic p-value were > 0.05.

data$water_cement_ratio <- data$Water/data$Cement
head(data)

# plot water:cement ratio against compressive strength
var <- "water_cement_ratio"
response_var <- "Concrete.compressive.strength"
f <- ggplot(data, aes_string(x = var, y = response_var)) +
  geom_point(alpha = 0.6, color = "green") +
  labs(title = paste("Relationship between", var, "and", response_var),
       x = var, y = response_var) +
  theme_minimal()
print(f)

# generate OLS regression results with water : cement ratio
mdel <- lm(Concrete.compressive.strength ~ ., data = data)
ols_regress(mdel) #sig. in output table represent p-value

# generate OLS summary with only water : cement ratio
mdel1 <- lm(Concrete.compressive.strength ~ water_cement_ratio, data = data)
ols_regress(mdel1)
# Looking at the p-value of the t-statistic: water : cement ratio has a strong association with compressive strength.

# Non-linearity can be determined from residual vs. predicted 
# value plot for water_cement_ratio variable
ols_plot_resid_fit(mdel1)
# Water : cement ratio residuals exhibits a near linear relationship.

# Let's conduct Breusch-Pagan test to clear out the constance variance assumption
bptest(mdel1, varformula = ~ water_cement_ratio, data = data, studentize = FALSE)
# Under Breusch-Pagan test non constant variance assumption is valid for water_cement_ratio variable 


