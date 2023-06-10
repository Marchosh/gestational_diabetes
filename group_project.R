#a
data <- read.csv("C:/Users/maria/OneDrive - MNSCU/Desktop/CIS 690/patients.csv")
head(data)

#b
summary(data)

#c
  #histogram of Glucose
library(ggplot2)
ggplot(data, aes(x = Glucose)) + geom_histogram(binwidth = 10, fill = "skyblue") + labs(x = "Glucose", y = "Count") + ggtitle("Distribution of Glucose")

  #Boxplot of BMI by Diagnosis
ggplot(data, aes(x = Diagnosis, y = BMI, group = Diagnosis)) + geom_boxplot(fill = "lightgreen") + labs(x = "Diagnosis", y = "BMI") + ggtitle("BMI by Diagnosis")

  #Scatterplot of BloodPressure and Age
ggplot(data, aes(x = Age, y = BloodPressure)) + geom_point(color = "blue", alpha = 0.6) + labs(x = "Age", y = "Blood Pressure") + ggtitle("Blood Pressure by Age")

  #Barplot of Pregnancies
ggplot(data, aes(x = Pregnancies)) + geom_bar(fill = "lightpink") + labs(x = "Pregnancies", y = "Count") +  ggtitle("Distribution of Pregnancies")

  #Density Plot of Insulin by Diagnosis
ggplot(data, aes(x = Insulin, fill = Diagnosis)) + geom_density(alpha = 0.5) + labs(x = "Insulin", y = "Density") + ggtitle("Insulin Distribution by Diagnosis")


#d
  #Missing values with media
# Find missing values (coded as 0) for each independent variable
missing_vars <- colnames(data)[apply(data == 0, 2, any) & colnames(data) != "Diagnosis"]

# Replace missing values with median values
for (var in missing_vars) {
  data[data[, var] == 0, var] <- median(data[data[, var] != 0, var], na.rm = TRUE)
}

head(data)


#e
  #Finding outliers
outliers <- sapply(data, function(x){
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[1] + 1.5 * iqr
  x < lower_bound | x > upper_bound
})
outliers

#f
  #using winsorization
replace_outliers <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  x
}

data_cleaned <- data
data_cleaned[] <- lapply(data_cleaned, replace_outliers)
head(data_cleaned)

#g
  #correlation plot
library(corrplot)
cor_matrix <- cor(data[, -ncol(data_cleaned)], use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color")

#h
library(caret)

# Specify the pre-processing method
preProcessMethod <- preProcess(data[, -which(colnames(data_cleaned) == "Diagnosis")], method = c("center", "scale"))

# Apply the pre-processing method to standardize the independent variables
standardized_data <- predict(preProcessMethod, newdata = data[, -which(colnames(data_cleaned) == "Diagnosis")])
standardized_data <- as.data.frame(standardized_data)

# Add the Diagnosis variable back to the standardized data
standardized_data$Diagnosis <- data_cleaned$Diagnosis

# View the standardized data
head(standardized_data)

#i
library(caret)
library(ROSE)
library(dplyr)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(standardized_data$Diagnosis, p = 0.7, list = FALSE)
train_data <- standardized_data[train_index, ]
test_data <- standardized_data[-train_index, ]

# Create logistic regression model using best features
best_features <- c("Glucose", "BMI", "Age")
train_data$Diagnosis <- as.factor(train_data$Diagnosis)
test_data$Diagnosis <- as.factor(test_data$Diagnosis)

# Create logistic regression model using best features
LRM1 <- train(Diagnosis ~ Glucose + BMI + Age, data = train_data, method = "glm", family = binomial())
summary(LRM1)

# Make predictions on test data
predictions_LRM1 <- predict(LRM1, newdata = test_data, type = "raw")

# Create classification report
class_report_LRM1 <- caret::confusionMatrix(predictions_LRM1, test_data$Diagnosis)
print(class_report_LRM1)

#k
# Create confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$Diagnosis)

# Calculate precision, recall, F1 score, and support
precision <- confusion_matrix$byClass['Pos Pred Value']
recall <- confusion_matrix$byClass['Sensitivity']
f1_score <- confusion_matrix$byClass['F1']
support <- ifelse(is.na(confusion_matrix$byClass['Support']), 0, confusion_matrix$byClass['Support'])

# Print the classification report
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
cat("Support:", support, "\n")

#l
  #accuracy score
# Calculate accuracy score for LRM1
accuracy_LRM1 <- sum(predictions_LRM1 == test_data$Diagnosis) / length(predictions_LRM1)
print(paste("Accuracy Score (LRM1):", accuracy_LRM1))

#m
  #create another model

# Create logistic regression model using all independent features
LRM2 <- glm(Diagnosis ~ ., data = train_data, family = binomial())
summary(LRM2)

# Oversample the minority class to balance the dataset
oversampled_data <- ROSE(Diagnosis ~ ., data = train_data, seed = 123)$data

# Create logistic regression model on the oversampled data
LRM2 <- glm(Diagnosis ~ ., data = oversampled_data, family = binomial())
summary(LRM2)

# Make predictions on test data
predictions_LRM2 <- predict(LRM2, newdata = test_data, type = "response")

# Convert predictions to factors
predictions_LRM2 <- as.factor(ifelse(predictions_LRM2 > 0.5, "1", "0"))

# Create classification report for LRM2
class_report_LRM2 <- caret::confusionMatrix(predictions_LRM2, test_data$Diagnosis)
print(class_report_LRM2)

# Calculate accuracy score for LRM2
accuracy_LRM2 <- sum(predictions_LRM2 == test_data$Diagnosis) / length(predictions_LRM2)
print(paste("Accuracy Score (LRM2):", accuracy_LRM2))

# Create confusion matrix
confusion_matrix_2 <- confusionMatrix(predictions, test_data$Diagnosis)

# Calculate precision, recall, F1 score, and support
precision_2 <- confusion_matrix_2$byClass['Pos Pred Value']
recall_2 <- confusion_matrix_2$byClass['Sensitivity']
f1_score_2 <- confusion_matrix_2$byClass['F1']
support_2 <- ifelse(is.na(confusion_matrix_2$byClass['Support']), 0, confusion_matrix_2$byClass['Support'])

# Print the classification report
cat("Precision:", precision_2, "\n")
cat("Recall:", recall_2, "\n")
cat("F1 Score:", f1_score_2, "\n")
cat("Support:", support_2, "\n")

#n
# Compare accuracy scores 
if (accuracy_LRM1 > accuracy_LRM2) { 
  cat("LRM1 is a better model based on accuracy.\n") 
} else { 
  cat("LRM2 is a better model based on accuracy.\n") 
} 

#o
# Assuming you have already fitted a logistic regression model called 'model'

# View the coefficient estimates
coef_summary <- summary(LRM1)$coefficients
print(coef_summary)

# Access the coefficient values
coefficients <- coef_summary[, "Estimate"]

# Access the p-values
p_values <- coef_summary[, "Pr(>|z|)"]

# Loop through the coefficients and interpret the results
for (i in 1:length(coefficients)) {
  coefficient <- coefficients[i]
  p_value <- p_values[i]
  
  if (p_value < 0.05) {
    significance <- "significant"
  } else {
    significance <- "not significant"
  }
  
  if (coefficient > 0) {
    direction <- "positive"
  } else if (coefficient < 0) {
    direction <- "negative"
  } else {
    direction <- "no"
  }
  
  predictor <- names(coefficients)[i + 1]  # Skip the intercept term
  
  cat(paste("The coefficient for", predictor, "is", coefficient,
            "indicating a", direction, "relationship, and is", significance, "\n"))
}

#p
# Convert 'Diagnosis' variable to numeric
train_data$Diagnosis <- as.numeric(train_data$Diagnosis)

# Check for missing values
sum(is.na(train_data))

# Fit Model 0 (Null Model)
model0 <- lm(Diagnosis ~ Glucose, data = train_data)

# Fit Model 1 (Current Model)
model1 <- lm(Diagnosis ~ Glucose + BMI + Age, data = train_data)

# Perform the likelihood ratio test
likelihood_ratio <- 2 * (logLik(model1) - logLik(model0))

# Calculate the degrees of freedom
df <- df.residual(model0) - df.residual(model1)

# Calculate the p-value
p_value <- 1 - pchisq(likelihood_ratio, df)

# Interpret the hypothesis test
if (is.na(p_value)) {
  interpretation <- "Unable to perform the hypothesis test. Check for missing values or other issues."
} else if (p_value < 0.05) {
  interpretation <- "Reject the null hypothesis. Model 1 is significantly better than Model 0."
} else {
  interpretation <- "Fail to reject the null hypothesis. Model 1 is not significantly better than Model 0."
}

# Print the results
cat("Test Statistic:", likelihood_ratio, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")
cat("Interpretation:", interpretation, "\n")


#q
# Original significance level
alpha <- 0.05

# Number of hypothesis tests
num_tests <- 1

# Bonferroni-corrected significance level
adjusted_alpha <- alpha / num_tests

# Perform hypothesis test using adjusted significance level
if (p_value < adjusted_alpha) {
  interpretation <- "Reject the null hypothesis. Model 1 is significantly better than Model 0."
} else {
  interpretation <- "Fail to reject the null hypothesis. Model 1 is not significantly better than Model 0."
}

# Print the adjusted significance level and interpretation
cat("Adjusted Significance Level:", adjusted_alpha, "\n")
cat("Interpretation:", interpretation, "\n")
