# Spencer Connolly
# CIS 690
# Group Assignment

require(ggplot2)
require(dplyr)
require(tidyverse)
require(vioplot)
require(corrplot)

# <<A>> 
# Load the dataset in R Studio
setwd("C:/Users/gadge/OneDrive/Documents/CIS690/Group Project")
patients <- read.csv('patients.csv')
patients

# Examine the first few rows of data using R. Explain your findings
head(patients)
# There are nine attributes as described by the group project pdf document.
# The attributes and their types are listed below:
#   Pregnancies     Integer
#   Glucose         Integer
#   BloodPressure   Integer
#   SkinThickness   Integer
#   Insulin         Integer
#   BMI             Double    One decimal place
#   Predigree       Double    Three decimal places
#   Age             Integer
#   Diagnosis       Integer   Binary

# Did you notice anything abnormal or interesting?
# Rows 3 and 6 contain a value of 0 in SkinThickness, which seems a little bogus.
# According to Google, a glucose level of 183 (row 3) would indicate a condition known as hyperglycemia. 
#   This strikes me a strong potential indicator of gestational diabetes.
# Pedigree calculates someone's predisposition for diabetes, which is essentially the likelihood (probability) that they develop diabetes.
#   In row 5, this value is 2.288. This seems to likely be an error in the data collection process.

# <<B>>
# Provide summary statistics. Calculate the mean, median, standard deviation, and quartiles for each independent variable. 
# Explain your results.
summary(patients)

# <<C>>
# Using the ggplot2 library, create any five visualizations.
# Explain your reasoning for selecting those visualizations.
# Explain the output of each visualization.
# What are the insights your visualizations reveal about the dataset?

patients %>% pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Value") +
  ggtitle("Multiple Column Visualization")

# Scatter Plots
# X = Index vs. Y = Variable
ggplot(data = patients, aes(x = 1:768, y = Pregnancies)) +    geom_point(color = 'red', size = 1.5) + labs(x = 'Index')
ggplot(data = patients, aes(x = 1:768, y = Glucose)) +        geom_point(color = 'orange', size = 1.5) + labs(x = 'Index')
ggplot(data = patients, aes(x = 1:768, y = BloodPressure)) +  geom_point(color = 'gold', size = 1.5) + labs(x = 'Index')
ggplot(data = patients, aes(x = 1:768, y = SkinThickness)) +  geom_point(color = 'green', size = 1.5) + labs(x = 'Index')
ggplot(data = patients, aes(x = 1:768, y = Insulin)) +        geom_point(color = 'cyan', size = 1.5) + labs(x = 'Index')
ggplot(data = patients, aes(x = 1:768, y = BMI)) +            geom_point(color = 'blue', size = 1.5) + labs(x = 'Index')
ggplot(data = patients, aes(x = 1:768, y = Pedigree)) +       geom_point(color = 'purple', size = 1.5) + labs(x = 'Index')
ggplot(data = patients, aes(x = 1:768, y = Age)) +            geom_point(color = 'magenta', size = 1.5) + labs(x = 'Index')

# Histograms
ggplot(data = patients, aes(x = Pregnancies)) +     geom_histogram(binwidth = 0.5, fill = 'red', color = 'white') + labs(x = 'Index')
ggplot(data = patients, aes(x = Glucose)) +         geom_histogram(binwidth = 5, fill = 'orange', color = 'white') + labs(x = 'Index')
ggplot(data = patients, aes(x = BloodPressure)) +   geom_histogram(binwidth = 2.5, fill = 'gold', color = 'white') + labs(x = 'Index')
ggplot(data = patients, aes(x = SkinThickness)) +   geom_histogram(binwidth = 2.5, fill = 'green', color = 'white') + labs(x = 'Index')
ggplot(data = patients, aes(x = Insulin)) +         geom_histogram(binwidth = 20, fill = 'cyan', color = 'white') + labs(x = 'Index')
ggplot(data = patients, aes(x = BMI)) +             geom_histogram(binwidth = 2, fill = 'blue', color = 'white') + labs(x = 'Index')
ggplot(data = patients, aes(x = Pedigree)) +        geom_histogram(binwidth = 0.1, fill = 'purple', color = 'white') + labs(x = 'Index')
ggplot(data = patients, aes(x = Age)) +             geom_histogram(binwidth = 2, fill = 'magenta', color = 'white') + labs(x = 'Index')

# Box Plots
ggplot(data = patients, aes(y = Pregnancies)) +   geom_boxplot(color = 'red')
ggplot(data = patients, aes(y = Glucose)) +       geom_boxplot(color = 'orange')
ggplot(data = patients, aes(y = BloodPressure)) + geom_boxplot(color = 'gold')
ggplot(data = patients, aes(y = SkinThickness)) + geom_boxplot(color = 'green')
ggplot(data = patients, aes(y = Insulin)) +       geom_boxplot(color = 'cyan')
ggplot(data = patients, aes(y = BMI)) +           geom_boxplot(color = 'blue')
ggplot(data = patients, aes(y = Pedigree)) +      geom_boxplot(color = 'purple')
ggplot(data = patients, aes(y = Age)) +           geom_boxplot(color = 'pink')

# Bar Graphs
ggplot(data = patients, aes(y = Pregnancies)) +   geom_bar(fill = 'red', color = 'white')
ggplot(data = patients, aes(y = Glucose)) +       geom_bar(fill = 'orange', color = 'white')
ggplot(data = patients, aes(y = BloodPressure)) + geom_bar(fill = 'gold', color = 'white')
ggplot(data = patients, aes(y = SkinThickness)) + geom_bar(fill = 'green', color = 'white')
ggplot(data = patients, aes(y = Insulin)) +       geom_bar(fill = 'cyan', color = 'white')
ggplot(data = patients, aes(y = BMI)) +           geom_bar(fill = 'blue', color = 'white')
ggplot(data = patients, aes(y = Pedigree)) +      geom_bar(fill = 'purple', color = 'white')
ggplot(data = patients, aes(y = Age)) +           geom_bar(fill = 'magenta', color = 'white')

# Violin Plots
ggplot(data = patients, aes(y = Pregnancies, x = 0)) +   geom_violin(color = 'red')
ggplot(data = patients, aes(y = Glucose, x = 0)) +       geom_violin(color = 'orange')
ggplot(data = patients, aes(y = BloodPressure, x = 0)) + geom_violin(color = 'gold')
ggplot(data = patients, aes(y = SkinThickness, x = 0)) + geom_violin(color = 'green')
ggplot(data = patients, aes(y = Insulin, x = 0)) +       geom_violin(color = 'cyan')
ggplot(data = patients, aes(y = BMI, x = 0)) +           geom_violin(color = 'blue')
ggplot(data = patients, aes(y = Pedigree, x = 0)) +      geom_violin(color = 'purple')
ggplot(data = patients, aes(y = Age, x = 0)) +           geom_violin(color = 'pink')

# <<D>>
# Find missing values for each independent variable and fill them with median values.
# The missing values for independent variables in the dataset are coded 0.
summary(patients)
patients$Pregnancies[patients$Pregnancies == 0] <- median(patients$Pregnancies)
patients$Glucose[patients$Glucose == 0] <- median(patients$Glucose)
patients$BloodPressure[patients$BloodPressure == 0] <- median(patients$BloodPressure)
patients$SkinThickness[patients$SkinThickness == 0] <- median(patients$SkinThickness)
patients$Insulin[patients$Insulin == 0] <- median(patients$Insulin)
patients$BMI[patients$BMI == 0] <- median(patients$BMI)
patients$Pedigree[patients$Pedigree == 0] <- median(patients$Pedigree)
patients$Age[patients$Age == 0] <- median(patients$Age)

# <<E>>
# Find outliers for each independent variable using the IQR rule.
summary(patients)
patients$Pregnancies[patients$Pregnancies > quantile(patients$Pregnancies, probs = 0.25) & quantile(patients$Pregnancies, probs = 0.75)]
patients$Pregnancies[patients$Glucose > quantile(patients$Glucose, probs = 0.25) & quantile(patients$Glucose, probs = 0.75)]
patients$Pregnancies[patients$BloodPressure > quantile(patients$BloodPressure, probs = 0.25) & quantile(patients$BloodPressure, probs = 0.75)]
patients$Pregnancies[patients$SkinThickness > quantile(patients$SkinThickness, probs = 0.25) & quantile(patients$SkinThickness, probs = 0.75)]
patients$Pregnancies[patients$Insulin > quantile(patients$Insulin, probs = 0.25) & quantile(patients$Insulin, probs = 0.75)]
patients$Pregnancies[patients$BMI > quantile(patients$BMI, probs = 0.25) & quantile(patients$BMI, probs = 0.75)]
patients$Pregnancies[patients$Pedigree > quantile(patients$Pedigree, probs = 0.25) & quantile(patients$Pedigree, probs = 0.75)]
patients$Pregnancies[patients$Age > quantile(patients$Age, probs = 0.25) & quantile(patients$Age, probs = 0.75)]

# <<F>>
# Replace outliers. 
# Explain your approach.
patients$Pregnancies[patients$Pregnancies > quantile(patients$Pregnancies, probs = 0.25) & quantile(patients$Pregnancies, probs = 0.75)]
patients$Pregnancies[patients$Glucose > quantile(patients$Glucose, probs = 0.25) & quantile(patients$Glucose, probs = 0.75)]
patients$Pregnancies[patients$BloodPressure > quantile(patients$BloodPressure, probs = 0.25) & quantile(patients$BloodPressure, probs = 0.75)]
patients$Pregnancies[patients$SkinThickness > quantile(patients$SkinThickness, probs = 0.25) & quantile(patients$SkinThickness, probs = 0.75)]
patients$Pregnancies[patients$Insulin > quantile(patients$Insulin, probs = 0.25) & quantile(patients$Insulin, probs = 0.75)]
patients$Pregnancies[patients$BMI > quantile(patients$BMI, probs = 0.25) & quantile(patients$BMI, probs = 0.75)]
patients$Pregnancies[patients$Pedigree > quantile(patients$Pedigree, probs = 0.25) & quantile(patients$Pedigree, probs = 0.75)]
patients$Pregnancies[patients$Age > quantile(patients$Age, probs = 0.25) & quantile(patients$Age, probs = 0.75)]

# <<G>>
# Find the best performing variables/features using a correlogram.

# We'll be removing SkinThickness and Insulin because they have a lot of missing values (more than half of observations)
# They also have very little predictive power on the dependent variable, Diagnosis

cor_matrix <- cor(patients)
corrplot(cor_matrix, type = 'upper', method = 'color', tol.cex = 0.7)

ggplot(data = patients, aes(x = Pregnancies, y = Age)) + geom_point(color = 'red', size = 1.5)
ggplot(data = patients, aes(x = Glucose, y = Insulin)) + geom_point(color = 'orange', size = 1.5)
ggplot(data = patients, aes(x = BloodPressure, y = Age)) + geom_point(color = 'gold', size = 1.5)
ggplot(data = patients, aes(x = SkinThickness, y = BMI)) + geom_point(color = 'green', size = 1.5)

# <<H>>
# Standardize your features to Gaussian distribution. 
# Explain why it would be a good idea to standardize the features to Gaussian distribution.
zscore <- function(x) {
  (x - mean(x)) / sd(x)
}
standardized.patients <- as.data.frame(lapply(patients, zscore))

# <<I>>
# Create a logistic regression model (call it LRM1) using your best features. 
# Describe your model.