library(ggplot2)
library(dplyr)
library(tidyr)
library(tree)
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)

setwd('/Users/ananya/Desktop/DA_FALL23_ANANYA_UPADHYAY')
wdi.data<-read.csv('Cleaned WDI Data.csv')
climate.pcpt.data<-read.csv('climatePcpt.csv')
potato<-read.csv('yield_potato.csv')
rice<-read.csv('yield_rice.csv')
wheat<-read.csv('yield_wheat.csv')
data1<-left_join(wdi.data,climate.pcpt.data,by=c('Country_Code','Country_Name','Year'))
data2<-left_join(potato,rice,by=c('Country_Name','Year'))
data2<-left_join(data2,wheat,by=c('Country_Name','Year')) 
data<-left_join(data1,data2,by=c('Country_Name','Year')) 
View(data)
#write.csv(data,'Crop_Production_WDI_merged.csv')
data$Country_Name<-factor(data$Country_Name)
data$Country_Code<-factor(data$Country_Code)
data=filter(data,data$Country_Name!="Maldives")
data=filter(data,data$Year!='1960')
attach(data)
str(data)
summary(data)

par(mfrow=c(3, 1))
hist(data$Yield_Potato, main = "Histogram of Potato yield", col = "orange", breaks = 30)
hist(data$Yield_Rice, main = "Histogram of Rice yield", col = "blue", breaks = 30)
hist(data$Yield_Wheat, main= "Histogram of Wheat Yield", col="red",breaks=30)


par(mfrow=c(4, 3))
hist(data$Year, main = "Histogram of Year", col = "pink", breaks = 30)
hist(data$Agricultural.machinery..tractors, main = "Histogram of Agricultural Machinery (Tractors)", col = "blue", breaks = 30)
hist(data$Agricultural.land....of.land.area., main = "Histogram of Agricultural Land (% of Land Area)", col = "green", breaks = 30)
hist(data$Arable.land....of.land.area., main = "Histogram of Arable Land (% of Land Area)", col = "orange", breaks = 30)
hist(data$Fertilizer.consumption..kilograms.per.hectare.of.arable.land., main = "Histogram of Fertilizer Consumption (kg/ha of Arable Land)", col = "purple", breaks = 30)
hist(data$CO2.emissions..kt., main = "Histogram of CO2 Emissions (kt)", col = "red", breaks = 30)
hist(data$Inflation..consumer.prices..annual..., main = "Histogram of Inflation (Consumer Prices, Annual)", col = "cyan", breaks = 30)
hist(data$GDP.growth..annual..., main = "Histogram of GDP Growth (Annual)", col = "yellow", breaks = 30)
hist(data$Climate.deg.C., main = "Histogram of Climate (Degrees Celsius)", col = "brown", breaks = 30)
hist(data$Precipitation.mm., main = "Histogram of Precipitation (mm)", col = "gray", breaks = 30)

par(mfrow=c(3, 3))
ggplot(data, aes(x = Year, y = Agricultural.land....of.land.area.
                 , group = Country_Name, color = Country_Name)) +
  geom_line() +
  xlab('Year') +
  ylab('Agricultural Land (% of Land Area)') +
  ggtitle('Agricultural Land Over the Years by Country') +
  theme_minimal()
    
ggplot(data, aes(x = Year, y = Arable.land....of.land.area.
                 , group = Country_Name, color = Country_Name)) +
  geom_line() +
  xlab('Year') +
  ylab('Arable Land (% of Land Area)') +
  ggtitle('Arable Land Over the Years by Country') +
  theme_minimal()


ggplot(data, aes(x = Year, y = Fertilizer.consumption..kilograms.per.hectare.of.arable.land.
                 , group = Country_Name, color = Country_Name)) +
  geom_line() +
  xlab('Year') +
  ylab('Fertilizer Consumption kg per hectare of arable land') +
  ggtitle('Fertilizer Consumption Over the Years by Country') +
  theme_minimal()

ggplot(data, aes(x = Year, y = CO2.emissions..kt.
                 , group = Country_Name, color = Country_Name)) +
  geom_line() +
  xlab('Year') +
  ylab('CO2 Emissions') +
  ggtitle('CO2 Emissions Over the Years by Country') +
  theme_minimal()

ggplot(data , aes(x = Year, y = GDP.growth..annual...
                 , group = Country_Name, color = Country_Name)) +
  geom_line() +
  xlab('Year') +
  ylab('Annual GDP growth') +
  ggtitle('GDP Growth(ANNUAL) Over the Years by Country') +
  theme_minimal()

ggplot(data, aes(x = Year, y = Climate.deg.C.
                 , group = Country_Name, color = Country_Name)) +
  geom_line() +
  xlab('Year') +
  ylab('Temperature in deg C') +
  ggtitle('Temperature(ANNUAL) Over the Years by Country') +
  theme_minimal()


ggplot(data, aes(x = Year, y = Precipitation.mm.
                 , group = Country_Name, color = Country_Name)) +
  geom_line() +
  xlab('Year') +
  ylab('Precipitation(in mm)') +
  ggtitle('Precipitation (ANNUAL) Over the Years by Country') +
  theme_minimal()

# Cleaning Yields --> Interpolation.
View(cleaned.data)
cleaned.data<-data

library(zoo)
zoo_data <- zoo(cleaned.data$Yield_Rice)
interpolated_data <- na.approx(zoo_data)
cleaned.data$Yield_Rice[is.na(cleaned.data$Yield_Rice)] <- as.numeric(interpolated_data)
View(cleaned.data)

zoo_data <- zoo(cleaned.data$Yield_Potato)
interpolated_data <- na.approx(zoo_data)
cleaned.data$Yield_Potato[is.na(cleaned.data$Yield_Potato)] <- as.numeric(interpolated_data)

zoo_data <- zoo(cleaned.data$Yield_Wheat)
interpolated_data <- na.approx(zoo_data)
cleaned.data$Yield_Wheat[is.na(cleaned.data$Yield_Wheat)] <- as.numeric(interpolated_data)


#write.csv(cleaned.data,"Interpolated Crops.csv")
pdf('outliers.pdf')
numeric_vars <- sapply(cleaned.data, is.numeric)
numeric_data <- cleaned.data[, numeric_vars]
for (var in names(numeric_data)) {
  # Calculate IQR
  Q1 <- quantile(numeric_data[[var]], 0.25)
  Q3 <- quantile(numeric_data[[var]], 0.75)
  IQR <- Q3 - Q1
  
  # Define the upper and lower bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identify outliers
  outliers <- numeric_data[[var]][numeric_data[[var]] < lower_bound | numeric_data[[var]] > upper_bound]
  
  # Print the number of outliers
  cat("Number of outliers in", var, ":", length(outliers), "\n")
  
  # Create a boxplot before removing outliers
  boxplot(numeric_data[[var]], main = paste("Boxplot of", var, "(Before Removing Outliers)"))
  
  # Remove outliers from the data
  cleaned.data <- cleaned.data[!(cleaned.data[[var]] %in% outliers), ]
  
  # Create a boxplot after removing outliers
  boxplot(cleaned.data[[var]], main = paste("Boxplot of", var, "(After Removing Outliers)"))
}
dev.off()

#significance test
variables <- c("Year",
               "Agricultural.land....of.land.area.",
               "Agricultural.machinery..tractors",
               "Arable.land....of.land.area.",
               "Fertilizer.consumption..kilograms.per.hectare.of.arable.land.",
               "CO2.emissions..kt.",
               "Inflation..consumer.prices..annual...",
               "GDP.growth..annual...",
               "Climate.deg.C.",
               "Precipitation.mm.")
#RICE
significance <- sapply(variables, function(var) {
  f_test <- summary(aov(cleaned.data$Yield_Rice ~ cleaned.data[[var]]))
  p_value <- f_test[[1]]$"Pr(>F)"[1]
  return(p_value)
})


alpha <- 0.05

# Check significance and print result
for (i in seq_along(variables)) {
  if (significance[i] < alpha) {
    cat(variables[i], "is significant (p-value =", significance[i], ")\n")
  } else {
    cat(variables[i], "is not significant (p-value =", significance[i], ")\n")
  }
}

#POTATO
significance <- sapply(variables, function(var) {
  f_test <- summary(aov(cleaned.data$Yield_Potato ~ cleaned.data[[var]]))
  p_value <- f_test[[1]]$"Pr(>F)"[1]
  return(p_value)
})

for (i in seq_along(variables)) {
  if (significance[i] < alpha) {
    cat(variables[i], "is significant (p-value =", significance[i], ")\n")
  } else {
    cat(variables[i], "is not significant (p-value =", significance[i], ")\n")
  }
}

#WHEAT
significance <- sapply(variables, function(var) {
  f_test <- summary(aov(cleaned.data$Yield_Wheat ~ cleaned.data[[var]]))
  p_value <- f_test[[1]]$"Pr(>F)"[1]
  return(p_value)
})

for (i in seq_along(variables)) {
  if (significance[i] < alpha) {
    cat(variables[i], "is significant (p-value =", significance[i], ")\n")
  } else {
    cat(variables[i], "is not significant (p-value =", significance[i], ")\n")
  }
}

#Contingency Table
contingency_table <- table(cleaned.data$Country_Name, cleaned.data$Country_Code)
print(contingency_table)


selected_columns <- cleaned.data[, c("Yield_Rice", "Yield_Potato", "Yield_Wheat",
                                     "Agricultural.land....of.land.area.",
                                     "Agricultural.machinery..tractors",
                                     "Fertilizer.consumption..kilograms.per.hectare.of.arable.land.",
                                     "CO2.emissions..kt.",
                                     "GDP.growth..annual...",
                                     "Inflation..consumer.prices..annual...",
                                     "Climate.deg.C.",
                                     "Precipitation.mm.")]

# Compute the correlation matrix
correlation_matrix <- cor(selected_columns)

# Melt the correlation matrix for plotting
melted_correlation <- as.data.frame(as.table(correlation_matrix))
names(melted_correlation) <- c("Var1", "Var2", "Correlation")


ggplot(melted_correlation, aes(Var2, Var1, fill = Correlation, label = round(Correlation, 2))) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Correlation != 0, round(Correlation, 2), "")), vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()


set.seed(123)  
split <- sample.split(cleaned.data$Yield_Rice, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

#<-------------------------------------------LM Model---------------------------------------------------->
# Build the multiple linear regression model for
#Rice  
rice.model <- lm(Yield_Rice ~ Country_Name + Country_Code +
              Year + 
              Agricultural.land....of.land.area.  +
              Agricultural.machinery..tractors +
              Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
              CO2.emissions..kt. +
              GDP.growth..annual... + 
              Climate.deg.C. + Precipitation.mm.
, data = train_data)

rice.predictions <- predict(rice.model, newdata = test_data)

mse <- sqrt(mean((rice.predictions - test_data$Yield_Rice)^2))
cat("Root Mean Squared Error:", mse, "\n")

rsquared <- 1 - sum((test_data$Yield_Rice - rice.predictions)^2) / sum((test_data$Yield_Rice - mean(test_data$Yield_Rice))^2)
cat("R-squared:", rsquared, "\n")



#POTATAO
set.seed(123)  
split <- sample.split(cleaned.data$Yield_Potato, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

potato.model <- lm(Yield_Potato ~ Country_Name + Country_Code +
              Year + 
              Agricultural.land....of.land.area.  +
              Agricultural.machinery..tractors +
              Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
              CO2.emissions..kt. +
              GDP.growth..annual... + 
              Climate.deg.C. + Precipitation.mm.
            , data = train_data)


potato.predictions <- predict(potato.model, newdata = test_data)

mse <- sqrt(mean((potato.predictions - test_data$Yield_Potato)^2))
cat("Root Mean Squared Error:", mse, "\n")
# Calculate R-squared on the test set
rsquared <- 1 - sum((test_data$Yield_Potato - potato.predictions)^2) / sum((test_data$Yield_Potato - mean(test_data$Yield_Potato))^2)
cat("R-squared:", rsquared, "\n")



#WHEAT
#filtered.data<-filter(data,Country_Code!="LKA")
filter.data<-cleaned.data

set.seed(123)  
split <- sample.split(filtered.data$Yield_Wheat, SplitRatio = 0.7)
train_data <- subset(filtered.data, split == TRUE)
test_data <- subset(filtered.data, split == FALSE)

wheat.model <- lm(Yield_Wheat ~ Country_Name + Country_Code +
              Year + 
              Agricultural.land....of.land.area.  +
              Agricultural.machinery..tractors +
              Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
              CO2.emissions..kt. +
              GDP.growth..annual... + 
              Climate.deg.C. + Precipitation.mm.
            , data = train_data)

wheat.predictions <- predict(wheat.model, newdata = test_data)

mse <- sqrt(mean((wheat.predictions - test_data$Yield_Wheat)^2))
cat("Root Mean Squared Error:", mse, "\n")

rsquared <- 1 - sum((test_data$Yield_Wheat - wheat.predictions)^2) / sum((test_data$Yield_Wheat - mean(test_data$Yield_Wheat))^2)
cat("R-squared:", rsquared, "\n")




#par(mfrow=c(1, 1))
#<--------------------------------------- Decision tree model--------------------------------------------------->
#RICE
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Rice, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

rice.tree <- rpart(Yield_Rice ~ Country_Name + Country_Code +
                     Year +
                     Agricultural.land....of.land.area. +
                     Agricultural.machinery..tractors +
                     Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                     CO2.emissions..kt. +
                     GDP.growth..annual... +
                     Inflation..consumer.prices..annual...+
                     Climate.deg.C. + Precipitation.mm. , data = train_data)


tree_pred <- predict(rice.tree, newdata = test_data)

rpart.plot(rice.tree)

tree_mse <- sqrt(mean((tree_pred - test_data$Yield_Rice)^2))
cat("Decision Tree Root Mean Squared Error:", tree_mse, "\n")

tree_rsquared <- 1 - sum((test_data$Yield_Rice - tree_pred)^2) / sum((test_data$Yield_Rice - mean(test_data$Yield_Rice))^2)
cat("Decision Tree R-squared:", tree_rsquared, "\n")





#POTATO
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Potato, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

potato.tree <- rpart(Yield_Potato ~ Country_Name + Country_Code +
                     Year +
                     Agricultural.land....of.land.area. +
                     Agricultural.machinery..tractors +
                     Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                     CO2.emissions..kt. +
                     GDP.growth..annual... +
                     Inflation..consumer.prices..annual...+
                     Climate.deg.C. + Precipitation.mm. , data = train_data)


tree_pred2 <- predict(potato.tree, newdata = test_data)

rpart.plot(potato.tree)

tree_mse2 <- sqrt(mean((tree_pred2 - test_data$Yield_Potato)^2))
cat("Decision Tree Root Mean Squared Error:", tree_mse2, "\n")

tree_rsquared2 <- 1 - sum((test_data$Yield_Potato - tree_pred2)^2) / sum((test_data$Yield_Potato - mean(test_data$Yield_Potato))^2)
cat("Decision Tree R-squared:", tree_rsquared2, "\n")


     #WHEAT
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Wheat, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

wheat.tree <- rpart(Yield_Wheat ~ Country_Name + Country_Code +
                       Year +
                       Agricultural.land....of.land.area. +
                       Agricultural.machinery..tractors +
                       Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                       CO2.emissions..kt. +
                       GDP.growth..annual... +
                       Inflation..consumer.prices..annual...+
                       Climate.deg.C. + Precipitation.mm. , data = train_data)


tree_pred3 <- predict(wheat.tree, newdata = test_data)

rpart.plot(wheat.tree)

tree_mse3 <- sqrt(mean((tree_pred3 - test_data$Yield_Wheat)^2))
cat("Decision Tree Root Mean Squared Error:", tree_mse3, "\n")

tree_rsquared3 <- 1 - sum((test_data$Yield_Wheat - tree_pred3)^2) / sum((test_data$Yield_Wheat - mean(test_data$Yield_Wheat))^2)
cat("Decision Tree R-squared:", tree_rsquared3, "\n")





#<--------------------------RANDOM FOREST ------------------------------------------------------------------>

set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Rice, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

cv_model_rf1 <- numeric(0)
accuracy_vector1 <- numeric(0)

for (ntree in seq(10, 100, by = 10)) {
  rf_model1 <- randomForest(Yield_Rice ~ Country_Name + Country_Code +
                              Year + 
                              Agricultural.land....of.land.area. +
                              Agricultural.machinery..tractors +
                              Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                              CO2.emissions..kt. +
                              GDP.growth..annual... +
                              Climate.deg.C. + Precipitation.mm.,
                            data = train_data, ntree = ntree)
  rf_pred1 <- predict(rf_model1, newdata = test_data)
  correct_predictions1 <- sum(rf_pred1 == test_data$Yield_Rice)
  cv_model_rf1 <- c(cv_model_rf1, correct_predictions1)
  
  # Calculate accuracy and store in the vector
  accuracy1 <- correct_predictions1 / nrow(test_data)
  accuracy_vector1 <- c(accuracy_vector1, accuracy1)
}

best_ntree1 <- seq(10, 100, by = 10)[which.max(cv_model_rf1)]
best_rf_model1 <- randomForest(Yield_Rice ~ Country_Name + Country_Code +
                                 Year + 
                                 Agricultural.land....of.land.area. +
                                 Agricultural.machinery..tractors +
                                 Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                                 CO2.emissions..kt. +
                                 GDP.growth..annual... +
                                 Climate.deg.C. + Precipitation.mm.,
                               data = train_data, ntree = best_ntree1)

# Predictions 
rf_pred1 <- predict(best_rf_model1, newdata = test_data)

# Variable Importance Plot
varImpPlot(best_rf_model1, main = "Variable Importance ", col = "black")

# Plot the cross-validation results
plot(seq(10, 100, by = 10), accuracy_vector1, type = "b", 
     xlab = "Number of Trees", ylab = "Accuracy",
     main = "Cross-Validation Results for Random Forest")

# Mark the best number of trees
abline(v = best_ntree1, col = "red", lty = 2)
text(best_ntree1, max(accuracy_vector1), labels = paste("Best ntree =", best_ntree1), pos = 1, col = "red")

# Evaluate the random forest model
rf_mse1 <- sqrt(mean((rf_pred1 - test_data$Yield_Rice)^2))
cat("Random Forest Root Mean Squared Error:", rf_mse1, "\n")

# Calculate R-squared on the test set
rf_rsquared1 <- 1 - sum((test_data$Yield_Rice - rf_pred1)^2) / sum((test_data$Yield_Rice - mean(test_data$Yield_Rice))^2)
cat("Random Forest R-squared:", rf_rsquared1, "\n")


#POTATO
# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Potato, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

cv_model_rf2 <- numeric(0)
accuracy_vector2 <- numeric(0)

for (ntree in seq(10, 100, by = 10)) {
  rf_model2 <- randomForest(Yield_Potato ~ Country_Name + Country_Code +
                              Year + 
                              Agricultural.land....of.land.area. +
                              Agricultural.machinery..tractors +
                              Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                              CO2.emissions..kt. +
                              GDP.growth..annual... +
                              Climate.deg.C. + Precipitation.mm.,
                            data = train_data, ntree = ntree)
  rf_pred2 <- predict(rf_model2, newdata = test_data)
  correct_predictions2 <- sum(rf_pred2 == test_data$Yield_Potato)
  cv_model_rf2 <- c(cv_model_rf2, correct_predictions2)
  
  # Calculate accuracy and store in the vector
  accuracy2 <- correct_predictions2 / nrow(test_data)
  accuracy_vector2 <- c(accuracy_vector2, accuracy2)
}

best_ntree2 <- seq(10, 100, by = 10)[which.max(cv_model_rf2)]
best_rf_model2 <- randomForest(Yield_Potato ~ Country_Name + Country_Code +
                                 Year + 
                                 Agricultural.land....of.land.area. +
                                 Agricultural.machinery..tractors +
                                 Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                                 CO2.emissions..kt. +
                                 GDP.growth..annual... +
                                 Climate.deg.C. + Precipitation.mm.,
                               data = train_data, ntree = best_ntree2)

# Predictions 
rf_pred2 <- predict(best_rf_model2, newdata = test_data)

# Variable Importance Plot
varImpPlot(best_rf_model2, main = "Variable Importance ", col = "darkgreen")

# Plot the cross-validation results
plot(seq(10, 100, by = 10), accuracy_vector2, type = "b", 
     xlab = "Number of Trees", ylab = "Accuracy",
     main = "Cross-Validation Results for Random Forest (Yield_Potato)")

# Mark the best number of trees
abline(v = best_ntree2, col = "red", lty = 2)
text(best_ntree2, max(accuracy_vector2), labels = paste("Best ntree =", best_ntree2), pos = 1, col = "red")

# Evaluate the random forest model
rf_mse2 <- sqrt(mean((rf_pred2 - test_data$Yield_Potato)^2))
cat("Random Forest Root Mean Squared Error (Yield_Potato):", rf_mse2, "\n")

# Calculate R-squared on the test set
rf_rsquared2 <- 1 - sum((test_data$Yield_Potato - rf_pred2)^2) / sum((test_data$Yield_Potato - mean(test_data$Yield_Potato))^2)
cat("Random Forest R-squared (Yield_Potato):", rf_rsquared2, "\n")


#WHEAT
# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Wheat, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

cv_model_rf3 <- numeric(0)
accuracy_vector3 <- numeric(0)

for (ntree in seq(10, 100, by = 10)) {
  rf_model3 <- randomForest(Yield_Wheat ~ Country_Name + Country_Code +
                              Year + 
                              Agricultural.land....of.land.area. +
                              Agricultural.machinery..tractors +
                              Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                              CO2.emissions..kt. +
                              GDP.growth..annual... +
                              Climate.deg.C. + Precipitation.mm.,
                            data = train_data, ntree = ntree)
  rf_pred3 <- predict(rf_model3, newdata = test_data)
  correct_predictions3 <- sum(rf_pred3 == test_data$Yield_Wheat)
  cv_model_rf3 <- c(cv_model_rf3, correct_predictions3)
  
  # Calculate accuracy and store in the vector
  accuracy3 <- correct_predictions3 / nrow(test_data)
  accuracy_vector3 <- c(accuracy_vector3, accuracy3)
}

best_ntree3 <- seq(10, 100, by = 10)[which.max(cv_model_rf3)]
best_rf_model3 <- randomForest(Yield_Wheat ~ Country_Name + Country_Code +
                                 Year + 
                                 Agricultural.land....of.land.area. +
                                 Agricultural.machinery..tractors +
                                 Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                                 CO2.emissions..kt. +
                                 GDP.growth..annual... +
                                 Climate.deg.C. + Precipitation.mm.,
                               data = train_data, ntree = best_ntree3)

# Predictions 
rf_pred3 <- predict(best_rf_model3, newdata = test_data)

# Variable Importance Plot
varImpPlot(best_rf_model3, main = "Variable Importance ", col = "darkblue")

# Plot the cross-validation results
plot(seq(10, 100, by = 10), accuracy_vector3, type = "b", 
     xlab = "Number of Trees", ylab = "Accuracy",
     main = "Cross-Validation Results for Random Forest (Yield_Wheat)")

# Mark the best number of trees
abline(v = best_ntree3, col = "red", lty = 2)
text(best_ntree3, max(accuracy_vector3), labels = paste("Best ntree =", best_ntree3), pos = 1, col = "red")

# Evaluate the random forest model
rf_mse3 <- sqrt(mean((rf_pred3 - test_data$Yield_Wheat)^2))
cat("Random Forest Root Mean Squared Error (Yield_Wheat):", rf_mse3, "\n")

# Calculate R-squared on the test set
rf_rsquared3 <- 1 - sum((test_data$Yield_Wheat - rf_pred3)^2) / sum((test_data$Yield_Wheat - mean(test_data$Yield_Wheat))^2)
cat("Random Forest R-squared (Yield_Wheat):", rf_rsquared3, "\n")





#<-----------------------------------POLYNOMIAL Regression----------------------------------------->
#RICE  
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Rice, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

# Assuming you want to add a quadratic term for 'Year'
train_data$Year_squared <- train_data$Year^2
test_data$Year_squared <- test_data$Year^2

# Build the polynomial regression model
rice.model_poly <- lm(Yield_Rice ~ Country_Name + Country_Code +
                        Year + Year_squared +
                        Agricultural.land....of.land.area.  +
                        Agricultural.machinery..tractors +
                        Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                        CO2.emissions..kt. +
                        GDP.growth..annual... + 
                        Climate.deg.C. + Precipitation.mm.
                      , data = train_data)

rice.predictions_poly <- predict(rice.model_poly, newdata = test_data)

mse_poly <- sqrt(mean((rice.predictions_poly - test_data$Yield_Rice)^2))
cat("Polynomial Model - Root Mean Squared Error:", mse_poly, "\n")

rsquared_poly <- 1 - sum((test_data$Yield_Rice - rice.predictions_poly)^2) / sum((test_data$Yield_Rice - mean(test_data$Yield_Rice))^2)
cat("Polynomial Model - R-squared:", rsquared_poly, "\n")

par(mfrow = c(1, 1))
# Plot for Yield_Rice
plot1 <- plot(test_data$Yield_Rice, rice.predictions_poly, 
              main = "Polynomial Regression: Actual vs Predicted (Rice)",
              xlab = "Actual Yield_Rice",
              ylab = "Predicted Yield_Rice",
              col = "purple")
abline(a = 0, b = 1, col = "red")
legend("bottomright", legend = c("Predictions", "Ideal Prediction Line"),
       col = c("purple", "red"), lty = 1:1, cex = 0.8)



#POTATO
set.seed(123)

# Split the data into training and testing sets
split <- sample.split(cleaned.data$Yield_Potato, SplitRatio = 0.7)
train_data <- subset(cleaned.data, split == TRUE)
test_data <- subset(cleaned.data, split == FALSE)

# Assuming you want to add a quadratic term for 'Year'
train_data$Year_squared <- train_data$Year^2
test_data$Year_squared <- test_data$Year^2

# Build the polynomial regression model for Yield_Potato
potato.model_poly <- lm(Yield_Potato ~ Country_Name + Country_Code +
                          Year + Year_squared +
                          Agricultural.land....of.land.area.  +
                          Agricultural.machinery..tractors +
                          Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                          CO2.emissions..kt. +
                          GDP.growth..annual... + 
                          Climate.deg.C. + Precipitation.mm.
                        , data = train_data)

potato.predictions_poly <- predict(potato.model_poly, newdata = test_data)

mse_poly_potato <- sqrt(mean((potato.predictions_poly - test_data$Yield_Potato)^2))
cat("Polynomial Model for Yield_Potato - Root Mean Squared Error:", mse_poly_potato, "\n")

rsquared_poly_potato <- 1 - sum((test_data$Yield_Potato - potato.predictions_poly)^2) / sum((test_data$Yield_Potato - mean(test_data$Yield_Potato))^2)
cat("Polynomial Model for Yield_Potato - R-squared:", rsquared_poly_potato, "\n")

# Plot for Yield_Potato
plot2 <- plot(test_data$Yield_Potato, potato.predictions_poly,
              main = "Polynomial Regression: Actual vs Predicted (Potato)",
              xlab = "Actual Yield_Potato",
              ylab = "Predicted Yield_Potato",
              col = "blue")
abline(a = 0, b = 1, col = "red")
legend("bottomright", legend = c("Predictions", "Ideal Prediction Line"),
       col = c("blue", "red"), lty = 1:1, cex = 0.8)
#WHEAT
set.seed(123)
#filtered.data<-cleaned.data
# Split the data into training and testing sets
split <- sample.split(filtered.data$Yield_Wheat, SplitRatio = 0.7)
train_data <- subset(filtered.data, split == TRUE)
test_data <- subset(filtered.data, split == FALSE)
  
# Assuming you want to add a quadratic term for 'Year'
train_data$Year_squared <- train_data$Year^2
test_data$Year_squared <- test_data$Year^2

# Build the polynomial regression model for Yield_Wheat
wheat.model_poly <- lm(Yield_Wheat ~ Country_Name + Country_Code +
                         Year + Year_squared +
                         Agricultural.land....of.land.area.  +
                         Agricultural.machinery..tractors +
                         Fertilizer.consumption..kilograms.per.hectare.of.arable.land. +
                         CO2.emissions..kt. +
                         GDP.growth..annual... + 
                         Climate.deg.C. + Precipitation.mm.
                       , data = train_data)

wheat.predictions_poly <- predict(wheat.model_poly, newdata = test_data)

mse_poly_wheat <- sqrt(mean((wheat.predictions_poly - test_data$Yield_Wheat)^2))
cat("Polynomial Model for Yield_Wheat - Root Mean Squared Error:", mse_poly_wheat, "\n")

rsquared_poly_wheat <- 1 - sum((test_data$Yield_Wheat - wheat.predictions_poly)^2) / sum((test_data$Yield_Wheat - mean(test_data$Yield_Wheat))^2)
cat("Polynomial Model for Yield_Wheat - R-squared:", rsquared_poly_wheat, "\n")

## Plot for Yield_Wheat
 plot(test_data$Yield_Wheat, wheat.predictions_poly, 
              main = "Polynomial Regression: Actual vs Predicted (Wheat)",
              xlab = "Actual Yield_Wheat",
              ylab = "Predicted Yield_Wheat",
              col = "green")
abline(a = 0, b = 1, col = "red")
legend("bottomright", legend = c("Predictions", "Ideal Prediction Line"),
       col = c("green", "red"), lty = 1:1, cex = 0.8)

