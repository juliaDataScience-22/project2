knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(Amelia)
library(caret)
data_raw <- read_csv("https://raw.githubusercontent.com/juliaDataScience-22/project2/main/studentDataToModel.csv")
data_raw <- data_raw |>
  select(PH, everything())

summary(data_raw)
na_list <- sapply(data_raw, function(x) sum(is.na(x)))

na_df <- data.frame(
  variable = names(na_list),
  count_na = unlist(na_list)) %>% 
  arrange(desc(count_na))

na_df %>% 
  summarize(sum(count_na))

# top ten
na_topten <- head(na_df,10)
na_topten

# bar plot
na_plot2 <- na_df %>% 
  ggplot(aes(x = reorder(variable,count_na,decreasing = TRUE), 
             y = count_na)) +
  geom_bar (stat="identity", fill = "lightblue") +
  ggtitle ("Count of Missing Values by Variable") +
  xlab ("Variable") +
  ylab("Count of NA Values") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))

na_plot2
ggsave("missing.png", plot = na_plot2, width = 8, height = 5)

# densityt plot from amelia package
missmap(data_raw, x.cex = 0.7, gap.xaxis = 0, 
        main = "Missing Data: Density Map by Variable")
# Categorical variable

brand_plot <-data_raw %>% 
  ggplot(aes(x=`Brand Code`)) + 
  geom_bar(fill = "cornsilk3") +
  ggtitle("Distribution by Brand Code") +
  scale_y_continuous(breaks = pretty_breaks(10))

brand_plot
ggsave("brand_histogram.png", plot = brand_plot, width = 8, height = 4)


# Numeric variables
data_melt <- melt(data_raw)

hist_plot <- data_melt %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(fill = "seagreen") + 
  facet_wrap(~variable, scales='free_x') +
  theme(
    title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size =12, face = "bold"),
    axis.title = element_text(size = 12)) +
  ggtitle("Frequency by Numeric Variable: All Brand Codes")

hist_plot
ggsave("histogram.png", plot = hist_plot, width = 16, height = 12)
cor_vars <- select(data_raw, -`Brand Code`)
cor_matrix <- cor(cor_vars, use ="pairwise.complete.obs")
plot_matrix <- melt(cor_matrix)

# extract most highly correlated pairs (positive or negative)
highly_cor <- filter(plot_matrix, (value !=1 & (value >.6 | value < -.6))) %>%   arrange(desc(value)) %>% 
  distinct(value, .keep_all = TRUE)

highly_cor

# plot all 
plot_correlations <- plot_matrix %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 14, hjust = 1),
        axis.text.y = element_text(size = 14)) 

plot_correlations
ggsave("correlations.png", plot = plot_correlations, width = 12, height = 8)
# Box plots for detecting outliers in numerical variables
plot_outlier <- data_melt %>% 
  ggplot(aes(y = value)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle("Box Plots for Outliers Detection")

plot_outlier
ggsave("outliers.png", plot = plot_outlier, width = 16, height = 12)
# numeric predictors only
numeric_predictors <- names(data_raw)[sapply(data_raw, is.numeric) & 
                                        names(data_raw) != "PH"]

# outlier function
is_outlier <- function(x) {
  return(abs(x - mean(x, na.rm = TRUE)) > 2 * sd(x, na.rm = TRUE))
}

# loop
for (var2 in numeric_predictors) {
  var2_fixed <- paste0("`", var2, "`")  
  data_raw$outlier <- is_outlier(data_raw[[var2]])
  
  scatter1 <- ggplot(data_raw, aes_string(x = var2_fixed, y = "`PH`")) +
    geom_point() +
    geom_point(data = subset(data_raw, outlier == TRUE), aes_string(x = var2_fixed, y = "`PH`"), color = 'red') +
    labs(title = paste("Scatter Plot of", var2, "vs. PH with Outliers Highlighted"),
         x = var2,
         y = "PH") +
    theme(title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) 
  print(scatter1)
  ggsave(paste0("scatter-",var2_fixed,".png"), plot = scatter1, width = 8, height = 6)
}

data_raw <- data_raw |>
  select(-outlier)
columnTypes <- read_csv("https://raw.githubusercontent.com/juliaDataScience-22/project2/main/dataColumnTypes.csv")
View(columnTypes)

dataToPredict <- read_csv("https://raw.githubusercontent.com/juliaDataScience-22/project2/main/studentEvaluationToPredict.csv")
View(dataToPredict)
# Install and load the package
library(mice)

# Rename columns
library(stringr)
colnames(data_raw) <- c(
  "pH", "brandCode", "carbVolume", "fillOunces", "pcVolume", "carbPressure",
  "carbTemp", "psc", "pscFill", "pscCO2", "mnfFlow", "carbPressure1",
  "fillPressure", "hydPressure1", "hydPressure2", "hydPressure3", "hydPressure4",
  "fillerLevel", "fillerSpeed", "temperature", "usageCont", "carbFlow", "density",
  "mfr", "balling", "pressureVacuum", "oxygenFiller", "bowlSetpoint", "pressureSetpoint",
  "airPressurer", "alchRel", "carbRel", "ballingLvl"
)

if (!is.factor(data_raw$brandCode)) {
  data_raw$brandCode <- as.factor(data_raw$brandCode)
}

methods <- c(
  "pmm", "polyreg", "pmm", "pmm", "pmm", "pmm", 
  "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", 
  "pmm", "pmm", "pmm", "pmm", "pmm", 
  "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", 
  "pmm", "pmm", "pmm", "pmm", "pmm", "pmm", 
  "pmm", "pmm", "pmm", "pmm"
)

# Perform multiple imputation
imputed_data <- mice(data_raw, m = 5, method = methods, maxit = 5, seed = 500)

# Complete the imputed data
complete_data <- complete(imputed_data, 1)


# Handle categorical/binary variables
encoded_data <- model.matrix(~ . - 1, data = complete_data)

df_Final_Data <- as.data.frame(encoded_data)

# Install and load the caret package
library(caret)

# Example: Split data into 70% training and 30% testing
index <- createDataPartition(df_Final_Data$pH, p = 0.7, list = FALSE)
training_data <- df_Final_Data[index, ]
testing_data <- df_Final_Data[-index, ]

# View the dimensions of training and testing data
dim(training_data)
dim(testing_data)

# Drop brandCodeA to avoid perfect multicollinearity
ols_train <- training_data %>%
  select(-brandCodeA)

# OLS Regression with stepwise selection
model <- lm(pH ~ ., data = ols_train)
#summary(model)

# Stepwise selection
step_model <- step(model, direction = "both", trace = 0)
summary(step_model)

# Rewrite using the train function from the caret package to make it easier to compare with other models

# Extract the formula from the step model
stepwise_formula <- formula(step_model)

step_model_train <- train(stepwise_formula, 
                        data = ols_train, 
                        method = "lm",
                        trControl = caret::trainControl(method = "cv",
                                                  number = 10))
# Install and load the pls package
library(pls)

# PLS Regression with cross-validation
pls_model_full <- plsr(pH ~ ., data = training_data, scale = TRUE, validation = "CV")
summary(pls_model_full)

# Select the top 15 components based on diminishing returns in explained variance
pls_model1 <- plsr(pH ~ ., data = training_data, scale = TRUE, validation = "CV", ncomp = 15)
#summary(pls_model)

# Rewrite using the train function from the caret package to make it easier to compare with other models
tune_grid <- expand.grid(ncomp = 1:15)
pls_model <- train(pH ~ ., data = training_data, method = "pls", trControl = trainControl(method = "cv", number = 10), tune_grid = tune_grid)
# Load necessary libraries
library(caret)
library(elasticnet)

# Elastic Net Regression with cross-validation
enet_model_full <- train(pH ~ ., data = training_data, method = "enet", trControl = trainControl(method = "cv", number = 10))

# Get R-squared value
enet_results <- enet_model_full$results
r_squared <- enet_results[enet_results$lambda == enet_model_full$bestTune$lambda & enet_results$fraction == enet_model_full$bestTune$fraction, "Rsquared"]

r_squared
# Random Forest with cross-validation
rf_model <- train(pH ~ ., data = training_data, method = "rf", trControl = trainControl(method = "cv", number = 10))

# Cubist with cross-validation
cubist_model <- train(pH ~ ., data = training_data, method = "cubist", trControl = trainControl(method = "cv", number = 10))
# SVM with cross-validation
svm_model <- train(pH ~ ., data = training_data, method = "svmRadial", trControl = trainControl(method = "cv", number = 10))
summary(svm_model)

# Plot the results
plot(svm_model)
# Compare the models
resamples <- resamples(list(OLS = step_model_train, PLS = pls_model, ElasticNet = enet_model_full, Random_Forest = rf_model, Cubist = cubist_model, SVM = svm_model))
resample_summary <- summary(resamples)
library(knitr)
# Convert summary to data frame
MAE_df <- as.data.frame(resample_summary$statistics$MAE)
MAE_df$Metric <- "MAE"
MAE_df$Model <- rownames(MAE_df)
rownames(MAE_df) <- NULL

RMSE_df <- as.data.frame(resample_summary$statistics$RMSE)
RMSE_df$Metric <- "RMSE"
RMSE_df$Model <- rownames(RMSE_df)
rownames(RMSE_df) <- NULL

Rsquared_df <- as.data.frame(resample_summary$statistics$Rsquared)
Rsquared_df$Metric <- "Rsquared"
Rsquared_df$Model <- rownames(Rsquared_df)
rownames(Rsquared_df) <- NULL

# Combine all data frames
combined_df <- bind_rows(MAE_df, RMSE_df, Rsquared_df)

# Melt the data frame for easier manipulation
melted_df <- melt(combined_df, id.vars = c("Model", "Metric"))

# Reorder the columns for better readability
final_df <- dcast(melted_df, Model + Metric ~ variable)

# Display the final dataframe
kable(final_df)
# Tune the Cubist model
tune_grid <- expand.grid(committees = seq(10, 100, by = 10), neighbors = c(0,3,5,7,9))

tuned_cubist_model <- train(pH ~ ., data = training_data, method = "cubist", trControl = trainControl(method = "cv", number = 10), tuneGrid = tune_grid)
# Evaluate all the models on the testing data
ols_pred <- predict(step_model, newdata = testing_data)
pls_pred <- predict(pls_model, newdata = testing_data)
enet_pred <- predict(enet_model_full, newdata = testing_data)
rf_pred <- predict(rf_model, newdata = testing_data)
cubist_first_pred <- predict(cubist_model, newdata = testing_data)
svm_pred <- predict(svm_model, newdata = testing_data)
cubist_tuned_pred <- predict(tuned_cubist_model, newdata = testing_data)

# Calculate the MAPE for each model
mape_ols <- mean(abs(testing_data$pH - ols_pred) / testing_data$pH)
mape_pls <- mean(abs(testing_data$pH - pls_pred) / testing_data$pH)
mape_enet <- mean(abs(testing_data$pH - enet_pred) / testing_data$pH)
mape_rf <- mean(abs(testing_data$pH - rf_pred) / testing_data$pH)
mape_cubist_first <- mean(abs(testing_data$pH - cubist_first_pred) / testing_data$pH)
mape_svm <- mean(abs(testing_data$pH - svm_pred) / testing_data$pH)
mape_cubist_tuned <- mean(abs(testing_data$pH - cubist_tuned_pred) / testing_data$pH)

# Calculate the RMSE for each model
rmse_ols <- sqrt(mean((testing_data$pH - ols_pred)^2))
rmse_pls <- sqrt(mean((testing_data$pH - pls_pred)^2))
rmse_enet <- sqrt(mean((testing_data$pH - enet_pred)^2))
rmse_rf <- sqrt(mean((testing_data$pH - rf_pred)^2))
rmse_cubist_first <- sqrt(mean((testing_data$pH - cubist_first_pred)^2))
rmse_svm <- sqrt(mean((testing_data$pH - svm_pred)^2))
rmse_cubist_tuned <- sqrt(mean((testing_data$pH - cubist_tuned_pred)^2))

# Create a data frame with the evaluation metrics
evaluation_metrics <- data.frame(
  Model = c("OLS", "PLS", "ElasticNet", "Random_Forest", "Cubist_First", "SVM", "Cubist_Tuned"),
  MAPE = c(mape_ols, mape_pls, mape_enet, mape_rf, mape_cubist_first, mape_svm, mape_cubist_tuned),
  RMSE = c(rmse_ols, rmse_pls, rmse_enet, rmse_rf, rmse_cubist_first, rmse_svm, rmse_cubist_tuned)
)

kable(evaluation_metrics)
# Extract variable importance from the Cubist model
var_importance <- varImp(tuned_cubist_model)

# Plot variable importance
plot(var_importance)
# prp plots
library(pdp)

# Extract the partial dependence of the most important variables
pdp_mnfFlow <- partial(tuned_cubist_model, pred.var = "mnfFlow")
pdp_ballingLvl <- partial(tuned_cubist_model, pred.var = "ballingLvl")
pdp_balling <- partial(tuned_cubist_model, pred.var = "balling")
pdp_alchRel <- partial(tuned_cubist_model, pred.var = "alchRel")
pdp_pressureVacuum <- partial(tuned_cubist_model, pred.var = "pressureVacuum")
# Plot the partial dependence
plot(pdp_mnfFlow)
plot(pdp_ballingLvl)
plot(pdp_balling)
plot(pdp_alchRel)
plot(pdp_pressureVacuum)
# Do all the same data prep steps as before
# Rename columns
colnames(dataToPredict) <- c(
  "brandCode", "carbVolume", "fillOunces", "pcVolume", "carbPressure",
  "carbTemp", "psc", "pscFill", "pscCO2", "mnfFlow", "carbPressure1",
  "fillPressure", "hydPressure1", "hydPressure2", "hydPressure3", "hydPressure4",
  "fillerLevel", "fillerSpeed", "temperature", "usageCont", "carbFlow", "density",
  "mfr", "balling", "pressureVacuum", "pH", "oxygenFiller", "bowlSetpoint", "pressureSetpoint",
  "airPressurer", "alchRel", "carbRel", "ballingLvl"
)

# Drop the ph column
dataToPredict <- dataToPredict |>
  select(-pH)

# Perform multiple imputation
imputed_eval <- mice(dataToPredict, m = 5, method = 'pmm', maxit = 5, seed = 500)

# Complete the imputed data
complete_eval <- complete(imputed_eval, 1)

# Handle categorical/binary variables
encoded_eval <- model.matrix(~ . - 1, data = complete_eval)

df_Final_Eval <- as.data.frame(encoded_eval)

# Check for NAs
sum(is.na(complete_eval$brandCode))
# find missing rows in df_Final_Eval
colnames(df_Final_Eval)
# Predict the pH value
predictions <- predict(tuned_cubist_model, newdata = df_Final_Eval)

# Add the predictions to the dataToPredict dataset
dataToPredict$pH <- predictions

# View the dataToPredict dataset with the predicted pH values
View(dataToPredict)
summary <- t(summary(data_raw))
library(kableExtra)
kable(summary)
