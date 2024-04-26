# Load packages
library(dplyr)
library(skimr)
library(ggplot2)
library(car)
library(leaps)
library(corrplot)
library(glmnet)
library(MASS)
library(Metrics)

# Import data
car_data1 <- read.csv("car_ads_data.csv", header = TRUE)
car_data2 <- read.csv("car_ads_data_02.csv", header = TRUE)

# Examine the data sets
View(car_data1)
View(car_data2)

# Remove whitespaces and replace text NA's with actual NA's in car data 2
car_data2[] <- lapply(car_data2, trimws)
car_data2[car_data2 == "NA"] <- NA

# Combine the two data sets
car_data <- rbind(car_data1, car_data2)

# Examine the new data set
View(car_data)
skim(car_data)
str(car_data)
summary(car_data)

# Handle duplicates
sum(duplicated(car_data$Id))   # 6449 duplicates
car_data <- car_data[!duplicated(car_data$Id), ]
sum(duplicated(car_data))   # Duplicates removed
dim(car_data)   # 11792x15

# Clean the miltal variable from space characters (remove all non-numeric chr)
car_data$Miltal <- gsub("[^0-9]", "", car_data$Miltal)

# Change data types to numerical values and factors
car_data <- car_data %>%
  mutate_at(c("Miltal", "Modellår", "HK", "Motorstorlek", "Pris"), as.numeric) %>%
  mutate_at(c("Märke", "Modell", "Bränsle", "Växellåda", "Biltyp", "Drivning", "Färg", "Datum.i.trafik", "Region"), as.factor)

# Handle missing values
sum(is.na(car_data))    # 4619
colSums(is.na(car_data))

# Remove rows with NA's in "Pris"
car_data <- car_data[!is.na(car_data$Pris), ]
sum(is.na(car_data))    # 2901

# Check the effect of dropping motorstorlek
plot(car_data$Motorstorlek, car_data$HK)
car_data <- car_data[, !(names(car_data) %in% c("Motorstorlek"))] 
sum(is.na(car_data))    # 1972

# Remove all remaining missing values
car_data <- na.omit(car_data)
sum(is.na(car_data))   # All NA's removed
dim(car_data)   # 11012x14

# Calculate the car's age from "Datum i trafik"
car_data$Datum.i.trafik <- as.Date(car_data$Datum.i.trafik, format = "%Y-%m-%d")
car_data$Ålder <- as.numeric(difftime(Sys.Date(), car_data$Datum.i.trafik, units = "days") / 365.25)
#car_data$Ålder <- as.numeric(Sys.Date() - car_data$Datum.i.trafik)
plot(car_data$Ålder, car_data$Pris)

# Remove two outliers, due to incorrect data entries in "Datum i trafik"
car_data <- car_data[!(car_data$Id %in% c(1400507742, 1400624020)), ]
plot(car_data$Ålder, car_data$Pris)

# Drop "Datum i trafik" 
car_data <- car_data[, !(names(car_data) %in% c("Datum.i.trafik"))]
head(car_data)

# Redefine filters for mileage and price
plot(car_data$Miltal, car_data$Pris)
car_data <- car_data[car_data$Miltal > 0 & car_data$Miltal <= 40000 & car_data$Pris > 20000 & car_data$Pris <= 750000, ]
plot(car_data$Miltal, car_data$Pris)
hist(car_data$Pris)
hist(car_data$Miltal)
dim(car_data)   # 10672x14

# Keep only the most common car types
car_data <- car_data[car_data$Biltyp %in% c("Kombi", "Halvkombi", "Sedan", "SUV"), ]
car_data$Biltyp <- droplevels(car_data$Biltyp)
dim(car_data)   # 9537x14

# Keep brands with more than 30 observations, 
counts <- table(car_data$Märke)
less_than_30 <- names(counts[counts < 30])
car_data <- car_data[!car_data$Märke %in% less_than_30, ]
car_data$Märke <- droplevels(car_data$Märke)
barplot(table(car_data$Märke))
table(car_data$Märke)
dim(car_data)   # 9291x14

# Drop Modell
car_data <- car_data[, !(names(car_data) %in% c("Modell"))]

# Check distribution and correlations
plot(car_data$Bränsle, car_data$Pris)
plot(car_data$Biltyp, car_data$Pris)
plot(car_data$Färg, car_data$Pris)
plot(car_data$Märke, car_data$Pris)
plot(car_data$Drivning, car_data$Pris)

# Check correlations for numeric variables, drop "Id"
car_data <- car_data[, !(names(car_data) %in% c("Id"))]
correlation_matrix <- cor(car_data[, sapply(car_data, is.numeric)], use="pairwise.complete.obs")
correlation_matrix

# Modellår and Ålder are highly correlated, as expected
plot(car_data$Modellår, car_data$Ålder)

# Drop Modellår 
car_data <- car_data[, !(names(car_data) %in% c("Modellår"))]

# Assess normality distribution, log transform right-skewed data
hist(car_data$HK)
car_data$HK_log <- as.numeric(log(car_data$HK))
hist(car_data$HK_log)

hist(car_data$Pris)
car_data$Pris_log <- as.numeric(log(car_data$Pris))
hist(car_data$Pris_log)
par(mfrow=c(2,1))
qqPlot(car_data$Pris)
qqPlot(car_data$Pris_log)

hist(car_data$Miltal)
hist(car_data$Ålder)

table(car_data$Färg)
table(car_data$Region)
table(car_data$Bränsle)
table(car_data$Växellåda)
table(car_data$Biltyp)


#### Data preparation

spec = c(train = .6, validate = .2, test = .2)

set.seed(123)
g = sample(cut(
  seq(nrow(car_data)), 
  nrow(car_data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(car_data, g)

train <- res$train
val <- res$validate
test <- res$test


##### First MLR model

model_1 <- lm(Pris ~. -Pris_log -HK_log, train)
summary(model_1)
par(mfrow=c(2,2))
plot(model_1)
vif(model_1)   # Märke

##### MLR Model 2
model_2 <- lm(Pris_log ~. -HK -Pris -Märke, train) 
summary(model_2)
par(mfrow=c(2,2))
plot(model_2)
vif(model_2)

##### MLR Model 3
model_3 <- lm(Pris_log ~. -HK -Pris -Märke -Färg -Region, train) # ingen skillnad när vi tog bort Region och Färg
summary(model_3)
par(mfrow=c(2,2))
plot(model_3)
vif(model_3)


#### Best subset selection

regfit_full <- regsubsets(Pris_log ~ .-Märke -HK -Pris, train)
reg.summary <- summary(regfit_full)

# Plotting model evaluation using different metrics
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2), reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

coef(regfit_full, 5)


##### MLR Model 4
model_4 <- lm(Pris_log ~ Miltal + Ålder + HK_log + Bränsle + Växellåda, data=train)
summary(model_4)
par(mfrow=c(2,2))
plot(model_4)
vif(model_4)

#### MLR Model 5
model_5 <- lm(Pris_log ~ Ålder, data=train)
summary(model_5)
par(mfrow=c(2,2))
plot(model_5)

#### Evaluating models on validation data

val_pred_m2 <- predict(model_2, newdata = val)
val_pred_m3 <- predict(model_3, newdata = val)
val_pred_m4 <- predict(model_4, newdata = val)
val_pred_m5 <- predict(model_5, newdata = val)

results <- data.frame(
  Model = c("Model 2", "Model 3", "Model 4", "Model 5"),
  RMSE_val_data = c(exp(rmse(val$Pris_log, val_pred_m2)),
                    exp(rmse(val$Pris_log, val_pred_m3)),
                    exp(rmse(val$Pris_log, val_pred_m4)),
                    exp(rmse(val$Pris_log, val_pred_m5))),
  Adj_R_squared = c(summary(model_2)$adj.r.squared,
                    summary(model_3)$adj.r.squared,
                    summary(model_4)$adj.r.squared,
                    summary(model_5)$adj.r.squared)
)

results

# Evaluate on test data
test_pred_m3 <- predict(model_3, newdata = test)
test_rmse <- (sqrt(mean((test$Pris_log - test_pred_m3)^2)))
test_rmse  # No tendencies to be overfitting
exp(test_rmse)

### Retrain final model on the whole data set

final_model <- lm(Pris_log ~ Miltal + Ålder + HK_log + Bränsle + Växellåda, car_data)
par(mfrow=c(2,2))
plot(final_model)
vif(final_model)

# Log-transform the coefficients
final_model$coefficients <- exp(final_model$coefficients)

summary(final_model)
confint(final_model)

# Predict prices on new data
new_data <- data.frame(
  Bränsle = c("Diesel", "Bensin", "Diesel"),
  Växellåda = c("Manuell", "Automat", "Automat"),
  Miltal = c(32888, 6409, 31458),
  Ålder = c(13.28131, 5.598905, 8.301164),
  HK_log = c(log(164), log(191), log(191)) # Use log() function for HK as well
)

# Create CI & PI for predictions
confidence_intervals <- predict(final_model, newdata = new_data, interval = "confidence", level = 0.95)
prediction_intervals <- predict(final_model, newdata = new_data, interval = "prediction", level = 0.95)

exp(confidence_intervals)
exp(prediction_intervals)



#### För att mata in nya bilars data (ålder)

# Ange specifikt datum för bilen
#specific_date <- as.Date("2016-01-05", format = "%Y-%m-%d")  # Byt ut detta mot det faktiska datumet för bilen
# Beräkna ålder för den specifika bilen
#current_date <- Sys.Date()  # Dagens datum
#age_of_car <- as.numeric(difftime(current_date, specific_date, units = "days") / 365.25)
# Visa åldern för den specifika bilen
#age_of_car
