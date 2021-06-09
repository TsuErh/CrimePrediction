# ----------------- Crime_and_Incarceration_by_State (LARGE POPULATION STATES) -----------------
# --------------------- Prisoner Count Prediction ----------------------

# Read Packages
library(tidyverse)
library(MASS)
library(caret)
library(car)
library(Metrics)

# Set up work direction
setwd('/Users/tsuerh/Documents/Depaul/DSC423\ Data\ Analysis\ and\ Regression/GroupProject/')
# Read data set
crime <- read.csv(file = 'crime_and_incarceration_by_state.csv')

# --------------------------------------------------------------------------------------------------------
# Transform crime variables into ratios
crime$murder_manslaughter <- crime$murder_manslaughter / crime$state_population
crime$rape_crime <- crime$rape_crime / crime$state_population
crime$robbery <- crime$robbery / crime$state_population
crime$agg_assault <- crime$agg_assault / crime$state_population
crime$burglary <- crime$burglary / crime$state_population
crime$larceny <- crime$larceny / crime$state_population
crime$vehicle_theft <- crime$vehicle_theft / crime$state_population

# Split data set by population
Crime_Large <- crime %>% 
  filter(jurisdiction %in% c('MISSOURI','INDIANA', 'TENNESSEE', 'MASSACHUSETTS', 
                             'ARIZONA', 'WASHINGTON', 'VIRGINIA', 'NEW JERSEY', 
                             'MICHIGAN', 'NORTH CAROLINA', 'GEORGIA', 'OHIO', 'PENNSYLVANIA',
                             'ILLINOIS', 'NEW YORK', 'FLORIDA', 'TEXAS', 'CALIFORNIA'))
summary(Crime_Large)
View(Crime_Large)

# --------------------------------------------------------------------------------------------------------
# Simple Model
# PART 1
crime.model.1 <- lm(prisoner_count ~ state_population + murder_manslaughter + rape_crime +
                    robbery + agg_assault + burglary + larceny + vehicle_theft + 
                    crime_reporting_change + crimes_estimated, data = Crime_Large)
summary(crime.model.1)

crime.model.2 <- lm(prisoner_count ~ state_population + murder_manslaughter + rape_crime +
                      agg_assault + larceny + vehicle_theft + crimes_estimated, data = Crime_Large)
summary(crime.model.2)
vif(crime.model.2)
# Check Residuals
crime.model.2$residuals
sum(crime.model.2$residuals)
hist(crime.model.2$residuals, breaks = 100)

mean = mean(crime.model.2$residuals)
sd = sd(crime.model.2$residuals)
resid_zscore = (crime.model.2$residuals - mean)/sd
hist(resid_zscore, breaks = 100)

durbinWatsonTest(crime.model.2)

# Plots
plot(Crime_Large$crime_reporting_change, resid_zscore)
plot(Crime_Large$crimes_estimated, resid_zscore)
plot(Crime_Large$state_population, resid_zscore)
plot(Crime_Large$murder_manslaughter, resid_zscore)
plot(Crime_Large$rape_crime, resid_zscore)
plot(Crime_Large$robbery, resid_zscore)
plot(Crime_Large$agg_assault, resid_zscore)
plot(Crime_Large$burglary, resid_zscore)
plot(Crime_Large$larceny, resid_zscore)
plot(Crime_Large$vehicle_theft, resid_zscore)

plot(crime.model.2)


# PART 3 - Second order & interaction
# Interaction Term
Crime_Large$v8v9 <- Crime_Large$murder_manslaughter * Crime_Large$rape_crime
Crime_Large$v8v11 <- Crime_Large$murder_manslaughter * Crime_Large$agg_assault
Crime_Large$v8v13 <- Crime_Large$murder_manslaughter * Crime_Large$larceny
Crime_Large$v8v10 <- Crime_Large$murder_manslaughter * Crime_Large$robbery
Crime_Large$v9v11 <- Crime_Large$rape_crime * Crime_Large$agg_assault
Crime_Large$v9v14 <- Crime_Large$rape_crime * Crime_Large$vehicle_theft
Crime_Large$v10v12 <- Crime_Large$robbery * Crime_Large$burglary
Crime_Large$v11v12 <- Crime_Large$agg_assault * Crime_Large$burglary
Crime_Large$v11v13 <- Crime_Large$agg_assault * Crime_Large$larceny
Crime_Large$v12v13 <- Crime_Large$burglary * Crime_Large$larceny
Crime_Large$v12v14 <- Crime_Large$burglary * Crime_Large$vehicle_theft
Crime_Large$v10v13 <- Crime_Large$larceny * Crime_Large$robbery
Crime_Large$aggSQ <- Crime_Large$agg_assault * Crime_Large$agg_assault
Crime_Large$burglarySQ <- Crime_Large$burglary * Crime_Large$burglary
Crime_Large$larcenySQ <- Crime_Large$larceny * Crime_Large$larceny
Crime_Large$theftSQ <- Crime_Large$vehicle_theft * Crime_Large$vehicle_theft


# Build model
crime.model.5 <- lm(prisoner_count ~ state_population + murder_manslaughter + rape_crime +
                      robbery + agg_assault + burglary + larceny + vehicle_theft + 
                      crime_reporting_change + crimes_estimated + aggSQ + burglarySQ + 
                      larcenySQ + theftSQ + v8v9 + v8v10 + v8v11 + v8v13 + v9v11 + 
                      v10v12 + v11v12 + v11v13 + v12v14, data = Crime_Large)
summary(crime.model.5)


crime.model.6 <- lm(prisoner_count ~ state_population + murder_manslaughter + rape_crime +
                      robbery + agg_assault + burglary + larceny + vehicle_theft + crimes_estimated + aggSQ + burglarySQ + 
                      v9v11 + v10v12 + v11v13, data = Crime_Large)
summary(crime.model.6)
vif(crime.model.6)


# Check Residuals
crime.model.6$residuals
sum(crime.model.6$residuals)
hist(crime.model.6$residuals, breaks = 100)

mean = mean(crime.model.6$residuals)
sd = sd(crime.model.6$residuals)
resid_zscore = (crime.model.6$residuals - mean)/sd
hist(resid_zscore, breaks = 100)

durbinWatsonTest(crime.model.6)

plot(crime.model.6)

#plot(Crime_L2$crime_reporting_change, resid_zscore)
plot(Crime_L2$crimes_estimated, resid_zscore)
plot(Crime_L2$state_population, resid_zscore)
plot(Crime_L2$murder_manslaughter, resid_zscore)
plot(Crime_L2$rape_crime, resid_zscore)
plot(Crime_L2$robbery, resid_zscore)
plot(Crime_L2$agg_assault, resid_zscore)
plot(Crime_L2$burglary, resid_zscore)
plot(Crime_L2$larceny, resid_zscore)
plot(Crime_L2$vehicle_theft, resid_zscore)


# Split train test set
partition <- sample(2, nrow(Crime_Large), replace = TRUE, prob = c(0.80, 0.20))
train <- Crime_Large[partition == 1,]
test <- Crime_Large[partition == 2,]
y <- test$prisoner_count

# Cross validation
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
cv = train(prisoner_count ~ state_population + murder_manslaughter + rape_crime +
              robbery + agg_assault + burglary + larceny + vehicle_theft + crimes_estimated + aggSQ + burglarySQ + 
              v9v11 + v10v12 + v11v13 + v12v14, data = train, method = 'lm', trControl = train.control)
cv

summary(cv$finalModel)
plot(cv)

prediction <- predict(cv$finalModel, test)
cor(prediction, y)
plot(prediction, test$readingScore)
final <- cv$finalModel

# Check residuals
plot(final$fitted.values, final$residuals)
hist(final$residuals)

qqnorm(final$residuals)
qqline(final$residuals)

mean = mean(final$residuals)
sd = sd(final$residuals)
resid_zscore = (final$residuals - mean)/sd
plot(final)
hist(resid_zscore)


