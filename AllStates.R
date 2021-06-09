# ----------------- Crime_and_Incarceration_by_State (ALLSTATES) -----------------
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
crime_and_incarceration_by_state <- read.csv(file = 'crime_and_incarceration_by_state.csv')

#rename for easier reference; make sure to change name or import method based on your preference
crime <- crime_and_incarceration_by_state

# Transform crime variables into ratios
crime$murder_manslaughter <- crime$murder_manslaughter / crime$state_population
crime$rape_crime <- crime$rape_crime / crime$state_population
crime$robbery <- crime$robbery / crime$state_population
crime$agg_assault <- crime$agg_assault / crime$state_population
crime$burglary <- crime$burglary / crime$state_population
crime$larceny <- crime$larceny / crime$state_population
crime$vehicle_theft <- crime$vehicle_theft / crime$state_population

# Check dataset
summary(crime)

# Create second order and interaction terms
crime$populationSQ <- crime$state_population * crime$state_population
crime$manslaughterSQ <- crime$murder_manslaughter * crime$murder_manslaughter
crime$rapeSQ <- crime$rape_crime *crime$rape_crime
crime$robberySQ <- crime$robbery * crime$robbery
crime$agg_SQ <- crime$agg_assault * crime$agg_assault
crime$burSQ <- crime$burglary * crime$burglary
crime$larcenySQ <- crime$larceny * crime$larceny
crime$vehicleSQ <- crime$vehicle_theft * crime$vehicle_theft
crime$v8v9 <- crime$murder_manslaughter * crime$rape_crime
crime$v8v11 <- crime$murder_manslaughter * crime$agg_assault
crime$v8v13 <- crime$murder_manslaughter * crime$larceny
crime$v8v10 <- crime$murder_manslaughter * crime$robbery
crime$v9v11 <- crime$rape_crime * crime$agg_assault
crime$v9v14 <- crime$rape_crime * crime$vehicle_theft
crime$v10v12 <- crime$robbery * crime$burglary
crime$v11v12 <- crime$agg_assault * crime$burglary
crime$v11v13 <- crime$agg_assault * crime$larceny
crime$v12v13 <- crime$burglary * crime$larceny
crime$v12v14 <- crime$burglary * crime$vehicle_theft
crime$v10v13 <- crime$larceny * crime$robbery

#visualize / confirm changes
plot(crime)


# Models
# --- Backward ---
model_full <- lm(prisoner_count ~ state_population + murder_manslaughter + rape_crime +
                   robbery + agg_assault + burglary + larceny + vehicle_theft + 
                   includes_jails + crime_reporting_change + crimes_estimated + populationSQ +
                   manslaughterSQ + rapeSQ + robberySQ + agg_SQ + burSQ + larcenySQ +
                   vehicleSQ + v8v9 + v8v11 + v9v11 + v9v14 + v10v12 +
                   v11v12 + v11v13 + v12v13 + v12v14 + v10v13, data = crime)

step <- stepAIC(model_full, direction = "backward")
step$anov

final_model <- lm(prisoner_count ~ state_population + murder_manslaughter + rape_crime + 
                    robbery + agg_assault + burglary + vehicle_theft + includes_jails + 
                    crimes_estimated + populationSQ + manslaughterSQ + rapeSQ + 
                    robberySQ + agg_SQ + burSQ + larcenySQ + v9v11 + v9v14 + 
                    v10v12 + v11v12 + v12v13 + v10v13, data = crime)
summary(final_model)
vif(final_model)

# Check Residuals
final_model$residuals
sum(final_model$residuals)
hist(final_model$residuals, breaks = 100)

mean = mean(final_model$residuals)
sd = sd(final_model$residuals)
resid_zscore = (final_model$residuals - mean)/sd
hist(resid_zscore, breaks = 100)

durbinWatsonTest(final_model)

# Plots
plot(final_model$crime_reporting_change, resid_zscore)
plot(final_model$crimes_estimated, resid_zscore)
plot(final_model$state_population, resid_zscore)
plot(final_model$murder_manslaughter, resid_zscore)
plot(final_model$rape_crime, resid_zscore)
plot(final_model$robbery, resid_zscore)
plot(final_model$agg_assault, resid_zscore)
plot(final_model$burglary, resid_zscore)
plot(final_model$larceny, resid_zscore)
plot(final_model$vehicle_theft, resid_zscore)

plot(final_model)


# --- Forward ---
model_empty <- lm(prisoner_count ~ 1, data = crime)
summary(model_empty)

step <- stepAIC(model_empty, direction = "forward", scope = list(upper = model_full, lower = model_empty))
summary(step)

final_model.2 <- lm(prisoner_count ~ state_population + burglary + rape_crime + includes_jails + 
              robbery + murder_manslaughter + larceny + crimes_estimated + 
              agg_assault, data = crime)

summary(final_model.2)

# Check Residuals
final_model.2$residuals
sum(final_model.2$residuals)
hist(final_model.2$residuals, breaks = 100)

mean = mean(final_model.2$residuals)
sd = sd(final_model.2$residuals)
resid_zscore = (final_model.2$residuals - mean)/sd
hist(resid_zscore, breaks = 100)

durbinWatsonTest(final_model.2)

# Plots
plot(final_model.2$crime_reporting_change, resid_zscore)
plot(final_model.2$crimes_estimated, resid_zscore)
plot(final_model.2$state_population, resid_zscore)
plot(final_model.2$murder_manslaughter, resid_zscore)
plot(final_model.2$rape_crime, resid_zscore)
plot(final_model.2$robbery, resid_zscore)
plot(final_model.2$agg_assault, resid_zscore)
plot(final_model.2$burglary, resid_zscore)
plot(final_model.2$larceny, resid_zscore)
plot(final_model.2$vehicle_theft, resid_zscore)

plot(final_model.2)