#Keep in mind - all the program needs to do is spit out the regression equation.  The equation
  #will then be added to the excel file to auto calculate for whatever area 
  #& drive time we input data for. 
  #If residual mean != 0, the forecast is biased. You'll need to add the mean to all forecasts
  #to improve accuracy.

###########################################################################
library(tidyverse) #load tidyverse, if needed
Dental.Model <- read.csv("Demographic Data by Zip - Dental Model.csv")

#Declare variables
div <- 90 #raw dataset row number where you will separate training data from testing data
r <- .988 #correlation coefficient cut off for variables that will be used in the model
raw_data <- Dental.Model #dataset used to generate model
response_var_columnnum <- 15 #column number of response variable in raw dataset
total_rows <- 289 #number of rows in raw dataset

############################################################################

#Create training data frame with only numeric data
training_data <- raw_data[1:div,-1]

#Add 'Demand' column and assign 'Dental.Services' values
training_data$Demand <- training_data[,(response_var_columnnum-1)]

#Remove 'Dental.Services' column
training_data[,(response_var_columnnum-1)] <- NULL

#Create correlation matrix and select high correlation variables from training data
correlation_matrix <- cor(training_data, training_data$Demand)
high_correlations <- correlation_matrix > r
predictors <- training_data[,high_correlations]

#Print variable names
names(predictors)

#Rename predictors variables to generic----------Need to create a loop
names(predictors) <- c("X1", "X2", "X3", "Y1")

#Create scatter plots for explanatory variables
ggplot(data = predictors) +
  geom_point(mapping = aes(x = X1, y = Y1), color = "dark blue")

ggplot(data = predictors) +
  geom_point(mapping = aes(x = X2, y = Y1), color = "red")

ggplot(data = predictors) +
  geom_point(mapping = aes(x = X3, y = Y1), color = "dark green")

#Create regression equation
regression <- lm(Y1 ~ X1 + X2 + X3, data = predictors)
regression

###############################################################################
#Run the analysis with test data

#Create training data frame with only numeric data
test_data <- raw_data[(div+1):total_rows,-1]

#Add 'Demand' column and assign 'Dental.Services' values
test_data$Demand <- test_data[,(response_var_columnnum-1)]

#Remove 'Dental.Services' column
test_data[,(response_var_columnnum-1)] <- NULL

#Filter down to predictor variables
predictors2 <- test_data[,high_correlations]

#Rename predictors variables to generic----------Need to create a loop
names(predictors2) <- c("X1", "X2", "X3", "Y1")

#Create scatter plots for explanatory variables
ggplot(data = predictors2) +
  geom_point(mapping = aes(x = X1, y = Y1), color = "dark blue")
ggsave("ScatterTest.pdf")

ggplot(data = predictors2) +
  geom_point(mapping = aes(x = X2, y = Y1), color = "red")

ggplot(data = predictors2) +
  geom_point(mapping = aes(x = X3, y = Y1), color = "dark green")

#Add Prediction column
transformed_predictors <- mutate(predictors2, X1 = X1*regression$coefficients[2], 
                                 X2 = X2*regression$coefficients[3], 
                                 X3 = X3*regression$coefficients[4])
transformed_predictors2 <- mutate(transformed_predictors, 
                                  P1 = regression$coefficients[1] + X1 + X2 + X3)

#Add error column and remove inf values
predictor_results <- mutate(transformed_predictors2, E1 = ((Y1 - P1)/Y1)*100)
good_rows <- predictor_results[,"E1"] != Inf
predictor_results2 <- predictor_results[good_rows,]

#Add absolute error and residuals columns
predictor_results3 <- mutate(predictor_results2, E2 = abs(E1))
predictor_results4 <- mutate(predictor_results3, Residuals1 = Y1 - P1)

###############################################################################
#Print results
summary(regression)

#Residual Mean
mean(predictor_results4[,"Residuals1"]) 

#Residual Std Dev
sd(predictor_results4[,"Residuals1"])

#Residual Max
max(predictor_results4[,"Residuals1"]) 

#Mean Absolute Percentage Error (MAPE)
mean(predictor_results3[,"E2"]) 

#Absolute Percentage Error Standard Deviation
sd(predictor_results3[,"E2"]) 

#Absolute Percentage Error Max
max <- max(predictor_results3[,"E2"]) 
max

timeseries_X1 <- ggplot(data = predictor_results4) +
  geom_line(mapping = aes(x = X1, y = Residuals1), color = "red")
timeseries_X1

timeseries_X2 <- ggplot(data = predictor_results4) +
  geom_line(mapping = aes(x = X2, y = Residuals1), color = "dark green")
timeseries_X2

timeseries_X3 <- ggplot(data = predictor_results4) +
  geom_line(mapping = aes(x = X3, y = Residuals1), color = "dark blue")
timeseries_X3

hist <- ggplot(data = predictor_results3) + 
  geom_histogram(mapping = aes(x = E2), binwidth = 2.5, color = "black", fill = "red")
  #coord_cartesian(xlim = c(0,max), ylim = c(0,50)) + 
  #scale_x_continuous(breaks = seq(0, max, 5)) + 
  #scale_y_continuous(breaks = seq(0, 50, 5))
hist

hist2 <- ggplot(data = predictor_results3) + 
  geom_histogram(mapping = aes(x = E1), binwidth = 2.5, color = "black", fill = "dark green")
  #coord_cartesian(xlim = c(-max,max), ylim = c(0,50)) + 
  #scale_x_continuous(breaks = seq(-max, max, 5)) + 
  #scale_y_continuous(breaks = seq(0, 50, 5))
hist2

residualplot <- ggplot(data = predictor_results4) +
  geom_point(mapping = aes(x = P1, y = Residuals1), color = "dark blue")
residualplot

residualhist <- ggplot(data = predictor_results4) +
  geom_histogram(mapping = aes(x = Residuals1), color = "black", fill = "light blue")
residualhist

ggsave("Optima_Model_v3.pdf")