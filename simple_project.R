##########################################################################################################
# A simple project in R to load some data and analyze and visualize it
# adapted from:
#    https://medium.com/analytics-vidhya/a-simple-ml-project-in-r-using-the-boston-dataset-e1143146ffb0
##########################################################################################################

##################
# load libraries
##################
library(mlbench)  # Contains the Boston Housing Dataset
# library(dplyr)    # Basic manipulation functions
library(ggplot2)  # Graphs and plots
# library(reshape2) # To reshape data
library(caret)    # Creating train test sets


##################
# load data
##################

# Load the Boston Housing dataset in the object named 
# 'BostonHousing'
data(“BostonHousing”)# For simplicity, lets call it 'housing'
housing = BostonHousing


####################
#
####################

# The str() function gives the details about the types of variables(attributes). Its necessary that you understand the meaning of these attributes. You can get more details on the meaning of the attributes here.
str(housing)

####################
# Plotting
####################

# TODO: Plotting

# TODO: Density plot of Median Value of house prices


############################
# Creating train-test sets
############################

set.seed(387)
train.idx = createDataPartition(y = housing$medv, p = 0.75, list = FALSE)
test.idx =createDataPartition(y=housing$medv, p=0.25,list=FALSE)
train = housing[train.idx, ]
test = housing[test.idx, ]

# The createDataPartition() returns random index numbers to select for the train and test set. The rows corresponding to these indexes are used to obtain the ‘train’ and ‘test’ set.

######################
# Building the model
######################

model = lm( medv ~ crim + rm + tax + lstat, data = train)

# The lm() function creates a linear model. In the first argument, variables on left of ‘~’ is the target variable and on the right are independent variables.


summary(model) # Obtain coefficients, Residuals and statistics
rsquare = summary(model)$r.squared # R-squared value

