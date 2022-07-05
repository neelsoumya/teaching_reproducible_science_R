##########################################################################################################
# A simple project in R to load some data and analyze and visualize it
# adapted from:
#    https://medium.com/analytics-vidhya/a-simple-ml-project-in-r-using-the-boston-dataset-e1143146ffb0
##########################################################################################################

##################
# load libraries
##################
library(mlbench)  # Contains the Boston Housing Dataset
library(dplyr)    # Basic manipulation functions
library(ggplot2)  # Graphs and plots
library(reshape2) # To reshape data
library(caret)    # Creating train test sets


##################
# load data
##################

# Load the Boston Housing dataset in the object named 
# 'BostonHousing'
data(“BostonHousing”)# For simplicity, lets call it 'housing'
housing = BostonHousing

