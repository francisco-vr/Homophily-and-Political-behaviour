##Master Script to replicate scale validation about political position ##


#Step 1: Prepare original Data frames and creates new Data frames to test

source("Pre-testing scales/Scripts/1.Processing.R")

#Step 2: Make Exploratory factorial Analysis and Confirmatory Factorial Analysis

source("Pre-testing scales/Scripts/2.FA-script.R")

#Final step: Cleaning environment

rm(list=ls())