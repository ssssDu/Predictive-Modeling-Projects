#The project aims to predict car price given multiple variables

First, import packages and dataset
library(lattice)
library(caret)
library(ggplot2)
attach(Cars)
library(glmnet)
library(tree)
library(pls)
library(randomForest)
library(gbm)
library(readr)
Cars <- read_csv("~/Downloads/Cars.csv")
