require("readr")
require("dplyr")
require("ggplot2")
require("corrplot")
require("tidyr")
require("purrr")
library(class)
library(bnstruct)
library(DMwR)
library(VIM)

Path <- getwd()

TrainSet <- read_csv(paste(Path, "/Train.csv", sep=""))
TestSet <- read_csv(paste(Path, "/Test.csv", sep=""))
Submission <- read_csv(paste(Path, "/Submission.csv", sep=""))

TrainSet %>% select(Outlet_Size) %>% is.na() %>% sum()
table(TrainSet$Outlet_Size)

knn_model <- kNN(TrainSet)
anyNA(knn_model)
