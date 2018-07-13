#Wczytanie bibliotek
library(readr)

#Wczytanie danych
TrainSet <- read_csv("E:/Programowanie/R Programy/Big-Mart-Sales-Prediction/Train.csv")
View(TrainSet)
TestSet <- read_csv("E:/Programowanie/R Programy/Big-Mart-Sales-Prediction/Test.csv")
View(TestSet)
Submission <- read_csv("E:/Programowanie/R Programy/Big-Mart-Sales-Prediction/Submission.csv")
View(Submission)