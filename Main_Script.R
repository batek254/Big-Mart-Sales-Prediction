#Wczytanie bibliotek
library(readr)

#Sciezka
Path <- getwd()

#Wczytanie danych
TrainSet <- read_csv(paste(Path, "/Train.csv", sep=""))
View(TrainSet)
TestSet <- read_csv(paste(Path, "/Test.csv", sep=""))
View(TestSet)
Submission <- read_csv(paste(Path, "/Submission.csv", sep=""))
View(Submission)