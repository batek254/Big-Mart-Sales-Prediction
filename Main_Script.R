#Sprawdzanie i ładowanie bibliotek
require("readr")
require("dplyr")
require("ggplot2")
require("corrplot")
require("tidyr")

#=============================================
#UWAGA INTERESUJE NAS TYLKO TRAINSET
#=============================================

#Sciezka
Path <- getwd()

#Wczytanie danych
TrainSet <- read_csv(paste(Path, "/Train.csv", sep=""))
View(TrainSet)
TestSet <- read_csv(paste(Path, "/Test.csv", sep=""))
View(TestSet)
Submission <- read_csv(paste(Path, "/Submission.csv", sep=""))
View(Submission)

#Statystyki opisowe
summary(TrainSet)

#Wykresy

#Diagram kołowy typów produktów 
table(TrainSet$Item_Type)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Item_Type))) + geom_bar(width = 1) + coord_polar(theta = "y")

#Diagram kołowy zawartosci tluszczow - widac dane do poprawy
table(TrainSet$Item_Fat_Content)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Item_Fat_Content))) + geom_bar(width = 1) + coord_polar(theta = "y")

#Diagram kołowy lokalizacji sklepów
table(TrainSet$Outlet_Location_Type)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Outlet_Location_Type))) + geom_bar(width = 1) + coord_polar(theta = "y")

#Diagram kołowy typu sklepów
table(TrainSet$Outlet_Type)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Outlet_Type))) + geom_bar(width = 1) + coord_polar(theta = "y")

#Wykres gęstości wagi przedmiotu
TrainSet %>% ggplot(aes(x = Item_Weight)) + geom_density()
