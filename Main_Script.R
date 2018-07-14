#Sprawdzanie i ładowanie bibliotek
require("readr")
require("dplyr")
require("ggplot2")
require("corrplot")
require("tidyr")
require("purrr")

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

glimpse(TrainSet)
str(TrainSet)

#Statystyki opisowe
summary(TrainSet)

#Musimy poradzić sobie najpierw z wartościami NA w danych, 
#bo jak widać wtedy wychodzą nam błędne wykresy, bo np. low fat pojawia się kilka razy
#pod różnymi nazwami
#Możemy sobie z tym poradzić na różne sposoby:
#1. Uzupełnianie średnią (dane ciągłe)
#2. Uzupełnianie wartością dominującą (dane dyskretne)
#3. Uzupełnianie na podstaiwe podobieństwa (odległości obserwacji)
#4. Ominąć te dane kiedy nie jest ich tak dużo (w naszym przypadku odpada, bo jest ich 1463 w Item_Weight)

#Zacznijmy od uzupełninia średnią
#Dane ciągłe (ilościowe) w naszym secie to Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales (!ROK JEST DYSKRETNĄ!)
#Zwrócmy uwagę, że w funkcji summary mamy policzne NA dla danych ilościowych
#Wystraczy więc zrobić to dla kolumnt Item_Weight
TrainSet %>% select(Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales) %>% is.na() %>% sum()
TrainSet$Item_Weight[which(is.na(TrainSet$Item_Weight))]

TrainSet$Item_Weight[which(is.na(TrainSet$Item_Weight))] <- mean(TrainSet$Item_Weight, na.rm = T)

TrainSet %>% select(Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales) %>% is.na() %>% sum()

#Teraz zajmiemy się danymi dyskretnymi
#Najpierw musimy poszukać NA
TrainSet %>% select(Item_Fat_Content, Item_Type, Outlet_Size, Outlet_Location_Type, Outlet_Type) %>% is.na() %>% sum()
#Mamy 2410 NAs
#Możemy dowiedzieć się gdzie
TrainSet %>% select(Item_Fat_Content) %>% is.na() %>% sum()
#Nie ma
TrainSet %>% select(Item_Type) %>% is.na() %>% sum()
#Nie ma
TrainSet %>% select(Outlet_Size) %>% is.na() %>% sum()
#Jest 2410
TrainSet %>% select(Outlet_Location_Type) %>% is.na() %>% sum()
#Nie ma
TrainSet %>% select(Outlet_Type) %>% is.na() %>% sum()
#Nie ma

#Szukanie wartości dominującej, jest bardzo proste, ale musimy się zastanowić, 
#czy w tym przypadku to nie popsuje nam danych
table(TrainSet$Outlet_Size)
#I tu mamy pierwszy problem z naszymi danymi
#Zamiana 2410 wartości w outlet_size na Medium może nam zepsuć dane
#W takim wypadku jesteśmy w kropce, bo albo zostaje nam całkowicie pominąć wiersze z NA,
#albo użyć algorytmu klasyfikacji, by przewidzieć te dane
#Wydaję mi się, że w naszym przypadku musimy pominąć, kNN to już trochę bardziej zaawansowna metoda i użycie jej
#gdy potem mamy zwykłą regresję byłoby po prostu dziwne
TrainSet <- na.omit(TrainSet)
anyNA(TrainSet)

#Następnym krokiem będzie poszukiwanie wartości odstających (outliers)

#Następnie uporządkowanie danych typu low fat
TrainSet$Item_Fat_Content[which(TrainSet$Item_Fat_Content == "LF")] <- "Low Fat"
TrainSet$Item_Fat_Content[which(TrainSet$Item_Fat_Content == "low fat")] <- "Low Fat"
TrainSet$Item_Fat_Content[which(TrainSet$Item_Fat_Content == "reg")] <- "Regular"

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

