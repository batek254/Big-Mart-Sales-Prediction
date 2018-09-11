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
TrainSet <- read.csv(paste(Path, "/Train.csv", sep=""))
View(TrainSet)
TestSet <- read.csv(paste(Path, "/Test.csv", sep=""))
View(TestSet)
Submission <- read.csv(paste(Path, "/Submission.csv", sep=""))
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
#Pomijamy ten krok na ten moment

#Następnie uporządkowanie danych typu low fat
TrainSet$Item_Fat_Content[which(TrainSet$Item_Fat_Content == "LF")] <- "Low Fat"
TrainSet$Item_Fat_Content[which(TrainSet$Item_Fat_Content == "low fat")] <- "Low Fat"
TrainSet$Item_Fat_Content[which(TrainSet$Item_Fat_Content == "reg")] <- "Regular"

#Analiza opisowa (napisałaś kawał przydatnego kodu, więc gdybyś mógł, to proszę byś swój skrypt testowy uporządkował
#następująco: analiza opisowa i potem wykresy, bo jednak trochę się w tym gubię xD, a nie chcę czegoś pominąć)

TrainSet %>% 
  summary()

#Wykresy

#Diagram kołowy typów produktów 
#Możemy użyć tego wykresu
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Item_Type))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(x = "", y = "", fill = "Typ produktu")
#Lub tego
TrainSet %>% ggplot(aes(x = Item_Type, fill = factor(Item_Type))) +
  geom_histogram(stat="count") +
  coord_polar(theta = "y") +
  labs(x = "", y = "", fill = 'Typ produktu')
#Lub tego
TrainSet %>% ggplot(aes(x = Item_Type, fill = factor(Item_Type))) + 
  geom_histogram(stat="count") + 
  coord_polar(theta = "x") + 
  labs(x = "", y = "", fill = 'Typ produktu')
#Lub standardowego
TrainSet %>% ggplot(aes(x = Item_Type, fill = factor(Item_Type))) + 
  geom_histogram(stat="count") +
  labs(x = "Typ produktu", y = "Częstość", fill = "")
#Można to ładnie posortować jeszcze, ale nie wiem, czy jest sens. Wtedy mamy:
TrainSet %>% select(Item_Type) %>% table() %>% as.data.frame() %>%
  ggplot(aes(x = reorder(., Freq), y = Freq, fill = factor(.))) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") + 
  labs(x = "", y = "", fill = 'Typ produktu')

#Diagram kołowy zawartosci tluszczow
table(TrainSet$Item_Fat_Content)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Item_Fat_Content))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(x = "", y = "", fill = "Zawartość tłuszczu")

#Diagram kołowy lokalizacji sklepów
table(TrainSet$Outlet_Location_Type)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Outlet_Location_Type))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(x = "", y = "", fill = "Typ lokalizacji")

#Diagram kołowy typu sklepów
table(TrainSet$Outlet_Type)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Outlet_Type))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(x = "", y = "", fill = "Typ sklepu")

#Wykres gęstości wagi przedmiotu
TrainSet %>% ggplot(aes(x = Item_Weight)) + geom_density()

#Wykres korelacji
#Musimy wybrać tylko zmienne ciągłe 
TrainSet %>% 
  select(Item_Weight, Item_Visibility, Item_MRP) %>% 
  cor()

#Praktycznie zerowa korelacja między zmiennymi, co nie przez nas pożądane.

#Bardzo ciekawy rodzaj wykresu violin (coś jak boxplot), aktulanie jest bardzo popularny. Proponowałbym by go użyć.
#Wykres violin, na osi x Item_Type, na y Item_Weight, a gridy to Item_Fat_Content.
TrainSet %>% 
  ggplot(aes(x = Item_Type, y = Item_Weight, fill = Item_Type)) + 
  geom_violin() +
  facet_grid(rows = vars(Item_Fat_Content)) +
  guides(fill = F) +
  scale_x_discrete(labels = abbreviate)

#Wykres violin, na osi x Item_Type, na y Item_Visibility, a gridy to Item_Fat_Content.
TrainSet %>% 
  ggplot(aes(x = Item_Type, y = Item_Visibility, fill = Item_Type)) + 
  geom_violin() +
  facet_grid(rows = vars(Item_Fat_Content)) +
  guides(fill = F) +
  scale_x_discrete(labels = abbreviate)

#Wykres violin, na osi x Item_Type, na y Item_MRP, a gridy to Item_Fat_Content.
TrainSet %>% 
  ggplot(aes(x = Item_Type, y = Item_MRP, fill = Item_Type)) + 
  geom_violin() +
  facet_grid(rows = vars(Item_Fat_Content)) +
  guides(fill = F) +
  scale_x_discrete(labels = abbreviate)

#TUTAJ WRZUCI SIĘ WIĘCEJ VIOLIN