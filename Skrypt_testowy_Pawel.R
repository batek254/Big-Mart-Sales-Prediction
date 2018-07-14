#Wczytanie bibliotek
library(readr)

###Sciezka
Path <- getwd()

##Wczytanie danych
TrainSet <- read_csv(paste(Path, "/Train.csv", sep=""))
View(TrainSet)
TestSet <- read_csv(paste(Path, "/Test.csv", sep=""))
View(TestSet)
Submission <- read_csv(paste(Path, "/Submission.csv", sep=""))
View(Submission)

###wykresy


##typ produktu


#wykres

table(TrainSet$Item_Type)
as.data.frame(table(TrainSet$Item_Type))-> Typ_produktu_data_frame
ggplot(data = Typ_produktu_data_frame, aes(x= Var1, y= Freq, group = 1))+
  geom_bar(stat= 'identity',color = 'blue')+ labs(x= "produkt", y= 'częstosc zakupu')

#Troszkę inny sposób, histogram to jest dokładnie wykres częstości, dla danych discrete używamy stat = "count"
#wtedy sam liczy częstość występowania cechy
TrainSet %>% ggplot(aes(x = Item_Type)) + geom_histogram(stat = "count") #+ scale_x_discrete(labels = abbreviate) gdy axis labels nachodzą na siebie

#podsumowanie
summary(Typ_produktu_data_frame)



##lokalizacja

#wykres
table(TrainSet$Outlet_Location_Type)
as.data.frame(table(TrainSet$Outlet_Location_Type))-> Outlet_Localisation_data_frame
ggplot(Outlet_Localisation_data_frame, aes(x= Var1 , y= Freq))+
  geom_bar(stat ="identity", alpha= 0.6, color= 'green') +
  labs(x = 'Tier', y = "ilość")



##rozmiar


#wykres kołowy
table(TrainSet$Outlet_Size)
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Outlet_Size))) + geom_bar(width = 1) + coord_polar(theta = "y")

#wykres słupkowy
ggplot(Outlet_Size_data_frame, aes(x= Var1 , y= Freq))+
    geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
       labs(x = 'wielkosc', y = "ilość")


##identyfikator_sklepu


#wykres słupkowy
as.data.frame(table(TrainSet$Outlet_Identifier))->Outlet_Identifier_data_frame
ggplot(Outlet_Identifier_data_frame, aes(x= Var1 , y= Freq))+
  geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'Outlet', y = "występywanie")

#podsumowanie
summary(Outlet_Identifier_data_frame)


#zawartość tłuszczów 
as.data.frame(table(Item_Fat_Content))->Item_Fat_Content_data_frame
ggplot(Item_Fat_Content_data_frame, aes(x= Item_Fat_Content , y= Freq))+ geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
labs(x = 'tłuszcz', y = "występywanie")

summary(Item_Fat_Content_data_frame)


## powstawanie sklepów
as.data.frame(table(Outlet_Establishment_Year))->Outlet_Establishment_Year_data_frame
ggplot(Outlet_Establishment_Year_data_frame, aes(x= Outlet_Establishment_Year, y= Freq))+
  geom_bar(stat = 'identity')+
  labs(x= "rok powstania",y = "ilość")

#podsumowanie
summary(Outlet_Establishment_Year_data_frame)



##widzialność produktu 


#histogram
TrainSet %>% ggplot(aes(x = Item_Visibility)) +
  geom_histogram(bins = 50, color = "blue")

#podsumowanie
summary(Item_Visibility)


##MRP


#histogram
TrainSet %>% ggplot(aes(x = Item_MRP)) +
      geom_histogram(bins = 50, color= "blue")


#podsumowanie
 summary(Item_MRP)


