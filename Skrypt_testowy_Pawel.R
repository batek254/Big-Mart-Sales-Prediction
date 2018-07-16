#Wczytanie bibliotek
library(readr)
library(e1071)
library(moments)

###Sciezka
Path <- getwd()

##Wczytanie danych
TrainSet <- read_csv(paste(Path, "/Train.csv", sep=""))
View(TrainSet)
TestSet <- read_csv(paste(Path, "/Test.csv", sep=""))
View(TestSet)
Submission <- read_csv(paste(Path, "/Submission.csv", sep=""))
View(Submission)
attach(TrainSet)
###wykresy
######Dane ciągłe (ilościowe) w naszym secie to Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales (!ROK JEST DYSKRETNĄ!)

##typ produktu


#wykres

table(TrainSet$Item_Type)
as.data.frame(table(TrainSet$Item_Type))-> Typ_produktu_data_frame
ggplot(data = Typ_produktu_data_frame, aes(x= Var1, y= Freq, group = 1))+
  geom_bar(stat= 'identity',color = 'blue')+ labs(x= "produkt", y= 'częstosc zakupu')+
  scale_x_discrete(labels = abbreviate)

###boxplot

#item visibility
 TrainSet %>% ggplot(aes(x= "box_plot_item_visibility",y= Item_Visibility))+geom_boxplot()
 
 #item_MRP
 TrainSet %>% ggplot(aes(x= "Item_MRP",y= Item_MRP))+geom_boxplot()

#item weight
 TrainSet %>% ggplot(aes(x= "Item_Weight",y= Item_Weight))+geom_boxplot()

 #item outlet sales
 TrainSet %>% ggplot(aes(x= "Outlet_Sales",y= Item_Outlet_Sales))+geom_boxplot()



#Troszkę inny sposób, histogram to jest dokładnie wykres częstości, dla danych discrete używamy stat = "count"
#wtedy sam liczy częstość występowania cechy
TrainSet %>% ggplot(aes(x = Item_Type)) + 
  geom_histogram(stat = "count") +
  scale_x_discrete(labels = abbreviate)
#gdy axis labels nachodzą na siebie

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
as.data.frame(table(TrainSet$Outlet_Size))->Outlet_Size_data_frame
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

IQR(Item_Visibility)
kurtosis(Item_Visibility)
skewness(Item_Visibility)

##MRP


#histogram
TrainSet %>% ggplot(aes(x = Item_MRP)) +
      geom_histogram(bins = 50, color= "blue")



#podsumowanie
 summary(Item_MRP)
 IQR(Item_MRP)
 
 skewness(Item_MRP)
 kurtosis(Item_MRP)
 

 
 ###Item_weight
 
 #histogram
 TrainSet %>% ggplot(aes(x = Item_Weight)) +
   geom_histogram(bins = 50, color = "blue")
  
  #podsumowanie
 summary(Item_Weight, na.rm=T)
 IQR(Item_Weight, na.rm=T)
 skewness(Item_Weight, na.rm = T)
 kurtosis(Item_Weight, na.rm = T)
 
 
 ###Item Outlet sales
 
 #histogram
 TrainSet %>% ggplot(aes(x = Item_Outlet_Sales)) +
      geom_histogram(bins = 50, color = "blue")
 #podsumowanie
 summary(Item_Outlet_Sales)
 IQR(Item_Outlet_Sales)
 skewness(Item_Outlet_Sales, na.rm = T)
 kurtosis(Item_Outlet_Sales, na.rm = T)
  
 
 #data without outliers
 ##sposob z https://www.youtube.com/watch?v=6hRKlZ8D_mk
 
 #item visibility
 summary(Item_Visibility)
 benchIVS<- 0.09459 + 1.5*IQR(Item_Visibility)
 Item_Visibility[Item_Visibility< benchIVS]-> Item_Visibility_Without_Outliers
 
 #histogram
 ggplot( as.data.frame(Item_Visibility_Without_Outliers),
         aes(x =Item_Visibility_Without_Outliers))+ 
   geom_histogram(fill='orange', col ='black')

 length(Item_Visibility)
 length(Item_Visibility_Without_Outliers)
 
 
 #item outlet sales
 summary(Item_Outlet_Sales)
 benchIOSS<- 3101.30 + 1.5*IQR(Item_Outlet_Sales)
 Item_Outlet_Sales[Item_Outlet_Sales< benchIOSS]-> 
   Item_OUtlet_Sales_Without_Outliers
 
 #histogram
 ggplot(as.data.frame(Item_OUtlet_Sales_Without_Outliers),
        aes(x= Item_OUtlet_Sales_Without_Outliers))+
   geom_histogram(fill= "red", col ="black")
 
 length(Item_Outlet_Sales)
 length(Item_OUtlet_Sales_Without_Outliers)
 