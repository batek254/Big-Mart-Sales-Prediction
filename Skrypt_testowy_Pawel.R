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

#Pytania:
#1. CZemu rozbijasz data frame na pojedyncze wektory? np. w linijce table(Outlet_Establishment_Year, Outlet_Size).
#Czemu tu jest samo Outlet_Establishment_Year, a nie TrainSet$Outlet_Establishment_Year? Za drugą metodą przemawiają
#dwie kwestie, po pierwsze nie muszę odpalać całego skryptu by dobrać się do danych, po drugie w danych tych masz
#również TestSet, gdzie kolumny nazywają się identycznie. Albo w linijce 40?
#2. Wariancję można liczyć z metody var() (chodzi mi o tą funkcję z 33 linijki)
#3. Do czego służy funkcja z 32linijki? Widzę, że jest użyta w 144. Nie kojarzę tego wzoru.
#Poza tym jest świetnie :D

###wykresy
######Dane ciągłe (ilościowe) w naszym secie to Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales (!ROK JEST DYSKRETNĄ!)

##typ produktu

 #funkcje
elim.deter<- function(x){((length(x)-1)/length(x))}
var.wd <- function(x){ sum((x-mean(x))^2)/length(x)}


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


#wersja poprawiona
TrainSet %>% select(Outlet_Location_Type) %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x=., y= Freq))+geom_bar(stat ="identity", alpha= 0.6, color= 'green') +
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

# poprawiony
TrainSet %>% select(Outlet_Size) %>% table() %>% as.data.frame() %>% 
 ggplot(aes(x=. , y= Freq))+ geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'wielkosc', y = "ilość")

##identyfikator_sklepu


#wykres słupkowy
as.data.frame(table(TrainSet$Outlet_Identifier))->Outlet_Identifier_data_frame
ggplot(Outlet_Identifier_data_frame, aes(x= Var1 , y= Freq))+
  geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'Outlet', y = "występywanie")

#poprawiony

TrainSet %>% select(Outlet_Identifier) %>% table() %>% as.data.frame() %>% 
ggplot(aes(x=., y=Freq))+geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'Outlet', y = "występywanie")

#podsumowanie
summary(Outlet_Identifier_data_frame)


#zawartość tłuszczów 
as.data.frame(table(Item_Fat_Content))->Item_Fat_Content_data_frame
ggplot(Item_Fat_Content_data_frame, aes(x= Item_Fat_Content , y= Freq))+ geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
labs(x = 'tłuszcz', y = "występywanie")

#poprawiony
TrainSet %>% select(Item_Fat_Content) %>% table() %>% as.data.frame() %>% 
  ggplot( aes(x= . , y= Freq))+ geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'tłuszcz', y = "występywanie")  


summary(Item_Fat_Content_data_frame)


## powstawanie sklepów
as.data.frame(table(Outlet_Establishment_Year))->Outlet_Establishment_Year_data_frame
ggplot(Outlet_Establishment_Year_data_frame, aes(x= Outlet_Establishment_Year, y= Freq))+
  geom_bar(stat = 'identity')+
  labs(x= "rok powstania",y = "ilość")

#poprawiony
TrainSet %>% select(Outlet_Establishment_Year) %>% table() %>% as.data.frame() %>% 
  ggplot (aes(x= ., y= Freq))+
  geom_bar(stat = 'identity')+
  labs(x= "rok powstania",y = "ilość")
    
#podsumowanie
summary(Outlet_Establishment_Year_data_frame)



##widzialność produktu 


#histogram
TrainSet %>% ggplot(aes(x = Item_Visibility)) +
  geom_histogram(bins = 50, color = "blue")

#podsumowanie
summary(Item_Visibility)#średnia =  0.06613 , mediana= 0.05393

IQR(Item_Visibility)#rozstęp między kwartylami wynosi  0.06759582
kurtosis(Item_Visibility)# kurtoza = 4.677757 wskazuje na wysmukły wykres
skewness(Item_Visibility)# występuje dość znaczna asymetria prawostronna
sd(Item_Visibility)
elim.deter(Item_Visibility)*sd(Item_Visibility)


##MRP


#histogram
TrainSet %>% ggplot(aes(x = Item_MRP)) +
      geom_histogram(bins = 50, color= "blue")



#podsumowanie
 summary(Item_MRP)# średnia = 140.99, mediana = 143.01
 IQR(Item_MRP)# rozstęp ćwiarkowy wynosi 91.8172
 
 skewness(Item_MRP)#skosność = 0.12718 wskazuje na to że wykres jest prawie symetryczny
 kurtosis(Item_MRP)#kurtoza wskazuje na to że wykres jest spłaszczony
 sd(Item_MRP)
 elim.deter(Item_MRP)*sd(Item_MRP)

 
 ###Item_weight
 
 #histogram
 TrainSet %>% ggplot(aes(x = Item_Weight)) +
   geom_histogram(bins = 50, color = "blue")
  
  #podsumowanie
 summary(Item_Weight, na.rm=T)# mediana = 12.600, średnia = 12.858
 IQR(Item_Weight, na.rm=T) # rozstęp ćwiartkowy = 8.07625
 skewness(Item_Weight, na.rm = T)# skośnośc wskazujen na to że wykres jest prawie symetryczny
 kurtosis(Item_Weight, na.rm = T)# wykres spłaszczony
 sd(Item_Weight)
 elim.deter(Item_Weight)*sd(Item_Weight)
 
 
 ###Item Outlet sales
 
 #histogram
 TrainSet %>% ggplot(aes(x = Item_Outlet_Sales)) +
      geom_histogram(bins = 50, color = "blue")
 #podsumowanie
 summary(Item_Outlet_Sales)# medniana = 1794.33, średnia = 2181.29 
 IQR(Item_Outlet_Sales)#rozstęp ćwiartkowy = 2267.049
 skewness(Item_Outlet_Sales, na.rm = T)# skośność = 1.177323, asymetria prawo stronna
 kurtosis(Item_Outlet_Sales, na.rm = T)# kurtoza = 4.614225, wykres wysmukły
 sd(Item_Outlet_Sales)
 elim.deter(Item_Outlet_Sales)*sd( Item_Outlet_Sales)
 
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
  
 #podsumowanie
 summary(Item_Visibility_Without_Outliers)
 sd(Item_Visibility_Without_Outliers)
 elim.deter(Item_Visibility_Without_Outliers)*sd(Item_Visibility_Without_Outliers)
 
 
 #item outlet sales
 summary(Item_Outlet_Sales)
 benchIOSS<- 3101.30 + 1.5*IQR(Item_Outlet_Sales)
 Item_Outlet_Sales[Item_Outlet_Sales< benchIOSS]-> 
   Item_OUtlet_Sales_Without_Outliers
 
 #histogram
 ggplot(as.data.frame(Item_OUtlet_Sales_Without_Outliers),
        aes(x= Item_OUtlet_Sales_Without_Outliers))+
   geom_histogram(fill= "red", col ="black")
 
 #z uzyciem dplyr
 Item_OUtlet_Sales_Without_Outliers %>% as.data.frame() %>%
   ggplot( aes (x=.))+geom_histogram(fill= "red", col ="black")
 
 length(Item_Outlet_Sales)
 length(Item_OUtlet_Sales_Without_Outliers)
 
 #podsumowanie
 summary(Item_OUtlet_Sales_Without_Outliers)
 sd(Item_OUtlet_Sales_Without_Outliers)
 elim.deter(Item_OUtlet_Sales_Without_Outliers)*sd( Item_OUtlet_Sales_Without_Outliers)
 
 #data_without_outliers
 #Grubbs test
 
 require(outliers)
 library(outliers)

 
 grubbs.test(Item_Outlet_Sales)
 grubbs.test(Item_Visibility)
 grubbs.test(Item_Weight)
 grubbs.test(Item_MRP)
 
 #outliers
#item_visibility
 outliers_Iv <-c(Item_Visibility[Item_Visibility>benchIVS])
 outliers_Iv
 
 ggplot(as.data.frame(outliers_Iv),
             aes(x= outliers_Iv))+
      geom_histogram(fill= "red", col ="black")
 
# z uzyciem dplyr 
outliers_Iv %>% as.data.frame()%>%
  ggplot(aes(x = .))+ 
  geom_histogram(fill= "red", col ="black")

    
      summary(outliers_Iv)  
  
#item_outlet_sales      ou
 
 outliers_IOS<-c(Item_Outlet_Sales[Item_Outlet_Sales>benchIOSS])
 outliers_IOS
 
 ggplot(as.data.frame(outliers_IOS),
        aes(x= outliers_IOS))+
 geom_histogram(fill= "red", col ="black")
 
 #z uzyciem dplyr
 outliers_IOS %>% as.data.frame()%>%
   ggplot(aes(x=.))+
   geom_histogram(fill= "red", col ="black")
 
 
 summary(outliers_IOS)  
 
 #mahalanobis 
 
 data.frame(Item_Visibility, Item_Outlet_Sales)->IVS.d
 MD <- mahalanobis(IVS.d, colMeans(IVS.d), cov(IVS.d))
 IVS.d$MD <- round(MD,3)
 benchMD<- 2.2423+ 1.5*IQR(MD)
 IVS.d$outlier_maha <- "F"
 IVS.d$outlier_maha[IVS.d$MD > benchMD]<-"T"
 IVS.d
 
 
 #Searching for outliers using Grubbs Test
 TrainSet %>% select(Item_Weight) %>% arrange(desc(Item_Weight))
 grubbs.test(TrainSet$Item_Weight)
 TrainSet %>% select(Item_Visibility) %>% arrange(desc(Item_Visibility))
 Grubbs_Item_Visibility <- grubbs.test(TrainSet$Item_Visibility)
 TrainSet %>% select(Item_Visibility) %>% arrange(desc(Item_Visibility)) %>% ggplot(aes(x = 1:nrow(TrainSet), y = Item_Visibility)) + geom_point()
 grubbs.test(TrainSet$Item_MRP)
 TrainSet %>% select(Item_Outlet_Sales) %>% arrange(desc(Item_Outlet_Sales))
 Gdubbs_Item_Outlet_Sales <- grubbs.test(TrainSet$Item_Outlet_Sales)
 TrainSet %>% select(Item_Outlet_Sales) %>% arrange(desc(Item_Outlet_Sales)) %>% ggplot(aes(x = 1:nrow(TrainSet), y = Item_Outlet_Sales)) + geom_point()
 
 Testowy <- TrainSet
 Grubbs_Item_Visibility_Testowy <- grubbs.test(Testowy$Item_Visibility)
 Grubbs_Item_MRP_Testowy <- grubbs.test(Testowy$Item_MRP)
 
 #wersja Alpha
 #item_visiblity
 while(Grubbs_Item_Visibility_Testowy$p.value <= 0.05){
   Testowy <- Testowy[-which.max(Testowy$Item_Visibility),]
   Grubbs_Item_Visibility_Testowy <- grubbs.test(Testowy$Item_Visibility)
 }

 
##### z uwagi na dużą probę przyjmujemy założenie o tym, że
 # rozkłady danych z Trainset można aproksymować rozkładem 
 # normalnym.
 
 
 #wykresy 2 danych
 data.frame(Outlet_Establishment_Year, Outlet_Type, Outlet_Size)->do.wykresów

 
 ####rok powstania sklepóW a ich wielkosc####
 table(Outlet_Establishment_Year, Outlet_Size)
 
 #widzimy że najwiecej sklepów to male jednostki,
 #rowniez widizmy ze najwiecej sklepow uzwytych w tym projekcie
 #powstalo w 1985r
 
 #wykres
 as.data.frame(table(Outlet_Establishment_Year, Outlet_Size, Outlet_Location_Type))->a
 qplot(Outlet_Establishment_Year, Freq, color = Outlet_Size,
       data =a, size =4, facets = .~ Outlet_Location_Type)+
   scale_x_discrete(labels = abbreviate)
 
 #poprawiony
 TrainSet %>% select(Outlet_Establishment_Year, Outlet_Size, 
  Outlet_Location_Type) %>% table() %>% as.data.frame()%>%
  ggplot(aes (x=Outlet_Establishment_Year , y = Freq))+
   geom_point(aes(color = Outlet_Size), size =3)+
   facet_grid(.~ Outlet_Location_Type)
   
   
 
 #wykres ilsoci powstalych sklepów, w danych latach, pogrupownych
 #wg lokalizacji,kolor oznacza wielkosc sklepu
 
 #
 
 
 
 
 
 