#Wczytanie bibliotek
library(readr)
library(e1071)
library(moments)
require(class)
require(bnstruct)
require(DMwR)
require(VIM)
require(outliers)
require(lsr)
require(GGally)
require(broom)

###Sciezka
Path <- getwd()

##Wczytanie danych
TrainSet <- read.csv(paste(Path, "/Train.csv", sep=""))
View(TrainSet)
TestSet <- read.csv(paste(Path, "/Test.csv", sep=""))
View(TestSet)
Submission <- read.csv(paste(Path, "/Submission.csv", sep=""))
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


##### z uwagi na dużą probę przyjmujemy założenie o tym, że
# rozkłady danych z Trainset można aproksymować rozkładem 
# normalnym.



####rok powstania sklepóW a ich wielkosc####
table(Outlet_Establishment_Year) 
table(Outlet_Identifier)
table(Outlet_Size)
table(Outlet_Location_Type)
table(Outlet_Type)
table (Item_Type)

table(Outlet_Establishment_Year,Outlet_Identifier,
Outlet_Size, Outlet_Location_Type,Outlet_Type,
Item_Type)

#widzimy że najwiecej sklepów to male jednostki,
#rowniez widizmy ze najwiecej sklepow uzwytych w tym projekcie
#powstalo w 1985r



##########################################
##############################staty################
###############################################


#####dane ciągle


####podsumowanie widzialność produktu 

summary(Item_Visibility)#średnia =  0.06613 , mediana= 0.05393
IQR(Item_Visibility)#rozstęp między kwartylami wynosi  0.06759582
kurtosis(Item_Visibility)# kurtoza = 4.677757 wskazuje na wysmukły wykres
skewness(Item_Visibility)# występuje dość znaczna asymetria prawostronna
sd(Item_Visibility)


####podsumowanie MPR
summary(Item_MRP)# średnia = 140.99, mediana = 143.01
IQR(Item_MRP)# rozstęp ćwiarkowy wynosi 91.8172
skewness(Item_MRP)#skosność = 0.12718 wskazuje na to że wykres jest prawie symetryczny
kurtosis(Item_MRP)#kurtoza wskazuje na to że wykres jest spłaszczony
sd(Item_MRP)


####podsumowanie Item weight
summary(Item_Weight, na.rm=T)# mediana = 12.600, średnia = 12.858
IQR(Item_Weight, na.rm=T) # rozstęp ćwiartkowy = 8.07625
skewness(Item_Weight, na.rm = T)# skośnośc wskazujen na to że wykres jest prawie symetryczny
kurtosis(Item_Weight, na.rm = T)# wykres spłaszczony
sd(Item_Weight, na.rm=T)


####podsumowanie Item Outlet sales
summary(Item_Outlet_Sales)# medniana = 1794.33, średnia = 2181.29 
IQR(Item_Outlet_Sales)#rozstęp ćwiartkowy = 2267.049
skewness(Item_Outlet_Sales, na.rm = T)# skośność = 1.177323, asymetria prawo stronna
kurtosis(Item_Outlet_Sales, na.rm = T)# kurtoza = 4.614225, wykres wysmukły
sd(Item_Outlet_Sales)





#####################################
######wykresy,wybierzmy stąd#################
 #################################



################dane dyskretne z outlieres################

##typ produktu

TrainSet %>% ggplot(aes(x = Item_Type)) + 
  geom_histogram(stat = "count") +
  scale_x_discrete(labels = abbreviate) #gdy axis labels nachodzą na siebie


##lokalizacja

#slupkowy
TrainSet %>% select(Outlet_Location_Type) %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x=., y= Freq))+geom_bar(stat ="identity", alpha= 0.6, color= 'green') +
  labs(x = 'Tier', y = "ilość")

# wykres kolowy
TrainSet %>% ggplot(aes(x = Outlet_Location_Type, fill = factor(Outlet_Location_Type))) + 
  geom_histogram(stat="count") + 
  coord_polar(theta = "x") + 
  labs(x = "", y = "", fill = 'Lokalizacja sklepu')




##rozmiar


#wykres kołowy
table(TrainSet$Outlet_Size)
as.data.frame(table(TrainSet$Outlet_Size))->Outlet_Size_data_frame
TrainSet %>% ggplot(aes(x = factor(1), fill = factor(Outlet_Size))) + geom_bar(width = 1) + coord_polar(theta = "y")

#jeszcze jeden wykres kołowy
TrainSet %>% ggplot(aes(x = Outlet_Size, fill = factor(Outlet_Size))) + 
  geom_histogram(stat="count") + 
  coord_polar(theta = "x") + 
  labs(x = "", y = "", fill = 'rozmiar')


# poprawiony
TrainSet %>% select(Outlet_Size) %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x=. , y= Freq))+ geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'wielkosc', y = "ilość")



##identyfikator_sklepu


#wykres słupkowy

TrainSet %>% select(Outlet_Identifier) %>% table() %>% as.data.frame() %>% 
ggplot(aes(x=., y=Freq))+geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'Outlet', y = "występywanie")

#jeszcze jeden wykres kołowy
TrainSet %>% ggplot(aes(x = Outlet_Identifier, fill = factor(Outlet_Identifier))) + 
  geom_histogram(stat="count") + 
  coord_polar(theta = "x") + 
  labs(x = "", y = "", fill = 'identyfikator sklepu')


##zawartość tłuszczy

TrainSet %>% select(Item_Fat_Content) %>% table() %>% as.data.frame() %>% 
  ggplot( aes(x= . , y= Freq))+ geom_bar(stat ="identity", alpha= 0.9, color= 'green')+
  labs(x = 'tłuszcz', y = "występywanie")  

# wykres kołowy
TrainSet %>% ggplot(aes(x = Item_Fat_Content, fill = factor(Item_Fat_Content))) + 
  geom_histogram(stat="count") + 
  coord_polar(theta = "x") + 
  labs(x = "", y = "", fill = 'zawartość tłuszczu')

## powstawanie sklepów

#poprawiony
TrainSet %>% select(Outlet_Establishment_Year) %>% table() %>% as.data.frame() %>% 
  ggplot (aes(x= ., y= Freq))+
  geom_bar(stat = 'identity')+
  labs(x= "rok powstania",y = "ilość")

#wykres kołowy
TrainSet %>% ggplot(aes(x = Outlet_Establishment_Year, fill = factor(Outlet_Establishment_Year))) + 
  geom_histogram(stat="count") + 
  coord_polar(theta = "x") + 
  labs(x = "", y = "", fill = 'Typ produktu')



###############Dane ciągłe############

##widzialność produktu 

#histogram
TrainSet %>% ggplot(aes(x = Item_Visibility)) +
  geom_histogram(bins = 50, color = "blue")


##MRP

#histogram
TrainSet %>% ggplot(aes(x = Item_MRP)) +
      geom_histogram(bins = 50, color= "blue")

 
##Item_weight
 
 #histogram
 TrainSet %>% ggplot(aes(x = Item_Weight)) +
   geom_histogram(bins = 50, color = "blue")

   
##Item Outlet sales
 
 #histogram
 TrainSet %>% ggplot(aes(x = Item_Outlet_Sales)) +
      geom_histogram(bins = 50, color = "blue")

 
 
 ###boxplot #################################3
 
 #item visibility
 TrainSet %>% ggplot(aes(x= "box_plot_item_visibility",y= Item_Visibility))+geom_boxplot()
 
 #item_MRP
 TrainSet %>% ggplot(aes(x= "Item_MRP",y= Item_MRP))+geom_boxplot()
 
 #item weight
 TrainSet %>% ggplot(aes(x= "Item_Weight",y= Item_Weight))+geom_boxplot()
 
 #item outlet sales
 TrainSet %>% ggplot(aes(x= "Outlet_Sales",y= Item_Outlet_Sales))+geom_boxplot()
 
 
 
 
 

 
 ##########wykresy 2 danych###########

 
 ####rok powstania sklepóW a ich wielkosc####
 
 TrainSet %>% select(Outlet_Establishment_Year, Outlet_Size, 
  Outlet_Location_Type) %>% table() %>% as.data.frame()%>%
  ggplot(aes (x=Outlet_Establishment_Year , y = Freq))+
   geom_point(aes(color = Outlet_Size), size =3)+
   facet_grid(.~ Outlet_Location_Type)
  
 ####wykres kołowy przedstawiajacy wielkość sklepu i lokalizacje #moze ten
 TrainSet %>% ggplot(aes(x =Outlet_Location_Type  , fill = factor(Outlet_Size))) + 
   geom_histogram(stat="count") + 
   coord_polar(theta = "x") + 
   labs(x = "", y = "", fill = 'wielkość sklepu')
  
 
 ####wykres wystepowania sklepow o danym identyfiktorze w danej lokalizacji
 TrainSet %>% select(Outlet_Identifier, Outlet_Location_Type)%>% table() %>% as.data.frame()%>%
     ggplot(aes ( Outlet_Identifier, Freq))+geom_point(aes (size = 3))+ facet_grid(.~ Outlet_Location_Type)+
   scale_x_discrete(labels = abbreviate)
 
 
 
  #####wykresy wielkosci sprzedazy # warto by sie nad tym zastnowic#
  
   #histogramy 
 
 #wyrkes sprzedazy przedmoiotow wg identyfikatora sklepu
 TrainSet %>% select(Item_Outlet_Sales,Outlet_Identifier) %>%
   ggplot(aes(x= Item_Outlet_Sales, 
              fill= Outlet_Identifier))+ 
   geom_histogram(binwidth = 300)+
   facet_wrap(~Outlet_Identifier)
 
 #wykres sprzedazy wg lokalizacji sklepu
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Location_Type) %>%
   ggplot(aes(x = Item_Outlet_Sales, fill= Outlet_Location_Type))+
 geom_histogram(binwidth = 300)+
   facet_wrap(~Outlet_Location_Type)
 
 #wykres sprzedazy wg wielkosci sklepu
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Size) %>%
   ggplot(aes(x = Item_Outlet_Sales, fill= Outlet_Size))+
 geom_histogram(binwidth = 300)+
   facet_wrap(~Outlet_Size)
 
 #wykres sprzedazy wg typu sklepu
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Type) %>%
   ggplot(aes(x = Item_Outlet_Sales, fill= Outlet_Type))+
 geom_histogram(binwidth = 300)+
   facet_wrap(~Outlet_Type)
 
 #wykres sprzedazy wg roku zalozenia
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Establishment_Year) %>%
      ggplot(aes(x = Item_Outlet_Sales, fill= Outlet_Establishment_Year))+
   geom_histogram(binwidth = 300)+
    facet_wrap(~Outlet_Establishment_Year)
 
 
 ####box ploty##### te tez
 #wyrkes sprzedazy przedmoiotow wg identyfikatora sklepu
 TrainSet %>% select(Item_Outlet_Sales,Outlet_Identifier) %>%
   ggplot(aes(y= Item_Outlet_Sales, 
              x= Outlet_Identifier))+ 
   geom_boxplot()
 
 #wykres sprzedazy wg lokalizacji sklepu
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Location_Type) %>%
   ggplot(aes(y = Item_Outlet_Sales, x= Outlet_Location_Type))+
  geom_boxplot()
 
 #wykres sprzedazy wg wielkosci sklepu
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Size) %>%
   ggplot(aes(y= Item_Outlet_Sales, x= Outlet_Size))+
  geom_boxplot()
 
 #wykres sprzedazy wg typu sklepu
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Type) %>%
   ggplot(aes(y = Item_Outlet_Sales, x = Outlet_Type))+
  geom_boxplot()
 
 #wykres sprzedazy wg roku zalozenia
 TrainSet %>% select(Item_Outlet_Sales, Outlet_Establishment_Year) %>%
   ggplot(aes(y = Item_Outlet_Sales, x= Outlet_Establishment_Year))+
  geom_boxplot()+ facet_wrap( ~Outlet_Establishment_Year)
 
 
 
 ###wykresy sprzedazy i MPR pogrupowane wg kryteriów:### te wykresy by wrzucil
 
 
 #Outlet_Identifire
 TrainSet %>% select(Item_Outlet_Sales, Item_MRP, Outlet_Identifier) %>%
   ggplot(aes(x= Item_MRP, y= Item_Outlet_Sales))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Identifier)
 
 #outlet_Location_Type
 TrainSet %>% select(Item_Outlet_Sales, Item_MRP, Outlet_Location_Type) %>%
   ggplot(aes(x= Item_MRP, y= Item_Outlet_Sales))+
   geom_point()+ facet_wrap( ~ Outlet_Location_Type)
 
 #Outlet_Size
 TrainSet %>% select(Item_Outlet_Sales, Item_MRP, Outlet_Size) %>%
   ggplot(aes(x= Item_MRP, y= Item_Outlet_Sales))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Size)
 
 #outlet_estabilshment_year
 TrainSet %>% select(Item_Outlet_Sales, Item_MRP, Outlet_Establishment_Year) %>%
   ggplot(aes(x= Item_MRP, y= Item_Outlet_Sales))+
   geom_bin2d()+ facet_wrap( ~Outlet_Establishment_Year)
 
 
 ###wykresy sprzedazy i widzlanosci pogrupowane wg kryteriów:###    te wykresy bym wrzucil

 #Outlet_Identifire
 TrainSet %>% select(Item_Outlet_Sales, Item_Visibility, Outlet_Identifier) %>%
   ggplot(aes(x= Item_Visibility, y= Item_Outlet_Sales))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Identifier)
 
 #outlet_Location_Type
 TrainSet %>% select(Item_Outlet_Sales, Item_Visibility, Outlet_Location_Type) %>%
   ggplot(aes(x= Item_Visibility, y= Item_Outlet_Sales))+
   geom_point()+ facet_wrap( ~ Outlet_Location_Type)
 
 #Outlet_Size
 TrainSet %>% select(Item_Outlet_Sales, Item_Visibility, Outlet_Size) %>%
   ggplot(aes(x= Item_Visibility, y= Item_Outlet_Sales))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Size)
 
 #outlet_estabilshment_year
 TrainSet %>% select(Item_Outlet_Sales, Item_Visibility, Outlet_Establishment_Year) %>%
   ggplot(aes(x= Item_Visibility, y= Item_Outlet_Sales))+
   geom_bin2d()+ facet_wrap( ~Outlet_Establishment_Year)
 
 ###wykresy MPR i widzlanosci pogrupowane wg kryteriów:### te wykresy bym wrzucil

 
 #Outlet_Identifire
 TrainSet %>% select(Item_MRP, Item_Visibility, Outlet_Identifier) %>%
   ggplot(aes(x= Item_Visibility, y= Item_MRP))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Identifier)
 
 #outlet_Location_Type
 TrainSet %>% select(Item_MRP, Item_Visibility, Outlet_Location_Type) %>%
   ggplot(aes(x= Item_Visibility, y= Item_MRP))+
   geom_point()+ facet_wrap( ~ Outlet_Location_Type)
 
 #Outlet_Size
 TrainSet %>% select(Item_MRP, Item_Visibility, Outlet_Size) %>%
   ggplot(aes(x= Item_Visibility, y= Item_MRP))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Size)
 
 #outlet_estabilshment_year
 TrainSet %>% select(Item_MRP, Item_Visibility, Outlet_Establishment_Year) %>%
   ggplot(aes(x= Item_Visibility, y= Item_MRP))+
   geom_bin2d()+ facet_wrap( ~Outlet_Establishment_Year)
 

 
 #################################################################
 ####### #######################korelacje #############################
 
 ##dane liczbowe
 TrainSet %>% select(Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales) %>% cor %>% corrplot 
 
 
 
 
 
 
 ######################################
 ######### tansfirmacja danych######### nie wiem czy to sie przyda 
 
 ######## logrytm , sqrt   czy te transformacje sa wlasciwe ? 
 
 
 #######statystyki
 
 ######log
 
 ##item visibility
 TrainSet %>% select(Item_Visibility) %>% summary() # dane bez transformacji
 TrainSet %>% select(Item_Visibility) %>% +1 %>% log %>% summary() #dane po transformacji logarytmincznej( 1 dodana by uniknac inf )
 
 ##item_weight
 TrainSet %>% select(Item_Weight)%>% +1 %>% log %>% summary()
 TrainSet %>% select(Item_Weight)%>% +1 %>% log %>% kurtosis()
 TrainSet %>% select(Item_Weight)%>% +1 %>% log %>% skewness()
 sd(log(TrainSet$Item_Weight)+1)  
 
 ##item_MPR
 TrainSet %>% select(Item_MRP)%>% +1 %>% log %>% summary()
 TrainSet %>% select(Item_MRP)%>% +1 %>% log %>% kurtosis()
 TrainSet %>% select(Item_MRP)%>% +1 %>% log %>% skewness()
 sd(log(TrainSet$Item_MRP)+1)  
 
 ##item_outlet_sales
 TrainSet %>% select(Item_Outlet_Sales)%>% +1 %>% log %>% summary()
 TrainSet %>% select(Item_Outlet_Sales)%>% +1 %>% log %>% kurtosis()
 TrainSet %>% select(Item_Outlet_Sales)%>% +1 %>% log %>% skewness()
 sd(log(TrainSet$Item_Outlet_Sales)+1)  

 #####sqrt
 
 ##item visibility
 TrainSet %>% select(Item_Visibility) %>% summary() # dane bez transformacji
 TrainSet %>% select(Item_Visibility) %>% sqrt %>% summary() #dane po transformacji logarytmincznej( 1 dodana by uniknac inf )
 TrainSet %>% select(Item_Visibility) %>% sqrt %>% kurtosis()
 TrainSet %>% select(Item_Visibility) %>% sqrt %>% skewness()
 sd(sqrt(TrainSet$Item_Visibility))   
 
 ##item_weight
 TrainSet %>% select(Item_Weight)%>% sqrt  %>% summary()
 TrainSet %>% select(Item_Weight) %>% sqrt %>% kurtosis()
 TrainSet %>% select(Item_Weight) %>% sqrt %>% skewness()
 sd(sqrt(TrainSet$Item_Weight))
 
 ##item_MPR
 TrainSet %>% select(Item_MRP)%>% sqrt %>% summary()
 TrainSet %>% select(Item_MRP) %>% sqrt %>% kurtosis()
 TrainSet %>% select(Item_MRP) %>% sqrt %>% skewness()
 sd(sqrt(TrainSet$Item_MRP))
 
 ##item_outlet_sales
 TrainSet %>% select(Item_Outlet_Sales)%>% sqrt %>% summary()
 TrainSet %>% select(Item_Outlet_Sales) %>% sqrt %>% kurtosis()
 TrainSet %>% select(Item_Outlet_Sales) %>% sqrt %>% skewness()
 sd(sqrt(TrainSet$Item_Outlet_Sales))
 
 
 #####corr
 TrainSet %>% select( Item_Visibility, Item_MRP, Item_Weight, Item_Outlet_Sales)%>% +1 %>% log %>% cor()
 TrainSet %>% select( Item_Visibility, Item_MRP, Item_Weight, Item_Outlet_Sales) %>% sqrt %>% cor()
 
 
 ####wykresy 
 
 # item_visibility
 TrainSet %>% ggplot(aes(x = log(Item_Visibility+1)))+ geom_histogram() # wykres po transformacji log
 TrainSet %>% ggplot(aes(x= sqrt(Item_Visibility)))+ geom_histogram() # wykres po transormacji pierwiastkowej
 
 # item weight
 TrainSet %>% ggplot(aes(x = log(Item_Weight+1)))+ geom_histogram()
 TrainSet %>% ggplot(aes (x= sqrt(Item_Weight)))+ geom_histogram()
 
 # item Mpr
 TrainSet %>% ggplot(aes (x = log(Item_MRP+1)))+ geom_histogram()
 TrainSet %>% ggplot(aes (x = sqrt(Item_MRP)))+ geom_histogram()
 
 # item outlet sales
 TrainSet %>% ggplot(aes (x= log (Item_Outlet_Sales+1)))+ geom_histogram()
 TrainSet %>% ggplot(aes (x= sqrt(Item_Outlet_Sales)))+ geom_histogram() 
 
 ##wykresy MPR I sales po transf
 
 #log
 #Outlet_Identifire
 TrainSet %>%  ggplot(aes(x= log(Item_MRP)+1, y= log(Item_Outlet_Sales)+1))+
   geom_bin2d()+facet_wrap(~ Outlet_Identifier)
 
 #outlet_Location_Type
 TrainSet %>% ggplot( aes(x = log(Item_MRP)+1, y= log(Item_Outlet_Sales)+1))+ 
   geom_point()+ facet_wrap( ~ Outlet_Location_Type)
 
 #Outlet_Size
 TrainSet %>% ggplot(aes(x= log(Item_MRP)+1, y= log(Item_Outlet_Sales)+1))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Size)
 
 #outlet_estabilshment_year
 TrainSet %>%ggplot(aes(x= log(Item_MRP)+1, y= log(Item_Outlet_Sales)+1))+
   geom_bin2d()+ facet_wrap( ~Outlet_Establishment_Year)
 
 
 
 #sqrt
 #Outlet_Identifire
 
 TrainSet %>%  ggplot(aes(x= sqrt(Item_MRP), y= sqrt(Item_Outlet_Sales)))+
   geom_bin2d()+facet_wrap(~ Outlet_Identifier)
 
 #outlet_Location_Type
 TrainSet %>% ggplot( aes(x = sqrt(Item_MRP), y= sqrt(Item_Outlet_Sales)))+ 
   geom_point()+ facet_wrap( ~ Outlet_Location_Type)
 
 #Outlet_Size
 TrainSet %>% ggplot(aes(x= sqrt(Item_MRP), y= sqrt(Item_Outlet_Sales)))+
   geom_bin2d()+ facet_wrap( ~ Outlet_Size)
 
 #outlet_estabilshment_year
 TrainSet %>%ggplot(aes(x= sqrt(Item_MRP), y= sqrt(Item_Outlet_Sales)))+
   geom_bin2d()+ facet_wrap( ~Outlet_Establishment_Year)
 
 
 
 
 
 
 
 
 ###proste modele
 
 ##item_outlet_sales ~ item_visibility+item_MPR
 #linowy
 lm(Item_Outlet_Sales ~ Item_MRP+Item_Visibility)
 lm(Item_Outlet_Sales ~ Item_MRP+Item_Visibility)%>% summary()
 lm(Item_Outlet_Sales ~ Item_MRP+Item_Visibility) %>% glance()
 lm(Item_Outlet_Sales ~ Item_MRP+Item_Visibility) %>% tidy()
 lm(Item_Outlet_Sales ~ Item_MRP+Item_Visibility) %>% augment() -> fitted_lm_values
 fitted_lm_values
 
 fitted_lm_values %>% ggplot(aes(y = .resid, x = .fitted))+geom_point()+
   geom_hline(yintercept = 0 ) + geom_smooth( )
 
 #wielomianowy
 #2,3,4 stopnia
 lm(Item_Outlet_Sales ~ I(Item_MRP)+I(Item_Visibility^2)) %>% summary()
 lm(Item_Outlet_Sales ~ I(Item_MRP)+I(Item_Visibility^3)) %>% summary()
 lm(Item_Outlet_Sales ~ I(Item_MRP)+I(Item_Visibility^4)) %>% summary()
 lm(Item_Outlet_Sales ~ I(Item_MRP)+I(Item_Visibility^2)) %>% augment() -> fitted_poly_values
 
  fitted_poly_values %>% ggplot(aes(y = .resid, x = .fitted))+geom_point()+
   geom_hline(yintercept = 0 ) + geom_smooth( )
  
  #iloczyn
  lm(Item_Outlet_Sales ~ Item_MRP*Item_Visibility) %>% summary()
  lm(Item_Outlet_Sales ~ Item_MRP*Item_Visibility) %>% augment() -> fitted_mu_values
        
  fitted_mu_values %>% ggplot(aes(y = .resid, x = .fitted))+geom_point()+
    geom_hline(yintercept = 0 ) + geom_smooth( )                                                                           
                                                                               
  
   
 
 #######data without outliers #######################3 
 ##sposob z https://www.youtube.com/watch?v=6hRKlZ8D_mk
 
 ########item visibility
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
 
 
 #########item outlet sales
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
 
 
 
 #######data_without_outliers#################################
 ####Grubbs test###############
 
 require(outliers)
 library(outliers)
 
 
 grubbs.test(Item_Outlet_Sales)
 grubbs.test(Item_Visibility)
 grubbs.test(Item_Weight)
 grubbs.test(Item_MRP)
  
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

 #############outliers###################################
 
 #item_visibility
 outliers_Iv <-c(Item_Visibility[Item_Visibility>benchIVS])
 outliers_Iv
  
 outliers_Iv %>% as.data.frame()%>%
   ggplot(aes(x = .))+ 
   geom_histogram(fill= "red", col ="black")
 
 
 summary(outliers_Iv)  
 
 #item_outlet_sales      ou
 
 outliers_IOS<-c(Item_Outlet_Sales[Item_Outlet_Sales>benchIOSS])
 outliers_IOS
 
 #z uzyciem dplyr
 outliers_IOS %>% as.data.frame()%>%
   ggplot(aes(x=.))+
   geom_histogram(fill= "red", col ="black")
 
 
 summary(outliers_IOS)  
 
 #####mahalanobis############# 
 
 data.frame(Item_Visibility, Item_Outlet_Sales)->IVS.d
 MD <- mahalanobis(IVS.d, colMeans(IVS.d), cov(IVS.d))
 IVS.d$MD <- round(MD,3)
 benchMD<- 2.2423+ 1.5*IQR(MD)
 IVS.d$outlier_maha <- "F"
 IVS.d$outlier_maha[IVS.d$MD > benchMD]<-"T"
 IVS.d
 
 
 
#############################################################
############# dane dyskretne############ nie wiem co z tym, czy bedzie brane do modelu 

##chi2, wspolczynik crammera 
 
TrainSet %>% select(Outlet_Size, Item_Type) %>% table() %>% chisq.test() # korelacja
TrainSet %>% select(Outlet_Size, Item_Type) %>% table() %>% cramersV() # korelacja pomiedzy wielkoscia sklepu a typem przedmiotu

 TrainSet %>% select(Outlet_Location_Type, Outlet_Size) %>% table %>% chisq.test()
 TrainSet %>% select(Outlet_Location_Type, Outlet_Size) %>% table %>% cramersV() # korelacja miedzy wielkoscia a lokalizacja

 TrainSet %>% select(Outlet_Location_Type, Item_Type) %>% table %>% chisq.test()
 TrainSet %>% select(Outlet_Location_Type, Item_Type) %>% table %>% cramersV() # korelacje miedzy lokalizacja a typem przedmiotu

 TrainSet %>% select(Outlet_Location_Type, Outlet_Type) %>% table %>% chisq.test()
 TrainSet %>% select(Outlet_Location_Type, Outlet_Type) %>% table %>% cramersV() #korelacje miedzy lokazlizacja a typem sklepu
 
  TrainSet %>% select(Outlet_Location_Type, Outlet_Identifier) %>% table %>% chisq.test()
 TrainSet %>% select(Outlet_Location_Type, Outlet_Identifier) %>% table %>% cramersV() #korelacja miedzy lokalizacja a identyfikatorem skleu
 
 TrainSet %>% select(Outlet_Location_Type, Outlet_Establishment_Year) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Location_Type, Outlet_Establishment_Year) %>% table %>% cramersV()# cor miedzy lokalizacja a data powstania
 
 TrainSet %>% select(Outlet_Location_Type, Outlet_Identifier) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Location_Type, Outlet_Identifier) %>% table %>% cramersV()#cor pomiedzy lokalizacja a identyfikatorem
 
 TrainSet %>% select(Outlet_Location_Type, Item_Fat_Content) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Location_Type, Item_Fat_Content) %>% table %>% cramersV()
 
 TrainSet %>% select(Item_Type, Outlet_Establishment_Year) %>% table %>% chisq.out.test()
 TrainSet %>% select(Item_Type, Outlet_Establishment_Year) %>% table %>% cramersV()
 
 TrainSet %>% select(Outlet_Location_Type, Outlet_Establishment_Year) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Location_Type, Outlet_Establishment_Year) %>% table %>% cramersV() # to jest dosc ciekawe
 
 TrainSet %>% select(Outlet_Type, Item_Type) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Type, Item_Type) %>% table %>% cramersV()
 
 TrainSet %>% select(Outlet_Establishment_Year, Outlet_Size) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Establishment_Year, Outlet_Size) %>% table %>% cramersV() 
 
 TrainSet %>% select(Outlet_Establishment_Year, Outlet_Type) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Establishment_Year, Outlet_Type) %>% table %>% cramersV() # dosc spora zaleznosc
 
 TrainSet %>% select(Outlet_Size, Outlet_Type) %>% table %>% chisq.out.test()
 TrainSet %>% select(Outlet_Size, Outlet_Type) %>% table %>% cramersV()
 
 
 ## proba korelacji wielorakiej danych liczbowych
 
 TrainSet %>% select(4,6,12) %>% ggpairs()

 TrainSet %>% select(4,6,12) %>% cor %>% as.matrix() -> tablica_cor
 wsp <- c(tablica_cor[2], tablica_cor[3],tablica_cor[6])
 sqrt((wsp[1]^2+wsp[2]^2-2*wsp[1]*wsp[2]*wsp[3])/(1-(wsp[3])^2)) #korelacja miedzy wieloraka pomiedzy zmiennymi
 
 ##korelacje cząstkowe
 
 
 