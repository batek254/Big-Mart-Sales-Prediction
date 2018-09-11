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
library(outliers)
library(gganimate)
#==================================================================================
#!!!!!!!!!!!!!!!!!!!!!!!UŻYĆ STAT_SUMMARY, GEOM_VIOLIN!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#==================================================================================

Path <- getwd()

TrainSet <- read.csv(paste(Path, "/Train.csv", sep=""))
TestSet <- read.csv(paste(Path, "/Test.csv", sep=""))
Submission <- read.csv(paste(Path, "/Submission.csv", sep=""))

TrainSet %>% select(Outlet_Size) %>% is.na() %>% sum()
table(TrainSet$Outlet_Size)

#Removing NAs by kNN imputation
knn_model <- kNN(TrainSet)
anyNA(knn_model)

#Searching for outliers using Grubbs Test
TrainSet %>% select(Item_Weight) %>% arrange(desc(Item_Weight))
grubbs.test(TrainSet$Item_Weight)
TrainSet %>% select(Item_Visibility) %>% arrange(desc(Item_Visibility))
Grubbs_Item_Visibility <- grubbs.test(TrainSet$Item_Visibility)
TrainSet %>% select(Item_Visibility) %>% arrange(desc(Item_Visibility)) %>% ggplot(aes(x = 1:nrow(TrainSet), y = Item_Visibility)) + geom_point()
grubbs.test(TrainSet$Item_MRP)
TrainSet %>% select(Item_Outlet_Sales) %>% arrange(desc(Item_Outlet_Sales))
Grubbs_Item_Outlet_Sales <- grubbs.test(TrainSet$Item_Outlet_Sales)
TrainSet %>% select(Item_Outlet_Sales) %>% arrange(desc(Item_Outlet_Sales)) %>% ggplot(aes(x = 1:nrow(TrainSet), y = Item_Outlet_Sales)) + geom_point()
grubbs.test(TrainSet$Item_Outlet_Sales)

Testowy <- TrainSet
Grubbs_Item_Visibility_Testowy <- grubbs.test(Testowy$Item_Visibility)

#Alpha version, probably could do it faster without while loop
while(Grubbs_Item_Visibility_Testowy$p.value <= 0.05){
  Testowy <- Testowy[-which.max(Testowy$Item_Visibility),]
  Grubbs_Item_Visibility_Testowy <- grubbs.test(Testowy$Item_Visibility)
}

#Ilość outliers
nrow(TrainSet) - nrow(Testowy)

#Wykresik animowany

TrainSet %>% select(Item_Outlet_Sales, Outlet_Size) %>% ggplot(aes(x = Outlet_Size, y = Item_Outlet_Sales, fill = Outlet_Size)) + geom_violin()

#Sprawdzenie danych w związku z niepewnością co do ich rodzaju
typeof(TrainSet)
summary(TrainSet)
#Zauważyłem bardzo ciekawą sprawę. Mianowicie funkcja read.csv inaczej czyta dane niż read_csv. Po funkcji summary widać,
#że bardziej pasuje nam read.csv, więc zmieniłem to w każdym skrypcie
