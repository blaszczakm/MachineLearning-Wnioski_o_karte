### Klasyfikacja wnioskow o wydanie karty platniczej ###
#autor: Marek Błaszczak

#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("caret")
library(caret)
#install.packages("DMwR")
library(DMwR)



#opis zbioru: http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/

#wczytanie danych
dane <- read.csv("https://raw.githubusercontent.com/blaszczakm/MachineLearning-Wnioski_o_karte/master/crx.data", 
                 header = T, sep=',')

#zobaczenie nazw kolumn
colnames(dane)

#zmiana nazw kolumn
colnames(dane)<-c("Płeć","Wiek","Zadłużenie","Stan_cywilny","Bank","Wykształcenie",
                  "Pochodzenie","Lata_pracy","Poz_hist_kred","Umowa_o_prace","Score_kredytowy",
                  "Prawo_jazdy","Obywatelstwo","Saldo_konta","Przychody","Wynik")

#sprawdzenie nazw kolumn i rozmiaru danych
colnames(dane)
dim(dane)

#sprawdzenie i zmiana typow danych
head(dane,5)
str(dane)

#Zamiana danych kategorycznych na numeryczne
dane <- dane %>%
  mutate_at(
    .vars = vars("Wiek","Saldo_konta"),
    .funs = funs(as.character(.))
  )

dane <- dane %>%
  mutate_at(
    .vars = vars("Wiek","Saldo_konta"),
    .funs = funs(as.numeric(.))
  )

str(dane)

#wykrycie brakow

sapply(dane, function(x) sum(is.na(x))) #sa braki
sapply(dane, function(x) length(which(x==""))) #brak
sapply(dane, function(x) length(which(x==" "))) #brak
sapply(dane, function(x) length(which(x=="?"))) #są braki

#usuniecie brakow
#z kolumn "Plec", "Stan_cywilny", "Bank", "Wykształcenie","Pochodzenie"

dane$Płeć[which(dane$Płeć=="?")]<-NA
dane$Stan_cywilny[which(dane$Stan_cywilny=="?")] <- NA
dane$Bank[which(dane$Bank=="?")] <- NA
dane$Wykształcenie[which(dane$Wykształcenie=="?")]<-NA
dane$Pochodzenie[which(dane$Pochodzenie=="?")] <- NA

sapply(dane, function(x) sum(is.na(x))) # sa braki
sapply(dane, function(x) length(which(x=="?"))) #brak brakow

#ramka z brakami
NA_tabela <- data.frame("is.null" = sapply(dane,function(x) any(is.na(x))),
                            "num.Of.nulls" = colSums(is.na(dane)),
                            "per.of.nulls" = colMeans(is.na(dane))*100)
NA_tabela

#usuwam braki poniewaz liczba brakujacych danych jest bardzo mala,
#oraz algorytm drzewa nie jest wrazliwy na brakujace dane - wobec tego ich nie imputuje

dane <- na.omit(dane)
sapply(dane, function(x) sum(is.na(x))) #nie ma brakow

#usuniecie pustych leveli
summary(dane)
dane <- dane %>%
  mutate_at(
    .vars = vars("Płeć","Stan_cywilny","Bank","Wykształcenie","Pochodzenie"),
    .funs = funs(factor(.))
  )

summary(dane) 

#Bede korzystal z drzew decyzyjnych zatem nie musze bdac o wartosci odstajace
# oraz o normalizacje zmiennych numerycznych

#Zamiana zmiennych kategorycznych z 2 levelami na binarne
sapply(dane, function(x) levels(x))

levels(dane$Płeć)<-c(1,0)
levels(dane$Poz_hist_kred)<-c(0,1)
levels(dane$Umowa_o_prace)<-c(0,1)
levels(dane$Prawo_jazdy)<-c(0,1)
levels(dane$Wynik)<-c(0,1)

sapply(dane, function(x) levels(x))

# analiza zmiennych numerycznych
# podstawowe statystyki
num <- c("Wiek","Zadłużenie", "Lata_pracy","Score_kredytowy",
         "Saldo_konta","Przychody")

summary(dane[,num])

# wniosek1: W niektorych zmiennych widac, ze srednia wyraznie rozni sie od
# mediany (jest np. duzo wieksza) - jest to objaw skosnosci rozkladu.
# Zatem nie jest to rozklad normalny - zatem nie moge uzyc korelacji Pearsona

# Wniosek2: Poniewaz istnieje wyrazne roznice w wartosciach maksymalnych i srednich
# w poszczegolnych zmiennych to musialbym normalizowac dane. Jednak w przypadku uzycia
# modelu drzew decyzyjnych nie musze tego robic - algorytm ten nie opiera sie na 
# miarach matematycznych

# Wniosek3: W przypadku uzywania algorytmow opartych na wyliczeniach matematycznych trzeba
# usunac wartosci odstajace (o ile nie stanowia one znaczna czesc wartosci danej zmiennej).
# Jednak algorytm drzew decyzyjnych nie jest wrazliwy na wartosci odstajace - zatem pozostawiam je

#Korelcja zmiennych numerycznych ze soba
abs(cor(dane[,num], method = "spearman"))

# Z racji wyboru algorytmu drzew decyzyjnych usuwanie dwoch mocno skorelowanych 
# zmiennych numerycznych ze soba pomine (algorytm nie jest wrazliwy na wspoliniowosc zmiennych)

str(dane)

#Analiza pojedynczych zmiennych kategorycznych

plec_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Płeć, fill=Wynik))
plec_bar
table(dane$Płeć,dane$Wynik)
#Wniosek: plec nie ma wiekszego wplywu na rozpatrzenie wniosku

stan_cywilny_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Stan_cywilny, fill=Wynik))
stan_cywilny_bar
table(dane$Stan_cywilny,dane$Wynik)
#Wniosek: stan_cywilny ma wplyw na rozpatrzenie wniosku, ale nie duzy
#Stan_cywilny l mozemy traktowac jako anomalie - poniewaz zaledwie dwie obserwacje
# maja te wartosc stanu cywilnego

bank_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Bank, fill=Wynik))
bank_bar
table(dane$Bank,dane$Wynik)
#Wniosek: bank ma wplyw na rozpatrzenie wniosku (takie same dane jak 
#w przypadku stanu_cywilnego - jest zatem blad i usuwam jedna zmienna)
#bank gg mozemy traktowac jako anomalie, poniewaz zaledwie dwie obserwacje maja
# te wartosc banku

wyksztalcenie_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Wykształcenie, fill=Wynik))
wyksztalcenie_bar
table(dane$Wykształcenie,dane$Wynik)
#Wniosek: wyksztalcenie ma wplyw na rozpatrzenie wniosku, ale nie duzy

pochodzenie_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Pochodzenie, fill=Wynik))
pochodzenie_bar
table(dane$Pochodzenie,dane$Wynik)
#Wniosek: pochodzenie ma wplyw lecz nie duzy

poz_hist_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Poz_hist_kred, fill=Wynik))
poz_hist_bar
table(dane$Poz_hist_kred,dane$Wynik)
#Wniosek: Poz_hist_kred ma mocny wplyw na rozaptrzenie wniosku

umowa_o_prace_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Umowa_o_prace, fill=Wynik))
umowa_o_prace_bar
table(dane$Umowa_o_prace,dane$Wynik)
#Wniosek: Umowa_o_prace ma istotny wplyw na rozpatrzenie wniosku

prawo_jazdy_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Prawo_jazdy, fill=Wynik))
prawo_jazdy_bar
table(dane$Prawo_jazdy,dane$Wynik)
#Wniosek: Prawo jazdy nie ma wiekszego wplywu na rozpatrzenie wniosku

obywatelstwo_bar <- ggplot(dane)+
  geom_bar(aes(x=dane$Obywatelstwo, fill=Wynik))
obywatelstwo_bar
table(dane$Obywatelstwo,dane$Wynik)
#Wniosek: Obywatelstwo nie ma wiekszego wplywu na rozpatrzenie wniosku
#obywatelstwo p mozemy traktowac jako anomalie

str(dane)

#Przeksztalcenie zmiennych kategorycznych na wektory

dane <- mlr::createDummyFeatures(dane,cols = c("Bank","Stan_cywilny","Wykształcenie",
                                          "Pochodzenie","Obywatelstwo"))
#sprawdzenie ilosci zmiennych
dim(dane) # 43 zmiennych - za duzo

#Podzial na zbior treningowy i testowy
set.seed(10052019)
wiersze_tren<-createDataPartition(dane$Wynik,p=0.8,list = FALSE)
zbior_treningowy<-dane[wiersze_tren,]
zbior_testowy<-dane[-wiersze_tren,]

#zobaczenie jak dobrze podzielil sie zbior wyjsciowy
dim(zbior_testowy)
prop.table(table(zbior_testowy$Wynik))
dim(zbior_treningowy)
prop.table(table(zbior_treningowy$Wynik))
#Wnioski: zbior treningowy i testujacy posiadaja mniej wiecej 
# zbilansowana ilosc danych obserwacji dla zmiennej celu

#Budowanie modelu i szukanie odpowiednich parametrow

#model wyjsciowy - 

control <- trainControl(method="repeatedcv", number=10, repeats = 1)

#model1 - bez szukania parametrow i wszystkie zmienne

model1 <- train(Wynik~.,data = zbior_treningowy,
                method = "ctree2",
                trControl = control)

acc_tren1<-round(max(model1$results$Accuracy),3)
acc_tren1 #0.857
model1$bestTune #maxdepth=1 mincriterion=0.01

przewidywanie_model1 <- predict(model1,zbior_testowy)
l <- length(zbior_testowy$Wynik)
acc_test1 <- round(sum(przewidywanie_model1==zbior_testowy$Wynik)/l,3)
acc_test1 #0.892

#moim wyjsciowym accuracy bedzie 89.2% wszystko co bedzie wieksze badz rowne
#uznam za sukcesi zakoncze projekt

#model2 - wybor zmiennych poprzez RFE
control_rfe <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(Wynik~.,data = zbior_treningowy, rfeControl=control_rfe)

#rezultaty
plot(results, type=c("g", "o"))
results$variables

#najlepszy podzbior dla 16 zmiennych
#zmienne: Poz_hist_kred, Score_kredytowy, Lata_pracy, Przychody
# Umowa_o_prace,Saldo_konta, Wykształcenie.x, Pochodzenie.ff, 
# Wyksztalcenie.ff, Wiek, Bank.g,Stan_cywilny.y, Bank.p, Wyksztalcenie.k
# Stan_cywilny.u, Wyksztalcenie.aa

# A nie uwzgledniajac kategorii dla zmiennych wplyw na decyzje ws. wniosku maja:
# Poz_hist_kred, Score_kredytowy, Lata_pracy, Przychody, Umowa_o_prace, Saldo_konta,
# Wyksztalcenie, Pochodzenie, Wiek, Bank, Stan_cywilny
# zatem razem 11 zmiennych


model2 <- train(Wynik~Poz_hist_kred+Score_kredytowy+Przychody+Lata_pracy+
                  Umowa_o_prace+Wiek+Saldo_konta+Pochodzenie.ff+Wykształcenie.k+
                  Wykształcenie.ff+Wykształcenie.x+Wykształcenie.aa+
                  Bank.g+Bank.p+Stan_cywilny.u+Stan_cywilny.y,
                data = zbior_treningowy,
                method = "ctree2",
                trControl = control)

#zbior_treningowy
model2$coefnames

acc_tren2<-round(max(model2$results$Accuracy),3)
acc_tren2 #0.856
model2$bestTune #maxdepth=1 mincriterion=0.01

przewidywanie_model2 <- predict(model2,zbior_testowy)
l <- length(zbior_testowy$Wynik)
acc_test2 <- round(sum(przewidywanie_model2==zbior_testowy$Wynik)/l,3)
acc_test2 #0.892

#szukanie odpowiednich parametrów

#spis parametrow
modelLookup("ctree2")
grid <- data.frame(maxdepth = seq(1,20,by=1), mincriterion = seq(0.01,3,by=0.01))


model3 <- train(Wynik~Poz_hist_kred+Score_kredytowy+Przychody+Lata_pracy+
                  Umowa_o_prace+Wiek+Saldo_konta+Pochodzenie.ff+Wykształcenie.k+
                  Wykształcenie.ff+Wykształcenie.x+Wykształcenie.aa+
                  Bank.g+Bank.p+Stan_cywilny.u+Stan_cywilny.y,
                data = zbior_treningowy,
                method = "ctree2",
                trControl = control,
                tuneGrid = grid)

acc_tren3<-round(max(model3$results$Accuracy),3)
acc_tren3 #0.856
model3$bestTune #maxdepth=1 mincriterion=0.01

przewidywanie_model3 <- predict(model3,zbior_testowy)
l <- length(zbior_testowy$Wynik)
acc_test3 <- round(sum(przewidywanie_model3==zbior_testowy$Wynik)/l,3)
acc_test3 #0.892


# Wnioski
# Zmienne wpływające na pozytywne rozpatrzenie wniosku:
# Poz_hist_kred, Score_kredytowy, Lata_pracy, Przychody
# Umowa_o_prace,Saldo_konta, Wykształcenie.x, Pochodzenie.ff, 
# Wyksztalcenie.ff, Wiek, Bank.g,Stan_cywilny.y, Bank.p, Wyksztalcenie.k
# Stan_cywilny.u, Wyksztalcenie.aa

# Hiperparametry:
#   maxdepth =1, mincriterion = 0.01
