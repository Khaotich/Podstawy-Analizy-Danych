#Karol Pichurski
#Lista 1 Zadanie 1(A)

#moja funkcja do czytelnego pokazywania wyników
my_print <- function(x, y)
{
    print(paste(x, ': ', y, sep='')) 
}

#importowanie bibliotek z funkcjami
library(readxl)
library(moments)

#dane
dane = read_excel('Zad_domowe_nr_1_2022-2023_KP.xlsx')
masa = dane$Masa_ptaka

#A

#średnia
my_print("Średnia", mean(masa))

#mediana
my_print("Mediana", median(masa))

#wariancja
my_print("Wariancja", var(masa))

#odchylenie standardowe
my_print("Odchylenie standardowe", sd(masa))

#kwartyle
my_print("Kwartyl dolny", quantile(masa, 0.25))
my_print("Kwartyl górny", quantile(masa, 0.75))

#odstęp międzykwartylowy
my_print("Odstęp międzykwartylowy", IQR(masa))

#odchylenie ćwiartkowe
my_print("Odchylenie ćwiartkowe", IQR(masa) / 2)

#współczynnik skośności
my_print("Współczynnik skośności", skewness(masa))

#kurtoza
my_print("Kurtoza", kurtosis(masa))

# użyteczne dane to średnia, mediana i kwartyle

#B

#histogram
hist(masa, main='Histogram rozkładu masy ptaków', xlab='Masa', ylab='Liczebność')

#C

#wykres ramka-wąsy
boxplot(masa)

#na wykresie ramka-wąsy nie zauważyłem odstających obserwacji

#D

#wykres kwantyl kwantyl
qqnorm(masa)
qqline(masa, col='red')

#na wykresie możemy zauważyć spore ogony a więc cecha nie ma
#rozkłau normalnego i ma odstajęce obserwacje
