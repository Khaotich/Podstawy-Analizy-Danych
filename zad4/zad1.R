my_print <- function(x, y)
{
  print(paste(x, ': ', y, sep='')) 
}

library(readxl)
library(car)
library(DescTools)

dane = read_excel("Zadanie_domowe_nr_4_2022_2023_KP.xls")
gat1 = na.omit(dane$Gatunek_1)
gat2 = na.omit(dane$Gatunek_2)
gat3 = na.omit(dane$Gatunek_3)
gat11 = dane$Gatunek_1
gat21 = dane$Gatunek_2
gat31 = dane$Gatunek_3

#średnia
my_print("Średnia gatunek 1", mean(gat1))
my_print("Średnia gatunek 2", mean(gat2))
my_print("Średnia gatunek 3", mean(gat3))

#mediana
my_print("Mediana gatunek 1", median(gat1))
my_print("Mediana gatunek 2", median(gat2))
my_print("Mediana gatunek 3", median(gat3))

#wariancja
my_print("Wariancja gatunek 1", var(gat1))
my_print("Wariancja gatunek 2", var(gat2))
my_print("Wariancja gatunek 3", var(gat3))

#odchylenie standardowe
my_print("Odchylenie standardowe gatunek 1", sd(gat1))
my_print("Odchylenie standardowe gatunek 2", sd(gat2))
my_print("Odchylenie standardowe gatunek 3", sd(gat3))

#kwartyle
my_print("Kwartyl dolny gatunek 1", quantile(gat1, 0.25))
my_print("Kwartyl górny gatunek 1", quantile(gat1, 0.75))
my_print("Kwartyl dolny gatunek 2", quantile(gat2, 0.25))
my_print("Kwartyl górny gatunek 2", quantile(gat2, 0.75))
my_print("Kwartyl dolny gatunek 3", quantile(gat3, 0.25))
my_print("Kwartyl górny gatunek 3", quantile(gat3, 0.75))

#odstęp międzykwartylowy
my_print("Odstęp międzykwartylowy gatunek 1", IQR(gat1))
my_print("Odstęp międzykwartylowy gatunek 2", IQR(gat2))
my_print("Odstęp międzykwartylowy gatunek 3", IQR(gat3))

#odchylenie ćwiartkowe
my_print("Odchylenie ćwiartkowe gatunek 1", IQR(gat1) / 2)
my_print("Odchylenie ćwiartkowe gatunek 2", IQR(gat2) / 2)
my_print("Odchylenie ćwiartkowe gatunek 3", IQR(gat3) / 2)


frame = data.frame(gat11, gat21, gat31)
stack = stack(frame)

anov = aov(values~ind, data=stack)

print(summary(anov))
plot(anov)

qqPlot(lm(values~ind, data=stack))

print(bartlett.test(values~ind, data=stack))
print(kruskal.test(values~ind, data=stack))

print(PostHocTest(anov, method="lsd"))
print(PostHocTest(anov, method="hsd"))
print(PostHocTest(anov, method="scheffe"))
