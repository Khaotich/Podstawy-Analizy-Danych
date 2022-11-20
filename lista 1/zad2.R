#Karol Pichurski
#Lista 1 Zadanie 2(B)

#moja funkcja do czytelnego pokazywania wyników
my_print <- function(x, y)
{
  print(paste(x, ': ', y, sep='')) 
}

#importowanie bibliotek z funkcjami
library(moments)

#1

#dane
k = runif(1, 1, 29)
my_print('k', k)

#2

#część całkowita
kl = as.integer(k)
my_print('kl', kl)

#zakres funkcji
x = seq(from=-5, to=5, by=0.01)
xx = dt(x, kl)
xxlim = c(-5, 5)
yylim = c(0, 1)

#wykres gęstości rozkładu studenta
curve(dt(x, kl), xlim=xxlim, ylim=yylim)

#wykres dystrybuanty rozkładu
curve(pt(x, kl), xlim=xxlim, ylim=yylim, col='red', add=TRUE)

#3

#losowanie 100 liczb z rozkładu studenta z punktu 2
kk = sample(xx, 100)

#wykres kwantyl-kwantyl
qqnorm(kk)
qqline(kk, col='red')

#rozkład danych bardzo odbiega od rozkladu normalnego ze 
#względu na spore ogony, które możemy zauważyć na wykresie

#4

#2 liczby z rozkładu normalnego
kkk = sort(runif(2, -3, 3))
my_print('Wylosowane wartości', kkk)

#prawdopodobieństwo rozkładu studenta
p1 = pt(kkk[2], kl) - pt(kkk[1], kl)
my_print('Prawdopodobieństwo dla rozkładu studenta', p1)

#prawdopodobieństwo rozkładu normalnego
p2 = pnorm(kkk[2]) - pnorm(kkk[1])
my_print('Prawdopodobieństwo dla rozkładu normalnego', p2)

#5

#porównanie wartości poszczególnych kwantyli
my_print('Kwantyl 0.001', abs(quantile(p1, 0.001) - quantile(p2, 0.001)))
my_print('Kwantyl 0.005', abs(quantile(p1, 0.005) - quantile(p2, 0.005)))
my_print('Kwantyl 0.01', abs(quantile(p1, 0.01) - quantile(p2, 0.01)))
my_print('Kwantyl 0.05', abs(quantile(p1, 0.05) - quantile(p2, 0.05)))
my_print('Kwantyl 0.95', abs(quantile(p1, 0.95) - quantile(p2, 0.95)))
my_print('Kwantyl 0.99', abs(quantile(p1, 0.99) - quantile(p2, 0.99)))
my_print('Kwantyl 0.995', abs(quantile(p1, 0.995) - quantile(p2, 0.995)))
my_print('Kwantyl 0.999', abs(quantile(p1, 0.999) - quantile(p2, 0.999)))