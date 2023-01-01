my_print <- function(x, y)
{
  print(paste(x, ': ', y, sep='')) 
}

library(readxl)

dane = read_excel("Zadanie_domowe_nr_2_2022_2023_KP.xlsx")
dlugosc_trasy = dane$Długość_trasy_w_milach
obciazenie_karty = dane$`Obciążenie_kart_w USD`
model = lm(formula=dlugosc_trasy~obciazenie_karty, data=dane)

print(summary(model))
plot(model)

y_mean = mean(obciazenie_karty)
SST = sum((obciazenie_karty - y_mean) ^ 2)
my_print("Całkowita suma kwadratów (SST)", SST)

SEE = sum(model$residuals ^ 2)
my_print("Suma kwadratów błędów (SEE)", SEE)

SSR = sum((model$fitted.values - y_mean) ^ 2)
my_print("Regresyjna suma kwadratów (SSR)", SSR)

R2 = SSR / SST
my_print("Współczynnik determinacji R2", R2)

n = nrow(dane)
p = 2

R2_adj = 1 - (1 - R2) * (n - 1) / (n - p)
my_print("Skorygowany współczynnik determinacji", R2_adj)

MSR = SSR / (1)
my_print("Minimalny znaczący współczynnik (MSR)", MSR)

MSE = SEE / (n - p)
my_print("Nieobciążony estymator wariancji (MSE)", MSE)

F = MSR / MSE
my_print("Statystyka F", F)

par(mfrow=c(1, 2))
plot(seq(0, 20, 0.1), df(seq(0, 20, 0.1), p-1, n-p), xlab="X", ylab="f(x)")
plot(seq(0, 20, 0.1), pf(seq(0, 20, 0.1), p-1, n-p), xlab="X", ylab="F(x)")

obszar_krytyczny = qf(0.95, p-1, n-p)
my_print("Obszar krytyczny wartości F", obszar_krytyczny)

graniczny_poziom_istotnosci = pf(F, p - 1, n - p, lower.tail=FALSE)
my_print("Graniczny poziom istotności", graniczny_poziom_istotnosci)

alfa = model$coefficients
alfa_se = sqrt(diag(vcov(model)))
t = alfa / alfa_se
my_print("Wartość t", t[2])

t_gr = qt(1 - 0.05/2, df=n-p)
my_print("Wartość graniczna t", t_gr)

p_value = 2*pt(-abs(t), df=n-p, lower.tail=TRUE)
my_print("P-value", p_value[2])


h = hatvalues(model)
print("Obserwacje odstające")
print(head(h))
plot(h)
abline(2 / n, 0)

d = cooks.distance(model)
print("Obserwacje wpływowe")
print(head(d))
plot(d)
