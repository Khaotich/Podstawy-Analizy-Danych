#zadanie A
t1 = pwr.anova.test(k=4, n=10, sig.level=0.05, f=0.25)
t2 = pwr.anova.test(k=4, n=10, sig.level=0.001, f=0.25)

print(t1)
print(t2)

t3 = pwr.t.test(n=20, sig.level=0.05, d=0.5)
t4 = pwr.t.test(n=20, sig.level=0.001, d=0.5)

print(t3)
print(t4)

#zadanie B
dane1 = read_excel("Zadanie_domowe_nr_5_2022_2023_KP_1.xlsx")

objawy = dane1$Objawy_choroby
dieta = dane1$Dieta
tab1 = table(objawy, dieta)
test1 = chisq.test(tab1)

print(test1)

dane2 = read_excel("Zadanie_domowe_nr_5_2022_2023_KP_2.xlsx")
przed = dane2$Przed_prezentacją
po = dane2$Po_prezentacji
tab2 = table(przed, po)

test2 = chisq.test(tab2)
test3 = fisher.test(tab2)
test4 = mcnemar.test(tab2)

print(test2)
print(test3)
print(test4)