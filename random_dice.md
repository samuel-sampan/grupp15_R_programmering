1.2 sum_of_random_dice()
Funktionens sum_of_dice(), från Datorlaboration 5, avsnitt 2.3, summerar värdet från ett fixt antal
tärningar, nu ska ni skriva en funktion som kan summera ett slumpmässigt antal tärningar. Tärningarna
ni ska summera är vanliga 6-sidiga tärningar med värden 1 till 6. Funktionen ska heta sum_of_random_dice()
med argumenten:
• K: ett positivt heltal. Styr antal dragningar från er fördelning.
4
• lambda: ett positivt kontinuerligt tal. Styr fördelningen för antal tärningar.
• my_seed: en seed som styr slumptalsgeneratorn. Ska ha defaultvärdet NULL.
Funktionen ska sedan göra följande:
• Om my_seed är satt så ska set.seed() anropas med denna seed.
• Skapa en tom matris som ska innehålla resultaten, det ska vara K rader och 2 kolumner. Kolumnerna
ska ha namnen Value och Dice i den ordningen.
• Ni ska sedan loopa från 1 till K och för varje iteration:
– Dra ett slumptal från en poissonfördelningen med parameterna lambda. Detta är antal
tärningar ni ska summera. Spara värdet i klumnen Dice.
– Anropa sum_of_dice() med K=1 och N=värdet ni fick från Poissonfördelningen. Spara resultatet i kolumnen Value. Observera att om N=0 ska värdet 0 sparas.
• Returnera matrisen med resultat.
# test 1:
sum_of_random_dice(K = 5, lambda = 3, my_seed = 123)
value dice
1 9 2
2 22 6
3 9 3
4 16 5
5 9 3
# test 2:
sum_of_random_dice(K = 5, lambda = 8, my_seed = 543)
value dice
1 49 12
2 30 7
3 26 7
4 42 12
5 32 9
# test 3:
x <- sum_of_random_dice(K = 300, lambda = 5, my_seed = 387)
# Ritar ett histogram över resultaten av summorna.
hist(x[,2])
5
Histogram of x[, 2]
x[, 2]
Frequency
0 2 4 6 8 10 12
0 10 20 30 40 50
# test 4:
y <- sum_of_random_dice(K = 100, lambda = 10, my_seed = 723)
# Beräknar medelvärde och standardavvikelse över resultaten.
mean(y[,2])
[1] 9.71
sd(y[,2])
[1] 2.879
# test 5:
z <- sum_of_random_dice(K = 30, lambda = 0.4, my_seed = 395)
# korstabell över antalet tärningar och summan.
table(z[,1],z[,2])
0 1 2 3 4
0 18 0 0 0 0
2 0 2 0 0 0
6
4 0 3 0 0 0
5 0 3 0 0 0
6 0 1 0 0 0
9 0 0 0 0 1
11 0 0 1 0 0
15 0 0 0 1 0
