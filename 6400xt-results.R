
table = as.data.frame(read.csv2('orientir.csv',sep = ""))
x = as.numeric(table$PARi[as.numeric(table$CO2R)<400 & table$StableF == 0.7])
y = as.numeric(as.character(table$Photo[as.numeric(table$CO2R)<400 & table$StableF == 0.7]))
boxplot(y~x, type ="point")
plot(x,y)
double Andrey = 9
Polina +Марина Подж 6-10
Mars + Kiselev = 8-9
Oleg+Ilya = 8
