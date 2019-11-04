path="https://www.uio.no/studier/emner/matnat/math/STK1110/data/temp.txt"
data =read.table(path,header=T)


# a. boxplot og normalfordelingsplot.
boxplot(data, xlab="kjønn", ylab="C", main="Kroppstemperatur pr. kjønn")

qqnorm(data$Menn, main="qqnorm Menn")
qqline(data$Menn)
qqnorm(data$Kvinner, main="qqnorm Kvinner")
qqline(data$Kvinner)
#ser nogen lunde normalt ut. Dog men har litt større ekstremaliteter

# b. alfa = 0.05, H0: mu_menn == mu_kvinner altså del_0 = 0, Ha: mu_menn != mu_kvinner
# x:menn, x:kvinner
alfa =0.05
x = data$Menn
y = data$Kvinner
#mellomregning
mean.x = mean(x)
mean.y = mean(y)
sd.x = sd(x)
sd.y = sd(y)
m = length(x)
n = length(y)
se.x = sd.x/sqrt(m)
se.y = sd.y/sqrt(n)

df = ( (se.x)^2 + (se.y)^2 )^2 / ( (((se.x)^4)/(m-1)) + ((se.y)^4/(n-1)) )
t_obs = (mean.x - mean.y) / sqrt( (sd.x^2)/m + (sd.y^2)/n )

p.t_obs = pt(t_obs, df) # = 0.015 > alfa/2 så vi kan forkaste null hypotesen med stor margin
# todo: ser ut til av vi skal ha sd.x = sd.y, må finne mer ut om dette.

#c. F-test
f = Sd.x^2/sd.x^2
   




