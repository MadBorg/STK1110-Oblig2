# notation: Yi = b0 + b1*Xi + ei
# b1: slope coefficient, 
# Y: responsvariabel
# xi forklaringsvariabel.
path="https://www.uio.no/studier/emner/matnat/math/STK1110/data/plastic.txt"
plast=read.table(path,header=T)

# a
# Strenght = respons = Y
# Temperature = forklaringsvariable = Xi
Y = plast$Strength
X = plast$Temperature
X.mean = mean(X)
Y.mean = mean(Y)
X.n = length(X)
Y.n = length(Y)

b1 = (sum((X-X.mean)*(Y-Y.mean))) / sum((X-X.mean)**2)# = Sxy/Sxx
b0 = (sum(Y) - b1*sum(X))/X.n # Y.mean - b1*X.mean

SS_R = sum((Y.hat-Y.mean)^2)
SS_T = sum((Y - Y.mean)^2)
r = SS_R/SS_T

# plot
plot(X, Y, xlab="Temperature", ylab="Strength")
abline(b0, b1)


# kommentar:
    # Regresjonslinjen ser ut til å stemme. Standardfeilen er dog stor rundt Temp = 200

# b) CI for regresjonskoeffisienten for Temperature. Git intervallet indikasjon på om temperatur er en viktig forklaringsvariabel
# CI for the slope B1:
    # b1 +- t_alfa/2,n-2 * Sb1
alfa = 0.05
s = sqrt( (sum(Y^2) - b0*sum(Y) - b1*sum(X*Y)) / (n-2) )
Sxx = sum((X-X.mean)^2)
S.b1 =  s / sqrt(Sxx)
b1.CI = b1 + c(1, -1) * qt(alfa/2,X.n-2) * S.b1

t_ratio = b1 / S.b1

# t_ratio er betydelig større en max(b1.CI) så dermed forkaster vi b1.CI

# c)
s = sqrt( (sum(Y^2) - b0*sum(Y) - b1*sum(X*Y)) / (n-2) )
s_xx = sum((X - X.mean)^2)
S_Y = s * sqrt( 1/n + ( X - X.mean )^2 / (s_xx) )
Y.hat = b0 + b1*X
Y.CI.p = Y.hat + qt(0.05/2, X.n-2) * S_Y
Y.CI.m = Y.hat - qt(0.05/2, X.n-2) * S_Y

lines(X, Y.CI.m, type="b", col="red")
lines(X,Y.CI.p, type="b", col="green")

# d)
X = plast$Pressure
X.mean = mean(X)
X.n = length(X)

b1 = (sum((X-X.mean)*(Y-Y.mean))) / sum((X-X.mean)**2)# = Sxy/Sxx
b0 = (sum(Y) - b1*sum(X))/X.n # Y.mean - b1*X.mean
Y.hat = b0+b1*X
# plot
plot(X, Y, xlab="Pressure", ylab="Strength")
abline(b0, b1)



alfa = 0.05
s = sqrt( (sum(Y^2) - b0*sum(Y) - b1*sum(X*Y)) / (n-2) )
Sxx = sum((X-X.mean)^2)
S.b1 =  s / sqrt(Sxx)
b1.CI = b1 + c(1, -1) * qt(alfa/2,X.n-2) * S.b1

SS_R = sum((Y.hat-Y.mean)^2)
SS_T = sum((Y - Y.mean)^2)
r = SS_R/SS_T

