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

# plot
plot(X, Y, xlab="Temperature", ylab="Strength")
abline(b0, b1)
# kommentar:
    # Regresjonslinjen ser ut til å stemme. Standardfeilen er dog stor rundt Temp = 200

# b) CI for regresjonskoeffisienten for Temperature. Git intervallet indikasjon på om temperatur er en viktig forklaringsvariabel
# CI for the slope B1:
    # b1 +- t_alfa/2,n-2 * Sb1
alfa = 0.05
S2 = (sum(Y^2) - b0*sum(Y) - b1*sum(X*Y)) / (X.n-1)
S = sqrt(S2)
Sxx =  sum((X- X.mean)^2)
Sb1 = S/sqrt(Sxx)

b1.CI = b1 + c(1,-1) * qt(alfa/2, X.n-2) * Sb1
t_ratio = b1 / Sb1

# t_ratio er betydelig større en max(b1.CI) så dermed forkaster vi b1.CI

# c)
Sy = S * sqrt( (1/Y.n) + (X - X.mean)^2/Sxx ) # dont know wich N to use
Y.CI.m = b0+b1*X - qt(alfa/2, Y.n-2) * Sy # same here
Y.CI.p = b0+b1*X + qt(alfa/2, Y.n-2) * Sy # same here

# PI
  # b0 + b1*x +- t(alfa/2, n-2) * S * sqrt( 1 + 1/n + (X - X.mean)/sXX )
x_star = c(210, 240, 270)
y_hat = b0 + b1 * x_star
PI <- function(b0, b1, x, x_mean, S, Sxx, n) {
    p = b0 + b1*x + qt(alfa/2, n-2) * S * sqrt(1+1/n+(x-x_mean)^2/Sxx)
    m = b0 + b1*x - qt(alfa/2, n-2) * S * sqrt(1+1/n+(x-x_mean)^2/Sxx)
    res = data.frame(p, m)
    colnames(res) <- c("p", "m")

    return(res)
}
# Y.PI.x_star.p = ( b0 + b1*x_star ) + qt(alfa/2, Y.n-2) * S * sqrt( 1 + 1/Y.n + (X - X.mean)/Sxx )
# Y.PI.x_star.m = ( b0 + b1*x_star ) - qt(alfa/2, Y.n-2) * S * sqrt( 1 + 1/Y.n + (X - X.mean)/Sxx )
Y.PI.1 = PI(b0, b1, x_star[1], X.mean, S, Sxx, Y.n)
Y.PI.2 = PI(b0, b1, x_star[2], X.mean, S, Sxx, Y.n)
Y.PI.3 = PI(b0, b1, x_star[3], X.mean, S, Sxx, Y.n)



