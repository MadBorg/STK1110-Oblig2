# oppg 2

# twin A: at biological parents, twin B: not
# Q: is there difference in IQ

#data
A.n = 31
A.mean = 93.32
A.sd = 15.41
A.se_mean = 2.77

B.n = 31
B.mean = 96.58
B.sd = 13.84
B.se_mean = 2.49

diff.N = 31
diff.mean = -3.26
diff.sd = 8.81
diff.se_mean = 1.58

#a
# Vi burde ha en paret sammenligning siden:
  # Vi sammenligner to resulater, og ikke et nytt resultat mot et tidligere.
# Vi maa annta at:
  # X1, X2, ... , Xm er uavhenige og identisk fordelte, med forventingsverdi mu_x og varians sigma_x
  # tillsvarende for Y.
  # og vi må ha at Xi og Yi er uavhengige.

#b
#Hypotesetest: H0: A - B = mu_D ==  0, mot Ha: A - B = mu_D = 0

mu_D = A.mean - B.mean
#df
#se.x = sd.x/sqrt(m)
#se.y = sd.y/sqrt(n)

df = ( (A.se_mean)^2 + (B.se_mean)^2 )^2 / ( (((A.se_mean)^4)/(A.n-1)) + ((B.se_mean)^4/(B.n-1)) ) # frihetsgrader
t_obs = (A.mean - B.mean - 0) / sqrt( (A.sd/A.n) + (B.sd/B.n))

P.val = pt(t_obs, df) # hvis denne er regnet rett er det stor grunn til å forkaste H0

#c
alfa = 0.05
diff.Ci = diff.mean + c(1,-1) * qt(alfa/2, df) * sqrt( (A.sd/A.n) + (B.sd/A.n))
# det at diff.Ci bare dekker negative verdier forteller at vi er mer en .95 sikker på at den sanne verdien er negative, siden vi er .95 sikker på at den sanne verdien ligger innenfor vaart CI.
# En slik tosidig test, tester om vi er lengere unna, enn an størrelse beskrevet av alfa/2, mu fra H0.
# Mens en ensidig test tester om test observatoren er en sørrelse beskrevet av alfa, mindre enn mu fra H0.



