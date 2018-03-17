# setwd("~/systislabra_sahko")

# Luetaan s?hk?nkulutus- ja l?mp?tiladata, hyp?t??n headerrivin yli
eletemp = read.table(file = "sahko.csv",
                     sep = ";",
                     dec = ",",
                     skip = 1,
                     col.names = c('kWh','Celcius'))

# S?hk?nkulutus aikasarjaksi
ele = ts(eletemp$kWh[1:816], start = 1, frequency = 24)

# L?mp?tila kahdeksi aikasarjaksi: 816 ensimm?ist? havaintoa k?ytet??n mallin estimointiin
# ja 24 viimeist? havaintoa ennustamiseen.
temp = ts(eletemp$Celcius, start = 1, frequency = 24)
temp816 = ts(eletemp$Celcius[1:816], start = 1, frequency = 24)
# start parametrina vektori: 817. tunti = 35. p?iv?n ensimm?inen tunti
temp24 = ts(eletemp$Celcius[817:840], start = c(35,1), frequency = 24)

# Plotataan aikasarjat
ts.plot(ele,
        xlab = "aika/vrk",
        ylab = "kulutus/kWh")
ts.plot(temp816,temp24,
        xlab = "aika/vrk",
        ylab = expression(~degree~C),
        col = c("black", "blue"))

# M??ritell??n 2x2 plottausruudukko.
par(mfrow=c(2,2))
# Jos haluat katsoa kuvia 2x2 matriisin sijaan yksitellen, niin
# par(mfrow=c(1,1))

# Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(ele, lag.max=168)
acf(ele, lag.max=168, type = "partial")
acf(temp, lag.max=168)
acf(temp, lag.max=168, type = "partial")
# Piirret??n ristikorrelaatiofunktio omaan kuvaan
par(mfrow=c(1,1))
ccf(ele,temp, lag.max=168)

# Mahdolliset kaudet aikasarjassa
L1 = 168
L2 = 24

# Differointien lukum√§√§r√§, kauden pituus
d = 1
S = L2
D = 1

dele168 <- diff(ele, lag = L1, differences = 1)
dtemp168 <- diff(temp, lag = L1, differences = 1)

ts.plot(dele168,
        xlab = "aika/vrk",
        ylab = "kulutus/kWh")

ts.plot(dtemp168,
        xlab = "aika/vrk",
        ylab = expression(~degree~C),
        col = c("black", "blue"))

par(mfrow=c(2,2))

# Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(dele168, lag.max=168)
acf(dele168, lag.max=168, type = "partial")
acf(dtemp168, lag.max=168)
acf(dtemp168, lag.max=168, type = "partial")
# Piirret??n ristikorrelaatiofunktio omaan kuvaan
par(mfrow=c(1,1))
ccf(dele168, dtemp168, lag.max=168)

dele <- dele168
dtemp <- dtemp168

if (d > 0) {
  dtemp <- diff(dtemp, lag = 1, differences = d)
  dele <- diff(dele, lag = 1, differences = d)
}
if (D > 0) {
  dtemp <- diff(dtemp, lag = L2, differences = D)
  dele <- diff(dele, lag = L2, differences = D)
}
ts.plot(dele,
        xlab = "aika/vrk",
        ylab = "kulutus/kWh")

ts.plot(dtemp,
        xlab = "aika/vrk",
        ylab = expression(~degree~C),
        col = c("black", "blue"))

par(mfrow=c(2,2))

# Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(dele, lag.max=168)
acf(dele, lag.max=168, type = "partial")
acf(dtemp, lag.max=168)
acf(dtemp, lag.max=168, type = "partial")
# Piirret??n ristikorrelaatiofunktio omaan kuvaan
par(mfrow=c(1,1))
ccf(dele,dtemp, lag.max=168)

# Estimoidaan malli ja lasketaan ennusteet ilman ulkoista muuttujaa.
p = 5
q = 1
P = 1
Q = 1

#l‰mpˆtilan mahdollinen viive
L = 0

# TODO: differentoi
tempestimointi = eletemp$Celcius[1:(816-L)]
tempennuste = eletemp$Celcius[(816-L+1):(816-L+24)]
eleestimointi = ts(eletemp$kWh[(1+L):816], start = 1, frequency = 24)
malli = arima(eleestimointi,
               order = c(p,d,q),
               seasonal = list(order = c(P, D, Q), period = S),
               xreg = tempestimointi,
               method = "CSS")
enne = predict(malli2, 
               n.ahead = 24,
               newxreg = tempennuste)



