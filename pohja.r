# CTRL+Shift+C kommentoi tai poistaa kommentit valituilta riveiltä

# Asetetaan työhakemisto SET Working Directory
# Muokkaa tarvittaessa oman työhakemistosi polku
# RStudiossa tämän voi tehdä myös valikosta Session->Set working directory
# setwd('Z:/Documents')
setwd("~/systislabra_sahko")

# Luetaan sähkönkulutus- ja lämpötiladata, hypätään headerrivin yli
eletemp = read.table(file = "sahko.csv",
                     sep = ";",
                     dec = ",",
                     skip = 1,
                     col.names = c('kWh','Celcius'))

# Sähkönkulutus aikasarjaksi
ele = ts(eletemp$kWh[1:816], start = 1, frequency = 24)

# Lämpötila kahdeksi aikasarjaksi: 816 ensimmäistä havaintoa käytetään mallin estimointiin
# ja 24 viimeistä havaintoa ennustamiseen.
temp = ts(eletemp$Celcius, start = 1, frequency = 24)
temp816 = ts(eletemp$Celcius[1:816], start = 1, frequency = 24)
# start parametrina vektori: 817. tunti = 35. päivän ensimmäinen tunti
temp24 = ts(eletemp$Celcius[817:840], start = c(35,1), frequency = 24)

# Plotataan aikasarjat
ts.plot(ele,
     xlab = "aika/vrk",
     ylab = "kulutus/kWh")
ts.plot(temp816,temp24,
        xlab = "aika/vrk",
        ylab = expression(~degree~C),
        col = c("black", "blue"))

# Määritellään 2x2 plottausruudukko.
par(mfrow=c(2,2))
# Jos haluat katsoa kuvia 2x2 matriisin sijaan yksitellen, niin
# par(mfrow=c(1,1))

# Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(ele, lag.max=168)
acf(ele, lag.max=168, type = "partial")
acf(temp, lag.max=168)
acf(temp, lag.max=168, type = "partial")
# Piirretään ristikorrelaatiofunktio omaan kuvaan
par(mfrow=c(1,1))
ccf(ele,temp, lag.max=168)

# Stationarisoidaan aikasarjat. Määrittele parametrit d,S,D
# Huomaa, että sähkönkulutuksen ja lämpötilan aikasarjojen differointien asteiden ei välttämättä tarvitse olla samoja.
d = 0 # Differoinnin kertaluku d
S = 0 # Kausidifferoinnin jakso S
D = 0 # Kausidifferensoinnin kertaluku D
dele = ele
dtemp = temp
if (d > 0) {
  dele = diff(dele, lag = 1, differences = d)
  dtemp = diff(dtemp, lag = 1, differences = d)
}
if (D > 0) {
  dele = diff(dele, lag = S, differences = D)
  dtemp = diff(dtemp, lag = S, differences = D)
}

# Differoitujen aikasarjojen autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(dele, lag.max=168)
acf(dele, lag.max=168, type = "partial")
acf(dtemp, lag.max=168)
acf(dtemp, lag.max=168, type = "partial")
ccf(dele, dtemp, lag.max=168)

# Estimoidaan malli ja lasketaan ennusteet ilman ulkoista muuttujaa.
p = 1
q = 1
P = 1
Q = 1
malli = arima(ele,
              order = c(p,d,q),
              seasonal = list(order = c(P, D, Q),period = S),
              method = "CSS")
enne = predict(malli, n.ahead = 24)

# Estimoidaan malli lämpötilan kanssa. Määrää lämpötilan mahdollinen viive L.
L = 1
tempestimointi = eletemp$Celcius[1:(816-L)]
tempennuste = eletemp$Celcius[(816-L+1):(816-L+24)]
eleestimointi = ts(eletemp$kWh[(1+L):816], start = 1, frequency = 24)
malli2 = arima(eleestimointi,
               order = c(p,d,q),
               seasonal = list(order = c(P, D, Q), period = S),
               xreg = tempestimointi,
               method = "CSS")
enne2 = predict(malli2,
                n.ahead = 24,
                newxreg = tempennuste)

# Esimerkki Portmanteau-testistä. Onko residuaaliaikasarjan alussa nollia?
Box.test(malli$residuals,
         lag = 20,
         type = "Ljung-Box",
         fitdf = p + q + P + Q)

# Palautetaan plottaus normaaliin 1x1 ruutuun
par(mfrow=c(1,1))

# Plotataan kuva sahkonkulutusaikasarjasta, mallin (1) sovitteesta,
# ennusteesta ja ennusteen 95 %:n eli 1,96*sigma-luottamusväleistä.
ts.plot(ele,
        ele - malli$residuals,
        enne$pred,
        enne$pred + 1.96*enne$se,
        enne$pred - 1.96*enne$se,
        col = c("black", "red", "blue", "blue", "blue"),
        main  = "Sovite ja ennuste")

# Plotataan kuva pelkästä ennusteesta.
ts.plot(enne$pred,
        enne$pred + 1.96*enne$se,
        enne$pred - 1.96*enne$se,
        col = c("black", "blue", "blue"),
        main = "Ennuste ja  95 %:n luottamusvälit")

# Kirjoitetaan ennuste ja luottamusvälit .csv-tiedostoon, jonka voi avata Excelillä.
output = cbind(enne$pred,
               enne$pred + 1.96*enne$se,
               enne$pred - 1.96*enne$se)
write.csv2(output, file = 'ennuste.csv')
