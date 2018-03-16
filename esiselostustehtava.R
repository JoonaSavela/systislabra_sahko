setwd("~/systislabra_sahko")


ma <- c(0.8, rep(0, 22), 0.3, -0.3*0.8)
ar <- c(rep(0, 23), 0.5)

lag.max = 100

plot(0:lag.max, ARMAacf(ar = 0, ma = ma, lag.max = lag.max)) # (i)

lag.max = 100

plot(0:lag.max, ARMAacf(ar = ar, ma = ma, lag.max = lag.max)) # (ii)

# pacf

plot(ARMAacf(ar = 0, ma = ma, lag.max = lag.max, pacf = T)) # (i)

plot(ARMAacf(ar = ar, ma = ma, lag.max = lag.max, pacf= T)) # (ii)

