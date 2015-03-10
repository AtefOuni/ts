tmp  <- scan("Data/global.dat")
tmp <- ts(data = tmp, start = c(1856,1), end = c(2005,12), frequency = 12)
plot(tmp)

data(AirPassengers)
AP  <- AirPassengers


Maine = read.table("Data/Maine.dat",header = TRUE)
Maine = ts(Maine$unemploy, start = c(1996,1), freq=12 )
plot(Maine)

Maine_A  <-  aggregate(Maine)/12
plot(Maine_A)

Maine_Fe = window(Maine,start=c(1996,2),freq=TRUE)

CBE  <- read.table("Data/cbe.dat",header=TRUE)

Elec  <-  ts(CBE[,3],start=1958,freq=12)
Beer  <-  ts(CBE[,2],start=1958,freq=12)
Choc  <-  ts(CBE[,1],start=1958,freq=12)

plot(cbind(Elec,Beer,Choc))

AP_Elec <- ts.intersect(AP,Elec)
plot(as.vector(AP_Elec[,1]),as.vector(AP_Elec[,2]))
abline(reg=lm(AP_Elec[,2] ~ AP_Elec[,1]))




Z <- read.table("Data/pounds_nz.dat",header=TRUE)

Z_ts  <- ts(Z, start=1991, frequency = 4)
plot(Z_ts)

Global  <- scan("Data/global.dat")
Global_ts  <- ts(Global,start=c(1856,1),end=c(2005,12),frequency = 12)
Global_ts_mean = aggregate(Global_ts,FUN = mean,nfrequency = 1)
str(Global_ts_mean)

layout(2:1)
plot(Global_ts)
plot(Global_ts_mean)

plot(Elec)
Elec_decomp  <- decompose(Elec,type = "multiplicative")
plot(Elec_decomp)


AP
window(AP,frequency = 1)

wave.dat <- read.table("Data/wave.dat",header = TRUE)
w_ts <- ts(wave.dat$waveht)
plot(w_ts)
w_acf <- acf(w_ts)
w_acf$acf

acf(AirPassengers)

van.dat <- read.table("Data/varnish.dat",header = TRUE)
View(van.dat)
plot(van.dat$x,van.dat$y)

Build.dat <- read.table("Data/ApprovActiv.dat",header=TRUE)
App.ts <- ts(Build.dat$Approvals,frequency = 4)
Act.ts <- ts(Build.dat$Activity,frequency = 4)
ts.plot(App.ts,Act.ts,lty=c(1,3))

acf(ts.union(App.ts,Act.ts))

Motor.dat <- read.table("Data/motororg.dat",header = TRUE)
Comp.ts <- ts(Motor.dat$complaints,start = 1996,frequency = 12)
plot(Comp.ts)

Comp.hw1 <- HoltWinters(Comp.ts,beta = 0,gamma = 0)
plot(Comp.hw1)
