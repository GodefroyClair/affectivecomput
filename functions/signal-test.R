# test of pracma library

library(pracma)
library(ggplot2)
library(stats)
library(stringr)
library(testthat)
library(purrr)

x <- seq(0, 1, len = 1024)
pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.40, 0.44, 0.65, 0.76, 0.78, 0.81)
hgt <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
wdt <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 0.005)

pSignal <- numeric(length(x))
for (i in seq(along=pos)) {
  pSignal <- pSignal + hgt[i]/(1 + abs((x - pos[i])/wdt[i]))^4
}
df.test <- data.frame(x,pSignal)
max.df <- as.data.frame(findpeaks(pSignal))
max.df$x <- x[max.df[,2]]
ggplot(df.test, aes(x, pSignal)) + 
  geom_line() 
#  geom_point(data = max.df, aes(x, V1))
fft(pSignal)

plot(pSignal)

## Not run:
plot(pSignal, type="l", col="navy")
grid()
x <- findpeaks(pSignal, npeaks=3, threshold=4, sortstr=TRUE)
points(x[, 2], x[, 1], pch=20, col="maroon")## End(Not run)



signal::specgram(x, n = min(256, length(x)), Fs = 2, window = hanning(n),
         overlap = ceiling(length(window)/2))
## S3 method for class 'specgram'
plot(x, col = gray(0:512 / 512), xlab="time", ylab="frequency")
## S3 method for class 'specgram'
print(x, col = gray(0:512 / 512), xlab="time", ylab="frequency")

load("/Users/godot/Downloads/orderts.RData")
orderts2 <- cbind(orderts[-13,], weekinq=c(1:117))
prev <- orderts2[1,]
runvar <- 1
for(i in 2:nrow(orderts2)){
  current <- orderts2[i,]
  orderts2[i,"weekinq"] <- ifelse(prev$quarter == current$quarter, runvar+1, 1)
  runvar <- ifelse(prev$quarter == current$quarter, runvar+1, 1)
  prev <- current
}
rm(prev, current, runvar, i)
fft:fft(orderts2[1:104, "orders"])
qplot(weekinq, orders, data = orderts2, colour = as.factor(year), geom = "line") +
  facet_wrap(~quarter)
f2 <- data.frame(coef = fft(orderts2[1:104, "orders"]), freqindex = c(1:104))
qplot(freqindex, Mod(coef), data = f2[2:53,], geom = "line")

## algo max

series <- c(0,1,2,3,4,3,2,1)
series.op <- - series
repet <- 4
my.signal <- rep(c(series, series.op),repet)
#fast loes for testing
my.signal.loess <- rep(rep(0,2*length(series)), repet)
expect_equal(length(my.signal.loess), length(my.signal))


#find the max (respectively min) for each series of values of signal above (under) signal.average
#then replace each value of the whole signal with the max (resp. min) of the series in its in.
signal.max.min <- function(signal, signal.average) {
  #boolean vec :check if signal is above or under the loess
  above <- signal >= signal.average
  #split according to above/under
  signal.split <- split(signal, cumsum(c(0, diff(above) != 0)))
  #replicate the max of the sequence above average (the min if under)
  
  #curried function :
  #1st param : a list of sequences
  #2nd param : a stat function
  sub.signal.lst <- function(signal.seq.list) {
      function(fun){
        map_(
          signal.seq.list, function(lst){
            #todo : need a condition to decide fun is min or max according to above...
            extremum <- fun(lst)
            #get the max for postive and the min for negative
            #replicate the length of the list
            rep(extremum, length(lst))
            }
        )#end sapply
        }
      #end anonymous extremum function
  }
  extremum <- sub.signal.lst(signal.split)
  max.lsts.lst <- extremum(max)
  min.lsts.lst <- extremum(min)
  
  
  #unwrap the list of lists
  max.list <- as.vector(unlist(max.lsts.lst))
  min.list <- as.vector(unlist(min.lsts.lst))
  res <- rep(0,length(above))
  res[above] <- max.list[above]
  res[!above] <- min.list[!above]
  res
}

signal.max.min(my.signal, my.signal.loess)

data.AB$loess <- loess(data.AB$respiration ~ as.numeric(data.AB$date), degree=1,span=.1)$fitted
data.AB$s.max.min <- signal.max.min(data.AB$respiration, loess.ex$fitted)
ggplot(data = data.AB[0:5000,]) + geom_line(aes(x= date, y = respiration), color = "blue") +
  #geom_smooth(method="loess", formula = y ~ x, se=TRUE, size= 1, span=.2, color = "green")  +
  geom_line(aes(x = date, y = loess ), color = "green")  +
  geom_line(aes(x = date, y = s.max.min), color = "red") 
