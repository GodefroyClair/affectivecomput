###DRAFT SIGNAL####

par(mfrow = c(1, 1))
number_of_cycles = 2
max_y = 40
N <- 256

x = 0:(N-1)
a = number_of_cycles * 2 * pi/length(x)

iy = max_y * sin(x*a)
noise1 = max_y * 1/10 * sin(x*a*10) #

plot(x, y, type="l", col="red", ylim=range(-1.5*max_y,1.5*max_y,5))
points(x, y + noise1, col="green", pch=20)
points(x, noise1, col="yellow", pch=20)

ecg <- read.table('http://www.indyrad.iupui.edu/public/mmiller3/sample-ecg-1kHz.txt')
names(ecg) <- c('t','ecg')


## Now let's look at some artificial data:
x <- seq(100000)/1000  # pretend we're sampling at 1 kHz

## We'll put in two frequency components, plus a dc offset
f1 <- 5  # Hz
f2 <- 2  # Hz
y <- 0.1*sin(2*pi*f1*x) + sin(2*pi*f2*x) + 50
fft.y <- fft(y)
delta <- x[2] - x[1]
f.Nyquist <- 1 / 2 / delta
f <- f.Nyquist*c(seq(length(x)/2), -rev(seq(length(x)/2)))/(length(x)/2)

par(mfrow=c(2,2))
plot(x,y, type='l', xlim=c(0,20))
plot(f, Mod(fft.y), type='l', log='y')

## Now let's zoom in and mark the points were I expect to see peaks:
plot(f, Mod(fft.y), type='l', log='y', xlim=c(-10,10))
rug(c(-f1, -f2, 0, f1, f2), col='red', side=3)
length(f)
length(Mod(fft.y))
length((fft.y))
