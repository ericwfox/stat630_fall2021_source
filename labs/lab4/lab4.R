# rnorm ------------------------------
# generate 1000 random numbers from N(0,1)
z <- rnorm(1000)
hist(z, main='')

# generate 1000 random numbers from N(10,3)
x <- rnorm(1000, mean=10, sd=3)
hist(x, main='')


# dnorm ------------------------------
x <- seq(-3, 3, by=0.01)
y <- dnorm(x)
plot(x, y, type="l", xlab="x", ylab="f(x)", main="N(0,1)")

# plot normal distribution with mean=10 and sd=3
x <- seq(0, 20, by=0.01)
y <- dnorm(x, mean = 10, sd = 3)
plot(x, y, type="l", xlab="x", ylab="f(x)", main="N(10,3)")

# how changing sd affects shape
x <- seq(-11, 31, by=0.01)
y1 <- dnorm(x, mean = 10, sd = 3)
y2 <- dnorm(x, mean = 10, sd = 5)
y3 <- dnorm(x, mean = 10, sd = 7)

plot(x, y1, type="l", xlab="x", ylab="f(x)", main='')
lines(x, y2, lty=2)
lines(x, y3, lty=3)

legend("topright", c("N(10,3)", "N(10,5)", "N(10,7)"), lty=c(1,2,3))


# assessing normality ------------------------------
cdc <- readRDS(url("https://ericwfox.github.io/data/cdc.rds"))
cdc_m <- subset(cdc, gender == "m") # subset males

par(mfrow=c(1,2))  # split graphics region into 2 panes
# histogram density with normal curve
hist(cdc_m$height, breaks=30, freq=FALSE, xlab="Male heights", main='')
x <- seq(50, 90, 0.01)
y <- dnorm(x, mean=mean(cdc_m$height), sd=sd(cdc_m$height))
lines(x, y, col="red", lwd=2) 

qqnorm(cdc_m$height) # normal QQ plot
qqline(cdc_m$height) # add line for reference


# male weights
par(mfrow=c(1,2)) 
hist(cdc_m$weight, breaks=30, freq=FALSE, xlab="Male weights", main='')
x <- seq(75, 500, 0.01)
y <- dnorm(x, mean=mean(cdc_m$weight), sd=sd(cdc_m$weight))
lines(x, y, col="red", lwd=2)

qqnorm(cdc_m$weight)
qqline(cdc_m$weight)


# simulate from N(0,1)
# n=30
par(mfrow=c(1,2))
set.seed(999) # set seed for reproducibility
sim_norm30 <- rnorm(30)
hist(sim_norm30, freq=FALSE, xlab='', main='')
x <- seq(-3, 3, 0.01)
y <- dnorm(x, mean=mean(sim_norm30), sd=sd(sim_norm30))
lines(x, y, col="red", lwd=2)

qqnorm(sim_norm30)
qqline(sim_norm30)

#n=100
par(mfrow=c(1,2))
sim_norm100 <- rnorm(100)
hist(sim_norm100, freq=FALSE, xlab='', main='')
x <- seq(-3, 3, 0.01)
y <- dnorm(x, mean=mean(sim_norm100), sd=sd(sim_norm100))
lines(x, y, col="red", lwd=2)

qqnorm(sim_norm100)
qqline(sim_norm100)

#n=1000
par(mfrow=c(1,2))
sim_norm1000 <- rnorm(1000)
hist(sim_norm1000, freq=FALSE, xlab='', main='')
x <- seq(-3, 3, 0.01)
y <- dnorm(x, mean=mean(sim_norm1000), sd=sd(sim_norm1000))
lines(x, y, col="red", lwd=2)

qqnorm(sim_norm1000)
qqline(sim_norm1000)
