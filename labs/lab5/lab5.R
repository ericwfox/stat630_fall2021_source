## ---------------------------------------------------------------------------------------
# read data set into R
cdc <- readRDS(url("https://ericwfox.github.io/data/cdc.rds"))


## ----fig.width=6, fig.height=3----------------------------------------------------------
summary(cdc$weight)

par(mfrow=c(1,2), cex=0.6)
hist(cdc$weight)

qqnorm(cdc$weight)
qqline(cdc$weight)


## ----fig.width=6, fig.height=3----------------------------------------------------------
set.seed(999) 
xbars <- rep(0, 5000)
for(i in 1:5000) {
  samp <- sample(cdc$weight, 30)
  xbars[i] <- mean(samp)
}

par(mfrow=c(1,2), cex=0.6)
hist(xbars, xlab="Mean weight", main="Histogram of Sample Means (n=30)")
qqnorm(xbars)
qqline(xbars)


## ---------------------------------------------------------------------------------------
mean(xbars) 
mean(cdc$weight) # population mean mu

sd(xbars)
sd(cdc$weight) / sqrt(30) # sigma / sqrt(n), where sigma is population st dev


## ---------------------------------------------------------------------------------------
# it's obviously tedious to generate a sampling distribution without using a loop:
set.seed(999)
samp1 <- sample(cdc$weight, 30)
mean(samp1)

samp2 <- sample(cdc$weight, 30)
mean(samp2)

samp3 <- sample(cdc$weight, 30)
mean(samp3)

samp4 <- sample(cdc$weight, 30)
mean(samp4)

set.seed(999)
xbars <- rep(0, 4)
for(i in 1:4) {
  samp <- sample(cdc$weight, 30)
  xbars[i] <- mean(samp)
}
xbars

# this loop prints out the variable i at each iteration
for(i in 1:10) {
  print(i)
}

# some other examples demonstrating the behavior of loops in R:
x <- c("a", "b", "c", "d")
for(i in 1:4) {
  print(x[i])
}

x <- c(5, 12, 13)
for(n in x) {
  print(n^2)
}


## ----fig.width=7, fig.height=7----------------------------------------------------------
set.seed(999) 
xbars5 <- rep(0, 5000) 
xbars10 <- rep(0, 5000)
xbars20 <- rep(0, 5000)
xbars100 <- rep(0, 5000)
for(i in 1:5000) {
  samp5 <- sample(cdc$weight, 5)
  xbars5[i] <- mean(samp5)
  
  samp10 <- sample(cdc$weight, 10)
  xbars10[i] <- mean(samp10)
  
  samp20 <- sample(cdc$weight, 20)
  xbars20[i] <- mean(samp20)
  
  samp100 <- sample(cdc$weight, 100)
  xbars100[i] <- mean(samp100)
}

par(mfrow=c(2,2), cex=0.75)
hist(xbars5, xlim=c(110, 260), xaxt="n", main = "n=5")
axis(side=1, at=seq(100, 250, 25), labels=seq(100, 250, 25))
hist(xbars10, xlim=c(110, 260), xaxt="n", main = "n=10")
axis(side=1, at=seq(100, 250, 25), labels=seq(100, 250, 25))
hist(xbars20, xlim=c(110, 260), xaxt="n", main = "n=20")
axis(side=1, at=seq(100, 250, 25), labels=seq(100, 250, 25))
hist(xbars100, xlim=c(110, 260), xaxt="n", main = "n=100")
axis(side=1, at=seq(100, 250, 25), labels=seq(100, 250, 25))


## ----fig.width=7, fig.height=7----------------------------------------------------------
par(mfrow=c(2,2), cex=0.75)
qqnorm(xbars5, main="Normal Q-Q Plot (n=5)")
qqline(xbars5)
qqnorm(xbars10, main="Normal Q-Q Plot (n=10)")
qqline(xbars10)
qqnorm(xbars20, main="Normal Q-Q Plot (n=20)")
qqline(xbars20)
qqnorm(xbars100, main="Normal Q-Q Plot (n=100)")
qqline(xbars100)


## ---------------------------------------------------------------------------------------
mean(cdc$weight) # population mean
# means of each sampling distribution
mean(xbars5)
mean(xbars10)
mean(xbars20)
mean(xbars100)

# standard deviations of each sampling distribution
sd(xbars5)
sd(cdc$weight) / sqrt(5)
sd(xbars10)
sd(cdc$weight) / sqrt(10)
sd(xbars20)
sd(cdc$weight) / sqrt(20)
sd(xbars100)
sd(cdc$weight) / sqrt(100)


## ----fig.width=6, fig.height=3----------------------------------------------------------
set.seed(999) 
meds <- rep(0, 5000)
for(i in 1:5000) {
  samp <- sample(cdc$weight, 30)
  meds[i] <- median(samp)
}

par(mfrow=c(1,2), cex=0.6)
hist(meds, main="Histogram of Sample Medians (n=30)")
qqnorm(meds)
qqline(meds)

