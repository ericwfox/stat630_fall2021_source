# useful r commands measures of position
x <- c(68, 76, 66, 63, 70, 66, 71, 71, 64, 71)
summary(x) 
mean(x)
median(x)
min(x)
max(x)
sort(x)

# affect of outlier
x <- c(62, 63, 64, 66, 72, 1000)
mean(x)
median(x)

# temperature
x = c(72, 67, 73, 81, 75)
mean(x)
median(x)
(5/9)*(73.6-32)
(5/9)*(73-32)

# using quantile 
quantile(cdc$age, c(0.2, 0.8))

# compute sd
x = c(2, 5, 10, 15, 18)
var(x)
sd(x)

# use r measures of variation
x <- c(68, 76, 66, 63, 70, 66, 71, 71, 64, 71)
var(x)
sd(x)
max(x)-min(x) #range
IQR(x)

# sd example
x=c(2, 5, 10, 15, 18)
sd(x)
var(x)

# comparing standard deviations
x1 = c(100, 99, 98, 50, 2, 1, 0); var(x1)
x2 = c(53, 52, 51, 50, 49, 48, 47); var(x2)
x3 = c(51, 51, 51, 50, 49, 49, 49); var(x3)
var(x1); mean(x1)
var(x2); mean(x2)
var(x3); mean(x3)



# boxplot
set.seed(10)
x = round(rexp(11, 0.05))
boxplot(x)
x <- c(0, 18, 15, 32, 5, 22, 47, 15, 26, 13, 9)
summary(x)

pdf("boxplot1.pdf", width=2.5, height=4)
par(mar = c(1, 3, 1, 1))
boxplot(x)
dev.off()


# problem set

# exercise 1
set.seed(1)
x = sample(cdc$age, 7)

# exercise 2
set.seed(100)
x = rnorm(30, 75, 7)
summary(x)
sd(x)

# exercise 3
set.seed(200)
x = round(rexp(14, 0.1))
boxplot(x)

# exercise 4
x <- mtcars$hp
boxplot(x)
summary(x)


# data transformations
library(openintro) # load library to access data
pdf("county_dist.pdf", width=7, height=6)
par(mfrow=c(2,2), mar=c(4.5, 4, 2, 2))
hist(county$pop2010, xlab="Population in 2010", main="")
hist(log10(county$pop2010), xlab="Log of Population in 2010", main="")
boxplot(county$pop2010, xlab="Population in 2010", horizontal = TRUE)
boxplot(log10(county$pop2010), 
        xlab="Log of Population in 2010", horizontal = TRUE)
dev.off()

# symmetric, skewed right, and skewed left distributions
pdf("distributions.pdf", width=7, height=2.5)
par(mfrow = c(1, 3), mar=c(1, 1, 3, 1))
# symmetric, normal
grd <- seq(-4, 4, by=0.01)
y <- dnorm(grd)
plot(grd, y, type = "l", xaxt="n", yaxt="n", 
     xlab="", ylab="", main="Symmetric")
# skewed right
grd <- seq(0, 15, by=0.01)
y <- dchisq(grd, df=5)
plot(grd, y, type = "l", xaxt="n", yaxt="n", 
     xlab="", ylab="", main="Skewed Right")
# skewed left
grd <- seq(0, 15, by=0.01)
y <- dchisq(grd, df=5)
plot(-1*grd, y, type = "l", xaxt="n", yaxt="n", 
     xlab="", ylab="", main="Skewed Left")
dev.off()


