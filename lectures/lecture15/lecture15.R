library(MASS)
lm1 <- lm(MPG.highway ~ Weight, data=Cars93)
summary(lm1)

pdf("figure/mpg_lmplots.pdf", width=7, height=3.5)
par(mfrow=c(1,2), mar=c(4.5, 4.5, 1.5, 1.5))
plot(Cars93$Weight, Cars93$MPG.highway, 
     xlab = "Weight (lbs)", ylab = "Highway MPG")
abline(lm1)
plot(Cars93$Weight, resid(lm1),
     xlab = "Weight (lbs)", ylab = "Residuals")
abline(h=0)
dev.off()

pdf("figure/cars_norm_resid.pdf", width=7, height=3.5)
par(mfrow=c(1,2))
hist(resid(lm1), xlab = "Residuals", main= "")
qqnorm(resid(lm1))
qqline(resid(lm1))
dev.off()


#-------------------------------------------
# linear
sim_1 = function(sample_size = 100) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = 2)
  data.frame(x, y)
}

# non constant variance
sim_2 = function(sample_size = 100) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x + rnorm(n = sample_size, mean = 0, sd = x)
  data.frame(x, y)
}

# non linear
sim_3 = function(sample_size = 100) {
  x = runif(n = sample_size) * 5
  y = 3 + 5 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 5)
  data.frame(x, y)
}

set.seed(42)
sim_data_1 = sim_1()

plot(y ~ x, data = sim_data_1, cex=0.75)
lm1 = lm(y ~ x, data = sim_data_1)
abline(lm1)

plot(predict(lm1), resid(lm1), cex=0.75)
abline(h=0)
dev.off()


# residual plot nonconst var---------------------
set.seed(42)
sim_data_2 = sim_2()

pdf(file='figure/resid_var.pdf', width=6, height=3)
par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(y ~ x, data = sim_data_2, cex=0.75)
lm2 = lm(y ~ x, data = sim_data_2)
abline(lm2)

plot(sim_data_2$x, resid(lm2), xlab='x', ylab='Residuals', cex=0.75)
abline(h=0)
dev.off()

# residual plot nonlin---------------------
set.seed(42)
sim_data_3 = sim_3()

pdf(file='figure/resid_nonlin.pdf', width=6, height=3)
par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(y ~ x, data = sim_data_3, cex=0.75)
lm3 = lm(y ~ x, data = sim_data_3)
abline(lm3)

plot(sim_data_3$x, resid(lm3),  xlab='x', ylab='Residuals', cex=0.75)
abline(h=0)
dev.off()
