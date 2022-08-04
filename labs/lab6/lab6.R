## ----eval=T, echo=T, warning=F, message=F------------------------------------------------------
library(resampledata)


## ----fig.width=8, fig.height=4-----------------------------------------------------------------
Arsenic <- Bangladesh$Arsenic 
summary(Arsenic)
sd(Arsenic)

par(mfrow=c(1,2))
hist(Arsenic)
qqnorm(Arsenic)
qqline(Arsenic)


## ----fig.width=8, fig.height=4-----------------------------------------------------------------
set.seed(9999)
n <- length(Arsenic); n
replicates <- rep(0, 5000)
for(i in 1:5000) {
  boot_samp <- sample(Arsenic, size = n, replace = TRUE)
  replicates[i] <- mean(boot_samp) 
}

# bootstrap distribution
par(mfrow=c(1,2))
hist(replicates, xlab="Replicates of sample mean", main='')
abline(v=mean(Arsenic), col="cyan", lwd=2)
qqnorm(replicates)
qqline(replicates)

# bootstrap standard error
sd(replicates)

# 95% bootstrap CI
quantile(replicates, c(0.025, 0.975))


## ----------------------------------------------------------------------------------------------
ci_lower <- mean(Arsenic) - 1.96 * sd(Arsenic) / sqrt(n)
ci_upper <- mean(Arsenic) + 1.96 * sd(Arsenic) / sqrt(n)
round(c(ci_lower, ci_upper), 2)


## ----fig.width=8, fig.height=4-----------------------------------------------------------------
mean(Arsenic, trim=0.25) # 25% trimmed mean
replicates <- rep(0, 5000)
for(i in 1:5000) {
  boot_samp <- sample(Arsenic, size = n, replace = TRUE)
  replicates[i] <- mean(boot_samp, trim = 0.25) 
}

# boostrap distribution
par(mfrow=c(1,2))
hist(replicates, xlab="Replicates of the trimmed mean", main='')
abline(v=mean(Arsenic, trim=0.25), col="cyan", lwd=2)
qqnorm(replicates)
qqline(replicates)

# bootstrap standard error
sd(replicates)

# 95% bootstrap CI
quantile(replicates, c(0.025, 0.975))