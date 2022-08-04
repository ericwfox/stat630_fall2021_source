plot_norm_both <- function(mu=0, sigma=1, a, b) {
  par(mar=c(2.5,1,1,1), cex=1.25)
  # draw normal curve
  x = seq(mu - 3*sigma, mu + 3*sigma, 0.1)
  y = dnorm(x, mean = mu, sd = sigma)
  plot(x, y, type="l", ylab='', xlab='', 
       xaxt="n", yaxt="n", main='')
  # shade area lower
  lb = mu - 4*sigma
  x = c(lb, seq(lb, a, 0.1), a) 
  y = c(0, dnorm(seq(lb, a, 0.1), mean = mu, sd = sigma), 0) 
  polygon(x, y, col="lightgrey")
  # shade area upper
  ub = mu + 4*sigma
  x = c(b, seq(b, ub, 0.1), b) 
  y = c(0, dnorm(seq(b, ub, 0.1), mean = mu, sd = sigma), 0) 
  polygon(x, y, col="lightgrey")
  axis(side=1, at=c(a, b), labels=c("-|t|", "|t|"))
}

plot_norm_greater <- function(mu=0, sigma=1, a) {
  par(mar=c(2.5,1,1,1), cex=1.25)
  # draw normal curve
  x = seq(mu - 3*sigma, mu + 3*sigma, 0.1)
  y = dnorm(x, mean = mu, sd = sigma)
  plot(x, y, type="l", ylab='', xlab='', 
       xaxt="n", yaxt="n", main='')
  # shade area
  b = mu + 4*sigma
  x = c(a, seq(a, b, 0.1), b) 
  y = c(0, dnorm(seq(a, b, 0.1), mean = mu, sd = sigma), 0) 
  polygon(x, y, col="lightgrey")
  #axis(side=1, at=c(mu, a), labels=c(mu, a))
  axis(side=1, at=a, labels="t")
}

plot_norm_less <- function(mu=0, sigma=1, b) {
  par(mar=c(2.5,1,1,1), cex=1.25)
  # draw normal curve
  x = seq(mu - 3*sigma, mu + 3*sigma, 0.1)
  y = dnorm(x, mean = mu, sd = sigma)
  plot(x, y, type="l", ylab='', xlab='', 
       xaxt="n", yaxt="n", main='')
  # shade area
  a = mu - 4*sigma
  x = c(a, seq(a, b, 0.1), b) 
  y = c(0, dnorm(seq(a, b, 0.1), mean = mu, sd = sigma), 0) 
  polygon(x, y, col="lightgrey")
  # axis(side=1, at=c(mu, b), labels=c(mu, b))
  axis(side=1, at=b, labels="t")
}

pdf("pvalue_upper.pdf", width=4, height=3)
plot_norm_greater(mu=0, sigma=1, a=1.75)
legend("topright", "p-value", pch=15, col="lightgrey")
dev.off()

pdf("pvalue_lower.pdf", width=4, height=3)
plot_norm_less(mu=0, sigma=1, b=-1.75)
legend("topright", "p-value", pch=15, col="lightgrey")
dev.off()

pdf("pvalue_both.pdf", width=4, height=3)
plot_norm_both(mu=0, sigma=1, a=-1.75, b=1.75)
legend("topright", "p-value", pch=15, col="lightgrey")
dev.off()






