set.seed(100)
x1 <- rnorm(10000, mean = 10, sd = 2)
x2 <- rnorm(10000, mean = 20, sd = 2)
x <- c(x1, x2)

pdf("figure/bimodal_pop.pdf", width = 5, height = 4)
par(mar = c(4, 1, 4, 1))
hist(x, col = "grey", border = "grey", breaks = 30, main = "Population", 
     yaxt = "n", ylab = "", xlab = "", xlim = c(0, 30))
dev.off()


pdf("figure/samp_dist.pdf", width = 8, height = 3.5)
par(mfrow=c(1, 3), mar = c(4, 1, 4, 1))

set.seed(100)
xbars20 <- c()
for(i in 1:5000) {
  samp <- sample(x, 20)
  xbars20[i] <- mean(samp) 
}
hist(xbars20, col = "grey", border = "grey", main = "(1)",
     yaxt = "n", ylab = "count", xlab = "", xlim = c(0, 30), cex.main=2)

xbars5 <- c()
for(i in 1:5000) {
  samp <- sample(x, 5)
  xbars5[i] <- mean(samp) 
}
hist(xbars5, col = "grey", border = "grey", main = "(2)",
     yaxt = "n", ylab = "", xlab = "", xlim = c(0, 30), cex.main=2)

xbars2 <- c()
for(i in 1:5000) {
  samp <- sample(x, 2)
  xbars2[i] <- mean(samp) 
}
hist(xbars2, col = "grey", border = "grey", main = "(3)", breaks = 40,
     yaxt = "n", ylab = "", xlab = "", xlim = c(0, 30), cex.main=2)
dev.off()




