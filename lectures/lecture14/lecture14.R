library(MASS)
pdf("mpg_scatter.pdf", width=5, height=4)
par(mar=c(4, 4, 1.5, 1.5))
plot(Cars93$Weight, Cars93$MPG.highway, 
     xlab = "Weight (lbs)", ylab = "Highway MPG")
dev.off()


library(MASS)
lm1 <- lm(MPG.highway ~ Weight, data=Cars93)
summary(lm1)
pdf("mpg_scatter_fit.pdf", width=5, height=4)
par(mar=c(4, 4, 1.5, 1.5))
plot(Cars93$Weight, Cars93$MPG.highway, 
     xlab = "Weight (lbs)", ylab = "Highway MPG")
abline(lm1)
dev.off()