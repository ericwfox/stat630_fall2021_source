# read in CDC data set
cdc <- readRDS(url("https://ericwfox.github.io/data/cdc.rds"))

# tables------------------
# frequency table
table(cdc$smoke100)

# relative frequency table
table(cdc$smoke100) / 20000

# bar plot----------------
smoke_tb <- table(cdc$smoke100)
barplot(smoke_tb, xlab="Smoked at least 100 cigarettes", 
        names.arg = c("no", "yes"), ylab="Count") 
barplot(smoke_tb/20000, 
        xlab="Smoked at least 100 cigarettes", 
        names.arg = c("no", "yes"), ylab="Proportion")

# contingency table--------------
table(cdc$smoke100, cdc$gender)
addmargins(table(cdc$smoke100, cdc$gender))

# stacked bar plot-----------------
barplot(table(cdc$smoke100, cdc$gender), 
        col=c("grey", "cyan"))
legend("bottomright", c("no", "yes"), col=c("grey", "cyan"), 
       title="smoke", pch=16)

# row and column proportions-------------------------
prop.table(table(cdc$smoke100, cdc$gender))
prop.table(table(cdc$smoke100, cdc$gender), margin=1)
prop.table(table(cdc$smoke100, cdc$gender), margin=2)

proptb <- prop.table(table(cdc$smoke100, cdc$gender), margin=2)
barplot(proptb, col=c("grey", "cyan"), ylab="Proportion")
legend("bottomright", c("no", "yes"), col=c("grey", "cyan"), 
       title = "smoke", bg="white", pch=16)

# factors----------------------
class(cdc$genhlth)
table(cdc$genhlth)

# create factor and specify order for levels  
cdc$genhlth <- factor(cdc$genhlth, 
  levels=c("poor", "fair", "good", "very good", "excellent"))
class(cdc$genhlth)
table(cdc$genhlth)

barplot(table(cdc$genhlth), xlab="General health", ylab="Count")

# ggplot2----------------------
library(ggplot2)

ggplot(data = cdc) + geom_bar(aes(x=genhlth)) +
  labs(x="general health")

ggplot(data = cdc) + 
  geom_bar(aes(x=genhlth, fill=factor(smoke100))) + 
  labs(x="general health", fill="smoke")

ggplot(data = cdc) +
  geom_bar(aes(x=genhlth, fill=factor(smoke100)), position="dodge") + 
  labs(x="general health", fill="smoke") 

ggplot(data = cdc) + 
  geom_bar(aes(x=genhlth, fill=factor(smoke100)), position="fill") + 
  labs(x="general health", y="proportion", fill="smoke")


# histogram-----------------------------
hist(mtcars$mpg, main='', xlab="Miles per gallon (mpg)")

# adjusting bin size
par(mfrow=c(2,2)) # split plot into 4 panels
hist(cdc$weight, main="default # of bins")
hist(cdc$weight, breaks=10, main="10 bins")
hist(cdc$weight, breaks=50, main="50 bins")
hist(cdc$weight, breaks=100, main="100 bins")
dev.off() # resets graphical parameters

# histogram density
hist(cdc$weight, freq=FALSE, main='')
lines(density(cdc$weight), col="red", lwd=1.5)

# box plot ----------------------
boxplot(cdc$weight, ylab = "Weight")
boxplot(weight ~ gender, data = cdc, xlab="Gender", ylab="Weight")

# maps----------------------
library(maps) 
map("world")
map("state", "california")
map("county", "ca")

# epa stream data---------------------
nrsa <- readRDS(url("https://ericwfox.github.io/data/nrsa.rds"))
head(nrsa, n=10)
dim(nrsa)

map("state")
points(nrsa$lon, nrsa$lat, cex=0.5)

nrsa_good <- subset(nrsa, cond == "Good")
nrsa_fair <- subset(nrsa, cond == "Fair")
nrsa_poor <- subset(nrsa, cond == "Poor")
map("state")
points(nrsa_good$lon, nrsa_good$lat, cex=0.5, col = "blue")
points(nrsa_fair$lon, nrsa_fair$lat, cex=0.5, col = "orange")
points(nrsa_poor$lon, nrsa_poor$lat, cex=0.5, col = "red")
legend("bottomright", c("poor", "fair", "good"), 
       col=c("red","orange","blue"), pch=1)


