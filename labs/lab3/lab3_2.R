setwd("~/Documents/CSUEB/teaching/STAT630_F19/labs/lab3/figure/")

# read in CDC data set
data_url <- "https://github.com/ericwfox/stat630data/raw/master/cdc.csv"
cdc <- read.csv(data_url, header = TRUE)

# tables------------------
# frequency table
table(cdc$smoke100)

#relative frequency table
table(cdc$smoke100) / 20000

#bar plot----------------
pdf(file="barplot_smoke.pdf",width=8,height=4)
par(mfrow=c(1,2), mar=c(4.5, 4.5, 2.5, 2), cex.lab=1.25, cex.axis=1.25)
smoke_tb <- table(cdc$smoke100)
barplot(smoke_tb, xlab="Smoked at least 100 cigarettes", 
        names.arg = c("no", "yes"), ylab="Count") 
barplot(smoke_tb/20000, 
        xlab="Smoked at least 100 cigarettes", 
        names.arg = c("no", "yes"), ylab="Proportion")
dev.off()

# contingency table--------------
table(cdc$smoke100, cdc$gender)
addmargins(table(cdc$smoke100, cdc$gender))

# stacked barplot-----------------
pdf(file="barplot_smokegen_stacked.pdf", width=4,height=4)
par(mar=c(4.5, 4.5, 2, 2), cex.lab=1.2, cex.axis=1.2)
barplot(table(cdc$smoke100, cdc$gender), 
        col=c("grey", "cyan"))
legend("bottomright", c("no", "yes"), col=c("grey", "cyan"), 
       title="smoke", bg="white", pch=16)
dev.off()

# Side-by-side barplot-----------------
pdf(file="barplot_smokegen_side.pdf", width=5,height=4)
par(mar=c(4.5, 4.5, 2, 2), cex.lab=1.2, cex.axis=1.2)
barplot(table(cdc$smoke100, cdc$gender), beside=T, 
        col=c("grey", "cyan"))
legend("bottomright", c("no", "yes"), col=c("grey", "cyan"), 
       title = "smoke", bg="white", pch=16)
dev.off()



# row and column proportions-------------------------
prop.table(table(cdc$smoke100, cdc$gender))
prop.table(table(cdc$smoke100, cdc$gender), margin=1)
prop.table(table(cdc$smoke100, cdc$gender), margin=2)

pdf(file="barplot_smokegen_prop.pdf", width=4, height=4)
par(mar=c(4.5, 4.5, 3, 2), cex.lab=1.2, cex.axis=1.2)
proptb <- prop.table(table(cdc$smoke100, cdc$gender), margin=2)
barplot(proptb, col=c("grey", "cyan"), ylab="Proportion")
legend("bottomright", c("no", "yes"), col=c("grey", "cyan"), 
       title = "smoke", bg="white", pch=16)
dev.off()

# mosaic plot
mosaicplot(table(cdc$gender, cdc$smoke100))


# factors----------------------
class(cdc$genhlth)
table(cdc$genhlth)

# create factor and reorder levels
cdc$genhlth <- factor(cdc$genhlth, 
  levels=c('poor', 'fair', 'good', 'very good', 'excellent'))
class(cdc$genhlth)
table(cdc$genhlth)

# character vs factor
gender_char <- c("m", "m", "m")
table(gender_char)
gender_factor <- factor(gender_char, levels=c("m", "f"))
table(gender_factor)

# barplot genhealth
pdf(file="barplot_genhealth.pdf", width=7, height=4)
par(cex.lab=1.25, cex.axis=1.25)
barplot(table(cdc$genhlth), xlab="General health", ylab="Count")
dev.off()

# ggplot2------------------
library(ggplot2)
pdf("barplot_hlth_gg.pdf", width=5, height=3)
ggplot(cdc, aes(x=genhlth)) + geom_bar() +
  labs(x="general health")
dev.off()

pdf("barplot_stack_hlthsmoke_gg.pdf", width=5, height=3)
ggplot(cdc, aes(x=genhlth, fill=factor(smoke100))) + 
  geom_bar() + 
  labs(x="general health", fill="smoke")
dev.off()

pdf("barplot_side_hlthsmoke_gg.pdf", width=5, height=3)
ggplot(cdc, aes(x=genhlth, fill=factor(smoke100))) + 
  geom_bar(position="dodge") + 
  labs(x="general health", fill="smoke")
dev.off()

pdf("barplot_prop_hlthsmoke_gg.pdf", width=5, height=3)
ggplot(cdc, aes(x=genhlth, fill=factor(smoke100))) + 
  geom_bar(position="fill") + 
  labs(x="general health", y="proportion", fill="smoke")
dev.off()

# customize
ggplot(cdc, aes(x=genhlth, fill=factor(smoke100))) + 
  geom_bar(position="dodge") + 
  scale_fill_discrete(name="smoke", breaks=c(0,1), labels=c("no", "yes")) +
  labs(x="general health")

ggplot(cdc, aes(x=genhlth, fill=factor(smoke100))) + 
  geom_bar(position="dodge") + 
  scale_fill_manual(values=c("cyan", "blue"), name="smoke", 
                      breaks=c(0,1), labels=c("no", "yes")) +
  labs(x="general health")

ggplot(cdc, aes(x=genhlth)) + geom_bar(fill="steelblue") +
  labs(x="general health")


# histogram-----------------------------
pdf(file="hist_mpg1.pdf", width=5, height=4)
par(mar=c(5, 4, 1, 2), cex.lab=1.15, cex.axis=1.15)
hist(mtcars$mpg, main='', xlab="Miles per gallon (mpg)")
dev.off()

# attributes
h1 <- hist(mtcars$mpg)
attributes(h1)
h1$breaks
h1$counts

# adjusting bin size
pdf(file=paste0(mydir, 'hist_weight_bins.pdf'), width=8, height=7)
par(mfrow=c(2,2))
par(cex.lab=1.2, cex.axis=1.2)
hist(cdc$weight, main="default # of bins")
hist(cdc$weight, breaks=10, main="10 bins")
hist(cdc$weight, breaks=50, main="50 bins")
hist(cdc$weight, breaks=100, main="100 bins")
dev.off()

#histogram density-----------------------
hist(mtcars$mpg)
hist(mtcars$mpg, freq=F)
h1 = hist(mtcars$mpg)
h1$density
n = length(mtcars$mpg)
h1$counts / (5*length(mtcars$mpg))

pdf(file=paste0(mydir, 'histd_mpg1.pdf'), width=5, height=4)
par(mar=c(5, 4, 1, 2))
hist(mtcars$mpg, freq=F, main='')
dev.off()

pdf(file=paste0(mydir, 'histd_wt.pdf'), width=5, height=4)
par(mar=c(5, 4, 1, 2))
hist(cdc$weight, freq=F, main='')
lines(density(cdc$weight), col='red', lwd=1.5)
dev.off()

# adjust banwidth
bw.nrd0(cdc$weight) #default
hist(cdc$weight, freq=F, main='')
lines(density(cdc$weight, bw=7), col='red', lwd=1.5)


# boxplot----------------------
pdf(file=paste0(mydir, 'boxplot_wt.pdf'), width=4, height=4)
par(cex.lab=1.25)
boxplot(cdc$weight, ylab = "Weight")
dev.off()

pdf(file=paste0(mydir, 'boxplot_wt_mf.pdf'), width=6, height=5)
par(cex.lab=1.25, cex.axis=1.25)
boxplot(cdc$weight~cdc$gender, xlab="Gender", ylab="Weight")
dev.off()

# maps----------------------
# load('/Users/ericfox/Documents/epa/mmigeostat_proj/mmigeostat2/data/mmi_data.rdata')
# 
# nrsa <- mmi_data[,c('LON_DD83', 'LAT_DD83', 'BENT_MMI_COND',
#                     'BENT_MMI_COND2', 'MMI_BENT')]
# names(nrsa) <- c('lon', 'lat', 'cond', 'cond2', 'mmi')
# write.csv(nrsa, file='/Users/ericfox/Documents/CSUEB/teaching/stat630data/nrsa.csv', row.names=F)

data_url <- "https://github.com/ericwfox/stat630data/raw/master/nrsa.csv"
nrsa <- read.csv(data_url, header = TRUE) # read in data set
head(nrsa, n=10)
dim(nrsa)

pdf(file=paste0(mydir, 'nrsamap0.pdf'), width=8, height=5)
map("state")
points(nrsa$lon, nrsa$lat, cex=0.5)
dev.off()

pdf(file=paste0(mydir, 'nrsamap.pdf'), width=8, height=5)
map("state")
points(nrsa$lon, nrsa$lat, cex=0.5,
       col=c("red","orange","blue")[nrsa$cond2])
legend("bottomright", c("poor", "fair", "good"), 
       col=c("red","orange","blue"), pch=1)
dev.off()
#pch 19

nrsa2 <- subset(nrsa, cond=='Good')
map('usa')
points(nrsa2$LON_DD83, nrsa2$LAT_DD83, cex=0.5, pch=19)

# other maps
pdf(file=paste0(mydir, 'world.pdf'), width=10, height=6)
map("world")
dev.off()

pdf(file=paste0(mydir, 'cali.pdf'), width=5, height=6)
map("state", "california")
dev.off()

pdf(file=paste0(mydir, 'cali2.pdf'), width=5, height=6)
map("county", "ca")
dev.off()

# using ggplot2
library(maps)
# library(ggmap)
library(ggplot2)
states <- map_data("state")
head(states)
ggplot(data=states, aes(long, lat, group=group)) + 
  geom_polygon(fill="NA", color="black")

ggplot(nrsa, aes(lon, lat, color=cond)) + 
  geom_point(alpha=0.75, shape=1) +
  scale_color_manual(values=c("orange", "blue", "red")) +
  geom_polygon(data=map_states, aes(long, lat, group=group), 
               fill="NA", color="black")

