## -------------------------------------------------------------------------------------
cdc <- readRDS(url("https://ericwfox.github.io/data/cdc.rds"))


## -------------------------------------------------------------------------------------
names(cdc)
dim(cdc)


## ------------------------------------------------------------------------------------
head(cdc)


## -------------------------------------------------------------------------------------
summary(cdc$weight)


## -------------------------------------------------------------------------------------
mean(cdc$weight) 
median(cdc$weight)
sd(cdc$weight)


## -------------------------------------------------------------------------------------
table(cdc$smoke100)


## -------------------------------------------------------------------------------------
table(cdc$smoke100)/20000


## --------------------------------------------------------------------------------------
barplot(table(cdc$smoke100))


## -------------------------------------------------------------------------------------
smoke_tb <- table(cdc$smoke100)
barplot(smoke)


## -------------------------------------------------------------------------------------
table(cdc$gender,cdc$smoke100)


## -------------------------------------------------------------------------------------
addmargins(table(cdc$gender, cdc$smoke100))


## -------------------------------------------------------------------------------------
cdc10 <- cdc[1:10,]
cdc10


## -------------------------------------------------------------------------------------
cdc10$gender
cdc10$gender == "m"


## -------------------------------------------------------------------------------------
subset(cdc10, gender == "m")


## -------------------------------------------------------------------------------------
cdc10$age
cdc10$age > 40
subset(cdc10, age > 40)


## -------------------------------------------------------------------------------------
subset(cdc10, gender == "m" & age > 40)


## -------------------------------------------------------------------------------------
cdc_m <- subset(cdc, gender == "m")
cdc_f <- subset(cdc, gender == "f")

summary(cdc_m$weight)
summary(cdc_m$wtdesire)

summary(cdc_f$weight)
summary(cdc_f$wtdesire)

