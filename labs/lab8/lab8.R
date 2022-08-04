## ----message=F, warning=F-----------------------------------------------------------------
library(resampledata)
data("GSS2002")


## -----------------------------------------------------------------------------------------
index <- which(is.na(GSS2002$Gender) | is.na(GSS2002$Postlife))
gender <- GSS2002$Gender[-index]
postlife <- GSS2002$Postlife[-index]


## -----------------------------------------------------------------------------------------
addmargins(table(gender, postlife))
prop.table(table(gender, postlife), margin=1)


## -----------------------------------------------------------------------------------------
n <- length(postlife)
n_f <- sum(gender == "Female")
n_m <-  sum(gender == "Male")
# sample proportion of females that believe in afterlife:
phat_f <- sum(gender == "Female" & postlife == "Yes") / n_f; phat_f
# sample proportion of males that believe in afterlife:
phat_m <- sum(gender == "Male" & postlife == "Yes") / n_m; phat_m
# pooled sample proportion
phat <- sum(postlife == "Yes") / n; phat


## -----------------------------------------------------------------------------------------
# compute test statistic
SE <- sqrt(phat*(1-phat)*(1/n_f + 1/n_m))
z <- (phat_f - phat_m) / SE; z

# p-value
pvalue = 2 * (1 - pnorm(z)); pvalue


## -----------------------------------------------------------------------------------------
SE <- sqrt(phat_f*(1-phat_f)/n_f + phat_m*(1-phat_m)/n_m)
ci_l <- phat_f - phat_m - 1.96 * SE
ci_u <- phat_f - phat_m + 1.96 * SE
round(c(ci_l, ci_u), 3)


## -----------------------------------------------------------------------------------------
education <- GSS2002$Education
death_penalty <- GSS2002$DeathPenalty
addmargins(table(education, death_penalty))
prop.table(table(education, death_penalty), margin=1)


## -----------------------------------------------------------------------------------------
chisq1 <- chisq.test(education, death_penalty)
chisq1


## -----------------------------------------------------------------------------------------
attributes(chisq1)
# test statistic
chisq1$statistic
# p-value
chisq1$p.value
# table of expected counts
chisq1$expected



