## ---- message = FALSE--------------------------------------------------------------
library(openintro)


## ---- fig.width=4, fig.height=3.75-------------------------------------------------
boxplot(weight ~ habit, data = ncbirths, xlab = "", ylab = "weight (lbs)")


## ----------------------------------------------------------------------------------
table(ncbirths$habit)
nc_smoke <- subset(ncbirths, habit == "smoker")
nc_nosmoke <- subset(ncbirths, habit == "nonsmoker")
summary(nc_smoke$weight)
summary(nc_nosmoke$weight)


## ----------------------------------------------------------------------------------
t.test(nc_nosmoke$weight, nc_smoke$weight)


## ---- eval=F-----------------------------------------------------------------------
## t.test(weight ~ habit, data=ncbirths)


## ----------------------------------------------------------------------------------
age_diff <- husbands_wives$age_husband - husbands_wives$age_wife
age_diff <- age_diff[!is.na(age_diff)] # remove missing entries (NA values)
length(age_diff)
summary(age_diff)


## ---- fig.width=5, fig.height=4----------------------------------------------------
hist(age_diff, xlab="Age difference", main='')


## ----------------------------------------------------------------------------------
t.test(age_diff)

