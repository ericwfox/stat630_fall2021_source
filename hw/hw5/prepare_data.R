employee2017 <- read.csv("Employee_Compensation2017.csv")
saveRDS(employee2017, file="employee2017.rds")


employee <- readRDS(url("https://ericwfox.github.io/data/employee2017.rds"))

Salaries <- employee$Salaries / 1000
saveRDS(Salaries, file = "Salaries.rds")

Salaries <- readRDS("Salaries.rds")







