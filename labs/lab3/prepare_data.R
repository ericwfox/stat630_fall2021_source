nrsa1 <- read.csv("nrsa1.csv")

str(nrsa1)
nrsa2 <- nrsa1[, 1:3]
saveRDS(nrsa2, file="nrsa.rds")

nrsa <- readRDS("nrsa.rds")
