cdc <- read.csv("cdc.csv")
saveRDS(cdc, file="cdc.rds")

cdc <- readRDS(url("https://ericwfox.github.io/data/cdc.rds"))

# cdc <- readRDS("cdc.rds")
