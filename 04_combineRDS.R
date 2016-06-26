
# Bring in libraries
library(data.table)

# Read in all files
rdsFiles <- list.files("./dressOutput", ".rds", full.names = TRUE)
dat <- rbindlist(lapply(rdsFiles, readRDS), fill=TRUE)

# check
check1 <- dat[, .(.N, dressReviewCount=dressReviewCount[1]), by=.(dressTitle, dressPrice)]
check1[N != as.numeric(gsub("[^0-9]", "", dressReviewCount))]
dat[dressTitle=="On Top of the Whirl Dress"]

# Save resulting data
saveRDS(dat, "B.Reviews_Data.rds")
