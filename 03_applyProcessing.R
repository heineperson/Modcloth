
# Bring in libraries
suppressMessages(library(data.table))
suppressMessages(library(rvest))
suppressMessages(library(stringr))
suppressMessages(library(parallel))
source('02_processModclothReview.R')

# Bring in links
linkRef <- readRDS("page1.rds")
linkRef[between(Count, 800, Inf), Time := 150000]
linkRef[between(Count, 110, 799), Time := 30000]
linkRef[between(Count, 36, 109), Time := 10000]
linkRef[between(Count, 0, 35), Time := 5000]

# Run model on all dresses
sapply(289:1, function(x) processModclothReview(
	website=linkRef[x, paste0("http://www.modcloth.com", Link)]
 ,jsfile = "./javascript/data_pull.js"
 ,phantom.path = "phantomjs-2.1.1-macosx/bin/phantomjs"
 ,timeToRun = linkRef[x, Time]
 ,delete=TRUE))

# Rerun on select dresses
# sapply(c(27,32,46), function(x) processModclothReview(
#   website=linkRef[x, paste0("http://www.modcloth.com", Link)]
#   ,jsfile = "./javascript/data_pull.js"
#   ,phantom.path = "phantomjs-2.1.1-macosx/bin/phantomjs"
#   ,timeToRun = 150000
#   ,delete=TRUE))


