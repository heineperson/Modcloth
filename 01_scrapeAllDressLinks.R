
# Find links of all dresses
scrapeDressLinks <- function(website, jsfile, phantom.path) {
	
	# Bring in libraries
	suppressMessages(library(data.table))
	suppressMessages(library(rvest))
	suppressMessages(library(stringr))
	suppressMessages(library(parallel))

	# Read in download webpage
	comm <- paste0(phantom.path, " ", jsfile, " ", website)
	system(comm)
	Sys.sleep(25)
	
	# Read and process webpage
	pageFile <- read_html("modclothDressList.html")
	pageFile <- pageFile %>% html_nodes(".product_list a") %>% html_attr("href")
	pageFile <- unique(pageFile)
	pageFileDat <- data.table(Link = pageFile)
	pageFileDat <- pageFileDat[!(Link %in% c(NA, "#"))]
	print(nrow(pageFileDat))
	
	# Function to grab number of reviews for each website
	grabReviewCount <- function(x) {
		print(x)
		reviewCount <- read_html(paste0("http://www.modcloth.com", x)) %>% html_nodes(".reviews-count") %>% html_text()
		reviewCount <- as.numeric(gsub("[^0-9]", "", reviewCount))
		return(reviewCount)
	}
	
	# Apply function
	pageFileDat[, Count := sapply(Link, grabReviewCount)]
	
	# Write file
	saveRDS(pageFileDat, paste0("A.page", gsub("[^0-9]", "", website), ".rds"))
	system("rm modclothDressList.html")
	
	return(website)
}

#Scrape dress links for mulitple pages
keylinks <- paste0("http://www.modcloth.com/shop/dresses#?sort=rating&page=", 1:6)
masterTable <- lapply(keylinks, function(x) scrapeDressLinks(
	website = x
 ,jsfile = "build_website_list.js"
 ,phantom.path = "phantomjs-2.1.1-macosx/bin/phantomjs"))

