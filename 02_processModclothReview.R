#' Process Modcloth Reviews
#'
#' Feed in the website url, the path the js file, & the path of the phantom js application and pull data
#' @param website This is the website string
#' @param jsfile This is the path of the js file to scrape the data (eg "~/Modcloth/data_pull.js")
#' @param phantom.path This is the path of the phantomjs app (eg "~/Modcloth/phantomjs-2.1.1-macosx/bin/phantomjs")
#' @param delete Do you want to delete the downloaded html file after using it?
#' @keywords modcloth scrape phantom
#' @export
#' @examples
#' Scrape dress data
#' processModclothReview(
#'   website = "http://www.modcloth.com/shop/dresses/indigo-gardens-dress"
#'  ,jsfile = "~/Dropbox/Modcloth/data_pull.js"
#'  ,phantom.path = "~/Dropbox/Modcloth/phantomjs-2.1.1-macosx/bin/phantomjs"
#'  ,delete=TRUE)
#'  
#' Resources
#  https://www.datacamp.com/community/tutorials/scraping-javascript-generated-data-with-r
#  http://phantomjs.org/page-automation.html

processModclothReview <- function(website, jsfile, phantom.path, timeToRun, delete=TRUE) {
  suppressMessages(library(data.table))
  suppressMessages(library(rvest))
  suppressMessages(library(stringr))
  
  # Print website
  print(website)
  
  # Use phantomjs to pull data from Modcloth
  output <- paste0(basename(website), ".html")
  system(paste0(phantom.path, " ", jsfile, " ", website, " ", output, " ", timeToRun))
  
  # Pull data using rvest function and place in data.table
  htmlOutput <- read_html(output)
  
  # Grab dress name
  dressTitle <- htmlOutput %>% html_nodes("#pdp-product-name") %>% html_text()
  
  # if dress name is null, then move on
  if (length(dressTitle)==0) {
    print(paste0(website, " not returned"))
    return(NULL)
  }
  
  # Grab dress price (sometimes multiple pries for sales. In this case,
  # pick minimum price available)
  dressPrice <- htmlOutput %>% html_nodes("#price-reviews span") %>% html_text()
  if (length(dressPrice) > 1) {
    dressPriceTmp <- gsub("\\$","", dressPrice[grep("\\$", dressPrice)])
    dressPriceTmp <- sapply(dressPriceTmp, trimws)
    dressPriceTmp <- as.numeric(dressPriceTmp)
    dressPrice <- min(dressPriceTmp, na.rm=TRUE)
  }
  dressPrice <- as.numeric(gsub("\\$", "", dressPrice))
  
  dressOverallRating <- htmlOutput %>% html_nodes("#details-and-measures p") %>% html_text()
  dressOverallRating <-  htmlOutput %>% html_nodes(".review-stars") %>% html_children()
  dressOverallRating <- as.numeric(gsub("[^0-9]", "", dressOverallRating))
  reviewOverallRating <- dressOverallRating[-1]
  dressOverallRating <- dressOverallRating[1]
  dressMeasurements <- htmlOutput %>% html_nodes(".measurements-data") %>% html_table()
  dressModelMeasurements <- htmlOutput %>% html_nodes(".pdp-measurements-data") %>% html_table()
  dressDescription <- htmlOutput %>% html_nodes("#details-and-measures p") %>% html_text()
  dressDescription <- dressDescription[1]
  dressDetails <- htmlOutput %>% html_nodes("#details-and-measures ul") %>% html_text()
  dressReviewCount <- htmlOutput %>% html_nodes(".reviews-count") %>% html_text()
  dressReviewCount <- gsub("\n", "", dressReviewCount)
  
  # Deal with review data
	reviewName <- htmlOutput %>% html_nodes(".user-review .review_info_name") %>% html_text()
	reviewName <- gsub("[pa]m\n", "", reviewName)
	reviewName <- gsub("\n", "", reviewName)
	reviewName <- str_extract(reviewName, "[A-Za-z]+")
	
	if (length(reviewName) == 0) {
	  outTable <- data.table(
	    dressTitle
	    ,dressReviewCount
	    ,dressPrice
	    ,dressOverallRating
	    ,dressDescription
	    ,dressDetails
	    ,Name = NA
	    ,userRating = NA
	    ,DateTime = NA
	    ,Height = NA
	    ,Bra = NA
	    ,Waist = NA
	    ,Hips = NA
	    ,Size = NA
	    ,UsefulCount = NA
	    ,Fit = NA
	    ,Length = NA
	    ,Quality = NA
	    ,Comments = NA)
	  outTable[, dressMeasurements := list(dressMeasurements)]
	  outTable[, dressModelMeasurements := list(dressModelMeasurements)]
	} else {
	  reviewDatTime <- htmlOutput %>% html_nodes(".user-review .review-left-date") %>% html_text()
	  reviewDatTime <- gsub("\n", "", reviewDatTime)
	  reviewDatTime <- gsub(" ", "", reviewDatTime)
	  reviewsSize <-  htmlOutput %>% html_nodes(".user-review .item_size") %>% html_text()
	  reviewsSize <-  gsub("\n", "", reviewsSize)
	  reviewsSize <-  gsub(" ", "", reviewsSize)
	  reviewsSizeIndx <-  htmlOutput %>% html_nodes(".review_right") %>% html_text()
	  reviewsSizeIndx <- grep("Item Size:", reviewsSizeIndx)
	  reviewsSizeIndx <- reviewsSizeIndx[order(reviewsSizeIndx)]
	  reviewsSizeIndx <- data.table(ID=reviewsSizeIndx, Size=reviewsSize)
	  reviewsSizeIndxFinal <- data.table(ID=1:length(reviewName))
	  reviewsSizeIndxFinal <- merge(reviewsSizeIndxFinal, reviewsSizeIndx, by="ID", all.x=TRUE)
	  reviewsSizeIndxFinal[, Size := gsub("ItemSize:", "", Size)]
	  reviewComment <- htmlOutput %>% html_nodes(".user-review .review_comment") %>% html_text()
	  reviewComment <- trimws(gsub("\n", "", reviewComment))
	  reviewRatings <- htmlOutput %>% html_nodes(".user-review .ratings") %>% html_text()
	  reviewRatings <- gsub("\n", "", reviewRatings)
	  reviewRatings <- gsub(" ", "", reviewRatings)
	  reviewRatings <- gsub("Fit", "", reviewRatings)
	  reviewRatings <- gsub("Length", "-", reviewRatings)
	  reviewRatings <- gsub("Quality", "-", reviewRatings)
	  reviewRatings <- as.data.table(tstrsplit(reviewRatings, "-"))
	  setnames(reviewRatings, c("Fit","Length","Quality"))
	  reviewUseful <- htmlOutput %>% html_nodes(".user-review .was_this_helpful") %>% html_text()  
	  reviewUseful <- gsub("[^0-9]", "", reviewUseful)
	  reviewAttribsIndx <- htmlOutput %>% html_nodes(".user-review .review_left") %>% html_text()
	  reviewAttribsIndx1 <- grep("Height", reviewAttribsIndx)
	  reviewAttribsIndx2 <- grep("Bra", reviewAttribsIndx)
	  reviewAttribsIndx3 <- grep("Waist", reviewAttribsIndx)
	  reviewAttribsIndx4 <- grep("Hips", reviewAttribsIndx)
	  reviewAttribsIndx <- c(reviewAttribsIndx1,reviewAttribsIndx2,reviewAttribsIndx3,reviewAttribsIndx4)
	  reviewAttribsIndx <- unique(reviewAttribsIndx)
	  reviewAttribsIndx <- reviewAttribsIndx[order(reviewAttribsIndx)]
	  reviewAttribs <- htmlOutput %>% html_nodes(".measures_wrapper") %>% html_text()
	  reviewAttribs <- gsub("\n","", reviewAttribs)
	  reviewAttribs <- gsub(" ","", reviewAttribs)
	  reviewAttribs <- gsub("Bra", "@@@Bra", reviewAttribs)
	  reviewAttribs <- gsub("Waist", "@@@Waist", reviewAttribs)
	  reviewAttribs <- gsub("Hips", "@@@Hips", reviewAttribs)
	  reviewAttribs <- as.data.table(tstrsplit(reviewAttribs, "@@@"))
	  reviewAttribs[grepl("Height", V1), Height := V1]
	  reviewAttribs[grepl("Height", V2), Height := V2]
	  reviewAttribs[grepl("Height", V3), Height := V3]
	  reviewAttribs[grepl("Height", V4), Height := V4]
	  reviewAttribs[grepl("Bra", V1), Bra := V1]
	  reviewAttribs[grepl("Bra", V2), Bra := V2]
	  reviewAttribs[grepl("Bra", V3), Bra := V3]
	  reviewAttribs[grepl("Bra", V4), Bra := V4]
	  reviewAttribs[grepl("Waist", V1), Waist := V1]
	  reviewAttribs[grepl("Waist", V2), Waist := V2]
	  reviewAttribs[grepl("Waist", V3), Waist := V3]
	  reviewAttribs[grepl("Waist", V4), Waist := V4]
	  reviewAttribs[grepl("Hips", V1), Hips := V1]
	  reviewAttribs[grepl("Hips", V2), Hips := V2]
	  reviewAttribs[grepl("Hips", V3), Hips := V3]
	  reviewAttribs[grepl("Hips", V4), Hips := V4]
	  reviewAttribs[, Height := gsub("Height","",Height)]
	  reviewAttribs[, Bra := gsub("Bra","",Bra)]
	  reviewAttribs[, Waist := gsub("Waist","",Waist)]
	  reviewAttribs[, Hips := gsub("Hips","",Hips)]
	  reviewAttribs[, (paste0("V",1:4)) := NULL]
	  reviewAttribs[, ID := reviewAttribsIndx]
	  reviewAttribsFinal <- data.table(ID=1:length(reviewName))
	  reviewAttribsFinal <- merge(reviewAttribsFinal, reviewAttribs, by=c("ID"), all.x=TRUE)
	  
	  # Make table
	  outTable <- data.table(
	    dressTitle
	    ,dressReviewCount
	    ,dressPrice
	    ,dressOverallRating
	    ,dressDescription
	    ,dressDetails
	    ,Name = reviewName
	    ,userRating = reviewOverallRating
	    ,DateTime = reviewDatTime
	    ,Height = reviewAttribsFinal[, Height]
	    ,Bra = reviewAttribsFinal[, Bra]
	    ,Waist = reviewAttribsFinal[, Waist]
	    ,Hips = reviewAttribsFinal[, Hips]
	    ,Size = reviewsSizeIndxFinal[, Size]
	    ,UsefulCount = reviewUseful
	    ,Fit = reviewRatings[, Fit]
	    ,Length = reviewRatings[, Length]
	    ,Quality = reviewRatings[, Quality]
	    ,Comments = reviewComment)
	  outTable[, dressMeasurements := list(dressMeasurements)]
	  outTable[, dressModelMeasurements := list(dressModelMeasurements)]	  
	}

	# Reorder column names
	setcolorder(outTable, c(
		"dressTitle","dressReviewCount","dressPrice","dressOverallRating","dressMeasurements",
		"dressModelMeasurements","dressDescription","dressDetails","Name","userRating","DateTime",
		"Height","Bra","Waist","Hips","Size","UsefulCount","Fit","Length","Quality","Comments"))
	
	# If dressOutput does not already exist, make the directory
	if (!dir.exists("dressOutput")) {
		dir.create("dressOutput")
	}
	
	# Save data
	file_nm <- gsub(" ", "", str_replace_all(dressTitle, "[^[:alnum:]]", " "))
	file_nm <- paste0("dressOutput/", file_nm, ".rds")
	saveRDS(outTable, file_nm)
	
	# Delete temporary modcloth.html file
	if (delete == TRUE) {
		system(paste0("rm ", output))
	}
	
  return(file_nm)
}




