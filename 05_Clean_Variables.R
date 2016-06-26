
# Bring in libraries
library(data.table)

# Read in data
dat <- readRDS("B.Reviews_Data.rds")

# Add dress index number
dat[, dressIdx := .GRP, by=.(dressTitle)]

# Convert variables to numeric
dat[, Waist := as.numeric(Waist)]                 
dat[, Hips := as.numeric(Hips)]   

# For bra size, separate Bra numeric and Bra alpha
dat[, BraNum := as.numeric(gsub("[^0-9]", "", Bra))]
dat[, BraAlpha := gsub("[0-9]", "", Bra)]
dat[BraAlpha %in% c("A","AA"), BraAlpha := "A"]
dat[BraAlpha %in% c("--","", NA), BraAlpha := NA]
dat[BraAlpha %in% c("H","I", "J","K"), BraAlpha := "PLUS"]
dat[, Bra := NULL]

# For Height, separate Feet and Inches
dat[Height %in% c(NA,"--"), Height := NA]
dat[, (c("Feet","Inches")) := tstrsplit(Height, "'")]
dat[, Feet := as.numeric(Feet)]
dat[, Inches := as.numeric(gsub("\"", "", Inches))]
dat[, Height := NULL]
dat[, Height := Feet*12 + Inches]
dat[, (c("Feet","Inches")) := NULL]

# Extract size length
sizeLength <- rbindlist(dat[, dressMeasurements], fill=TRUE)
sizeLength[, PersonSize := dat$Size]
sizeLength[, PersonSizeIdx := sapply(PersonSize, function(x) which(names(sizeLength)==x))]
sizeLength[, PersonSizeNum := mapply(x=1:.N, y=PersonSizeIdx, function(x,y) as.numeric(unlist(sizeLength[x, y, with=FALSE])))]
sizeLength[, SizeLength := as.numeric(PersonSizeNum)]
dat[, SizeLength := sizeLength$SizeLength]


# Clean up other variables
dat[trimws(Fit)=="", Fit := NA]
dat[trimws(Length)=="", Length := NA]
dat[trimws(Quality)=="", Quality := NA]
dat[, dressReviewCount := as.numeric(gsub("[^0-9]", "", dressReviewCount))]
dat[, dressMeasurements := NULL]
dat[, UsefulCount := as.numeric(UsefulCount)]

# Reorder variables
setcolorder(dat, c(
"Name","userRating","DateTime","Waist",               
"Hips","Size","UsefulCount","Fit",                   
"Length","Quality","BraNum","BraAlpha","Height",
"SizeLength","Comments",
"dressIdx","dressTitle","dressReviewCount","dressPrice","dressOverallRating",   
"dressDescription","dressDetails","dressModelMeasurements"))
sapply(dat, class)
lapply(setdiff(names(dat),"dressModelMeasurements"), function(x) dat[, .N, by=x][order(-N)][1:10][!is.na(N)])
saveRDS(dat, "C.Cleaned_Data.rds")
