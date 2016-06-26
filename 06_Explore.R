# Bring in libraries
library(data.table)

# Read in data
dat <- readRDS("C.Cleaned_Data.rds")

dress1 <- dat[dressIdx != 1000, .(Name,Fit,Length,Quality,Waist, Hips, Size, SizeLength, BraNum, BraAlpha, Height)]
dress1 <- na.omit(dress1)
dress1[, WaistHipRatio := Waist/Hips]

plot(ecdf(dress1$Waist))
plot(ecdf(dress1$Hips))
plot(ecdf(dress1$BraNum))

check <- dress1[, .N, by=.(Fit, Hips=cut(Hips, c(25,30,35,40,45,65)))][order(Hips, Fit)]
check[, Per := N/sum(N), by=.(Hips)]

dress1[, FitsJustRight := 1*(Fit=="JustRight")]
mod <- glm(FitsJustRight~Waist+Hips+BraNum+SizeLength+Height+WaistHipRatio, data=dress1, family=binomial(link="logit"))
summary(mod)

dress1[, c(.N, lapply(.SD, mean)), by=.(Size), .SDcols=c("Waist","Hips","BraNum","Height","WaistHipRatio")][order(Size)]
dress1[, c(.N, lapply(.SD, mean)), by=.(Fit,Size), .SDcols=c("Waist","Hips","BraNum","Height","WaistHipRatio")][order(Fit,Size)]
dress1[, c(.N, lapply(.SD, mean)), by=.(Length,Size), .SDcols=c("Waist","Hips","BraNum","Height","WaistHipRatio")][order(Length,Size)]
dress1[, c(.N, lapply(.SD, mean)), by=.(Quality,Size), .SDcols=c("Waist","Hips","BraNum","Height","WaistHipRatio")][order(Quality,Size)]

# x <- model.matrix(~.-1-Name-Size-Fit, data=dress1)
# library(glmnet)
# cvfit=cv.glmnet(x, dress1[, Fit], family="multinomial", type.multinomial = "grouped", parallel = TRUE)
# plot(cvfit)
