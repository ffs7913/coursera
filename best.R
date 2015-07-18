# Part of Coursera Course 2, R Programming


best <- function(state, outcome) {
	require(dplyr)
	outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	possibleoutcome <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
	possiblestates <- outdata[,7]
	if (!is.element(state, possiblestates)) stop ("invalid state")
	if (!is.element(outcome, names(possibleoutcome))) stop ("invalid outcome")

	outcome <- possibleoutcome[[outcome]]
	outdata<- filter(outdata, outdata$State==state)

	num <- 1
	
	outdata[,outcome] <- suppressWarnings(as.numeric(outdata[,outcome]))
	ind<-order(outdata[,outcome],outdata[,2], na.last = NA)[[num]]

	outdata[ind,2]
}
