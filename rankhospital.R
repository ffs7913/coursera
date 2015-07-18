# Part of Coursera Course 2, R Programming

rankhospital <- function(state, outcome, num="best") {
	require(dplyr)
	
	outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	possibleoutcome <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
	possiblestates <- outdata[,7]
	if (!is.element(state, possiblestates)) stop ("invalid state")
	if (!is.element(outcome, names(possibleoutcome))) stop ("invalid outcome")

	outcome <- possibleoutcome[[outcome]]
	outdata<- filter(outdata, outdata$State==state)

	decreas <- FALSE
	if (num=="worst") {
		decreas <- TRUE
		num <-1
	}
	if (num=="best") num <- 1
	if (num>length(outdata[,2])) return(NA)
	
	suppressWarnings(outdata[,outcome] <- as.numeric(outdata[,outcome]))
	ind <- order(outdata[,outcome],outdata[,2], na.last = NA, decreasing=decreas)[[num]]

	outdata[[ind,2]]
}
