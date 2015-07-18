# Part of Coursera Course 2, R Programming

rankall <- function( outcome, num="best") {	
	outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	possibleoutcome <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
	if (!is.element(outcome, names(possibleoutcome))) stop ("invalid outcome")
	outcome <- possibleoutcome[[outcome]]
	suppressWarnings(outdata[,outcome] <- as.numeric(outdata[,outcome]))

	decreas <- FALSE
	if (num=="worst") {
		decreas <- TRUE
		num <-1
	}
	if (num=="best") num <- 1

	findMin <- function(outdata, outcome, num) {
		#print(head(outdata[,c(2,7,11)]))
		hospitalnames <-  outdata[,2]
		outcomes <- outdata[,outcome]
		if (num>sum(!is.na(outcomes))) return(NA)
		else {
			ind <- order(outcomes, hospitalnames, na.last = NA, decreasing=decreas)[[num]]
			return (hospitalnames[[ind]])
		}
	}
	
	foo <- by(outdata, outdata[,7], findMin, outcome, num, simplify = TRUE)
	as.data.frame(cbind(hospital=foo, state=names(foo)))
}
