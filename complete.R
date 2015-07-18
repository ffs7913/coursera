# Part of Coursera Course 2, R Programming

complete <- function(directory, id = 1:332) {
	files <- formatC(id, width = 3, format = "d", flag = "0")
	files <- paste0(directory,'/',files,'.csv')
	data = lapply(files,read.csv)
	
	sna <- function(x) {sum(complete.cases(x))}
	
	data.frame(id, nobs=sapply(data, sna))
}
