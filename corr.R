# Part of Coursera Course 2, R Programming

corr <- function(directory, threshold = 0) {
	id <- 1:332
	
     files <- formatC(id, width = 3, format = "d", flag = "0")
	  files <- paste0(directory,'/',files,'.csv')
	  data = lapply(files,read.csv)
	  
	  foo <- which(sapply(data, function(x) sum(complete.cases(x))>threshold))
	  
	  sapply(foo, function(n) cor(data[[n]]$nitrate, data[[n]]$sulfate, use="complete.obs"))
}
