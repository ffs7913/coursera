# Part of Coursera Course 2, R Programming

pollutantmean <- function(directory, pollutant, id = 1:332) {
	files <- formatC(id, width = 3, format = "d", flag = "0")
	files <- paste0(directory,'/',files,'.csv')
	data = do.call(rbind,lapply(files, read.csv))
	
	mean (data[[pollutant]], na.rm=TRUE)
}
