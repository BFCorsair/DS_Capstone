repeat {
	# cat("Enter source - one of: Blog, News, Twitter","\n") # prompt
	# source <- scan(what=character(),nlines=1)
	source <- readline("Enter source - one of: Blog, News, Twitter: ") # prompt
	# ToDo: Clean the sentence
	if(nchar(source) == 0) stop("Aborted")
	if (source %in% c("Blog", "News", "Twitter")) {
		break 
	} else {
		print("Source must be one of: Blog, News, Twitter")
		print("Enter <CR> to abort")
	}

}
print(paste("Success - source is: ", source))

