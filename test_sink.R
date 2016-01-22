#! /Library/Frameworks/R.framework/Resources/Rscript
source("BF_util.R")  # my personal utilities 
library("getopt")

# sink(file = "tt.log", append = FALSE, type = c("output", "message"), split = FALSE)
spec = matrix(c(
'verbose', 'v', 2, "integer",
'help' , 'h', 0, "logical",
'source' , 's', "Blog", "character"
), byrow=TRUE, ncol=4);

if(is.na(get_Rscript_filename()) {  # Interactive
	source <- getSource()
	progName <- "test_sink"
	logFileName <- create_logFileName(source, prog=progName) 

} else { # command line
	opt = getopt(spec, command = get_Rscript_filename());
	logFileName <- create_logFileName(opt$source)
}
# if help was asked for print a friendly message
# and exit with a non-zero error code
if ( !is.null(opt$help) ) {
	cat(getopt(spec, usage=TRUE));
	q(status=1);
}
 
sink(logFileName)
consoleOut("Starting at: ", Sys.time())

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

arguments <- commandArgs()
consoleOut(arguments)
sink() # returns the output to console

# -------- Output --------
# rscript test_sink.R --hello -h

# [1] "Starting at:  2016-01-20 16:44:33"
# [1] "/Library/Frameworks/R.framework/Resources/bin/exec/R"
# [2] "--slave"
# [3] "--no-restore"
# [4] "--file=test_sink.R"
# [5] "--args"
# [6] "--hello"
# [7] "-h"