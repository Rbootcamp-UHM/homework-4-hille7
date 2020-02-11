#setwd("C:/Users/hille/Documents/Rbootcamp_S2020/homework-4-hille7/")

##################################################
#             Function Definitions               #
##################################################

read.spec <- function (x)  {  #Function to read in spec data
	dat <- read.table(file=x, skip=17, comment.char=">") #Removes header and footer from data
	names(dat) <- c("lambda", "intensity") #Renames columns
	dat <- dat[dat$lambda >= 300 & dat$lambda <= 700,] #Subsets the wavelengths between 300-700 
	return(dat)
}
plot.spec <- function(x) {
	dat <- x
	dat2 <- dat[order(dat$intensity, decreasing=T),] #sorts dat by decreasing order and makes new object
	maxint <- dat2[1,2] #indexes max intensity
	intlambda <- dat2[1,1] #indexes lambda associated with max intensity
	
	plot(dat, type="l") #plots data if TRUE
	points(intlambda, maxint, cex=2, pch=19, col="purple") #plots max intensity and associated lambda 
	
	out <- matrix(nrow=1, ncol=2, c(maxint, intlambda))
	return(out) #retuns max intensity and associated lambda
}
wrapper <- function(x) { #wrapper function that can generate plots and returns max intensity and associated lambda
	x <- x
	dat <- read.spec(x)
	dat2 <- plot.spec(dat)
	return(dat2)
}

##################################################
#                 Working Script                 #
##################################################

myfiles <- list.files("./Data", pattern="20070725_") #reads all files following the pattern
myfiles <- paste("Data/", myfiles, sep="") #adds "Data/" to file names
myfiles

wrapper(x=myfiles[1]) #applies wrapper function index 1 of "myfiles"

bigdata <- matrix(nrow=length(myfiles), ncol=2) #creates matrix of appropriate dimensions 
pdf(file="SpecFunctionPlots_EH.pdf") #saves plots to PDF using my wrapper function
for(i in 1:length(myfiles)) { #for loop that repeats the wrapper function for each file
	dati <- wrapper(x=myfiles[i])
	bigdata[i,] <- dati	#fills "bigdata" with max intensity and lambda from each file 
}
dev.off()

output <- data.frame(myfiles, bigdata[,2], bigdata[,1]) #creates dataframe
names(output) <- c("file", "lambda", "max intensity") #renames columns of dataframe
write.csv(x=output, file="SpecFunctionOutput_EH.csv") #saves dataframe to .csv
