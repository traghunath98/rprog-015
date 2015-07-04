pollutantmean <- function(directory, pollutant, id =1:332) {

	computed_mean <- NA
	
	#Traverse the directory and build an index of all the files
	df_allFiles <- data.frame(fileName=list.files(path=directory), index=as.numeric(strtrim(list.files(path=directory),3)))
	
	#Subselect the files in the range provided
	df_chosenFiles <- subset(df_allFiles, df_allFiles$index %in% id)
	
	v_chosenFiles <- as.vector(df_chosenFiles$fileName)
	df_allSelectedData <- NULL
	
	#iterate in the chosen files to build data and get mean
	if(length(v_chosenFiles > 0)){
		
		for( i in seq_along(v_chosenFiles)){
			if( i == 1){
				df_allSelectedData <- read.csv(file.path(directory,v_chosenFiles[i]),stringsAsFactors=FALSE)	
			} else {
				df_chosenData <- read.csv(file.path(directory, v_chosenFiles[i]), stringsAsFactors=FALSE)
				df_allSelectedData <- rbind(df_allSelectedData, df_chosenData)		
			}
		}
		# get the pollutant data for valid attribute
		v_attr_check <- grep(pollutant, names(df_allSelectedData), fixed=TRUE) 
		if( (length(v_attr_check) > 0) && (v_attr_check >0)){
		
			v_AllSelectedData <- as.vector(df_allSelectedData[pollutant])
		
			# remove the NA values
			v_AllSelectedData <- as.numeric(v_AllSelectedData[!is.na(v_AllSelectedData)])
			computed_mean <- mean(v_AllSelectedData)	
	
		}
				
	}
	computed_mean
}