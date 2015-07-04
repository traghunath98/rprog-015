complete <- function(directory, id=1:332){
	
	df_result <- NULL
	
	#Traverse the directory and build an index of all the files
	if(file.exists(directory)){
		df_allFiles <- data.frame(fileName=list.files(path=directory), fileIndex=as.numeric(strtrim(list.files(path=directory),3)))	
		
		#Subselect the files in the range provided
		df_chosenFiles <- subset(df_allFiles, df_allFiles$fileIndex %in% id)
				
		# iterate in the chosen files to get the statistics
		v_id <- NULL
		v_nobs <- NULL
		if(length(df_chosenFiles$fileIndex >0 )){
			for (j in seq_along(df_chosenFiles$fileName)){
				df_selectedData <- read.csv(file.path(directory, df_chosenFiles$fileName[j]), stringsAsFactors=FALSE)
				df_selectedData <- subset(df_selectedData, !is.na(df_selectedData$sulfate) & !is.na(df_selectedData$nitrate))
			
				v_id <- c(v_id, df_chosenFiles$fileIndex[j])
				v_nobs <- c(v_nobs, length(row.names(df_selectedData)))							
			}
			df_result <- data.frame(id=v_id, nobs=v_nobs)
		}
	}
	df_result	
}